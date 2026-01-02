structure Exp :> EXPRESSION = struct
    datatype exp = k of int | var of string 
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp

    fun expn _ 0 = k 1
        | expn exp 1 = exp
        | expn exp n = times(exp, expn exp (n-1))

    fun subst (k n) _ _ = k n 
        | subst (var s) name new = if String.compare (s, name) = EQUAL 
                                        then new else var s
        | subst (plus (exp1, exp2)) name new = plus (subst exp1 name new, subst exp2 name new)
        | subst (times (exp1, exp2)) name new = times (subst exp1 name new, subst exp2 name new)
        | subst (neg exp) name new = neg (subst exp name new)
        | subst (inv exp) name new = inv (subst exp name new)

    fun toString _ = "welp" (* da implementare *)

    fun sEq (k n1) (k n2) = n1 = n2
        | sEq (var str1) (var str2) = String.compare (str1, str2) = EQUAL
        | sEq (plus (exp1, exp2)) (plus (exp3, exp4)) = sEq exp1 exp3 andalso sEq exp2 exp4
        | sEq (times (exp1, exp2)) (times (exp3, exp4)) = sEq exp1 exp3 andalso sEq exp2 exp4
        | sEq (neg exp1) (neg exp2) = sEq exp1 exp2
        | sEq (inv exp1) (inv exp2) = sEq exp1 exp2
        | sEq _ _ = false

    fun parse _ = k 0 (* da implementare *)
end

structure Ass :> ASSERTION = struct
    structure EXP = Exp

    datatype ass = t | f | imply of ass * ass
        | less of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp

    fun not ass = imply (ass, f)

    fun isNot (imply (ass, f) : ass) : ass option = SOME ass 
        | isNot _ = NONE

    fun orr ass1 ass2 = imply (not ass1, ass2)

    fun isOrr (imply (ass1, ass2) : ass) : (ass * ass) option = ( case isNot ass1 of 
                                                                    SOME assn => SOME (assn, ass2)
                                                                    | NONE => NONE )
        | isOrr _ = NONE

    fun andd ass1 ass2 = not (imply (ass1, not ass2))

    fun isAndd (imply (ass1, ass2) : ass) : (ass * ass) option = ( case isNot ass2 of 
                                                                    SOME assn => SOME (ass1, assn)
                                                                    | NONE => NONE )
        | isAndd _ = NONE
    
    fun more exp1 exp2 = not (orr (less (exp1, exp1)) (eq (exp1, exp2)))

    fun isMore (ass : ass) : (EXP.exp * EXP.exp) option = 
        ( case isNot ass of 
            SOME assn => ( case isOrr assn of
                            SOME ((less (exp1, exp2)), 
                                    (eq (exp3, exp4))) => if EXP.sEq exp1 exp3 andalso EXP.sEq exp2 exp4
                                                        then SOME (exp1, exp2) else NONE
                            | _ => NONE )
            | NONE => NONE )

    fun subst t _ _ = t
        | subst f _ _ = f
        | subst (imply (ass1, ass2)) name new = imply (subst ass1 name new, subst ass2 name new)
        | subst (less (exp1, exp2)) name new = less (EXP.subst exp1 name new, EXP.subst exp2 name new)
        | subst (eq (exp1, exp2)) name new = eq (EXP.subst exp1 name new, EXP.subst exp2 name new)

    fun toString ass = 
        ( case isNot ass of 
            SOME assn => "¬( " ^ toString assn ^ " )"
            | NONE => 
                ( case isOrr ass of 
                    SOME (ass1, ass2) => "( " ^ toString ass1 ^ " ) ∨ ( " ^ toString ass2 ^ " )" 
                    | NONE => 
                        ( case isAndd ass of
                            SOME (ass1, ass2) => "( " ^ toString ass1 ^ " ) ∧ ( " ^ toString ass2 ^ " )"
                            | NONE => 
                                ( case isMore ass of
                                    SOME (exp1, exp2) => EXP.toString exp1 ^ " > " ^ EXP.toString exp2
                                    | NONE => 
                                        ( case ass of 
                                            t => "TRUE"
                                            | f => "FALSE"
                                            | imply (ass1, ass2) => "( " ^ toString ass1 ^ " ) ⊃ ( " ^ toString ass2 ^ " )"
                                            | less (exp1, exp2) => EXP.toString exp1 ^ " < " ^ EXP.toString exp2
                                            | eq (exp1, exp2) => EXP.toString exp1 ^ " = " ^ EXP.toString exp2
                                            )))))



    fun parse _ = t (* da implementare *)

    fun sEq t t = true
        | sEq f f = true
        | sEq (imply (ass1, ass2)) (imply (ass3, ass4)) = sEq ass1 ass3 andalso sEq ass2 ass4 
        | sEq (less (exp1, exp2)) (less (exp3, exp4)) = EXP.sEq exp1 exp3 andalso EXP.sEq exp2 exp4
        | sEq (eq (exp1, exp2)) (eq (exp3, exp4)) = EXP.sEq exp1 exp3 andalso EXP.sEq exp2 exp4
        | sEq _ _ = false
end

structure Imp :> IMPERATIVE = struct
    structure ASS = Ass

    datatype program = skip | cons of program * program 
        | if_then_else of ASS.EXP.exp * program * program | while_do of ASS.ass * program 
        | assign of string * ASS.EXP.exp
    
    fun toString _ = "welp" (* da implementare *)

    fun parse _ =  skip (* da implementare *)
end

structure Hoare :> LOGIC = struct
    structure IMP = Imp

    datatype logic_rule = TRUTH | FALSEHOOD | STRENGTHENING | WEAKENING | AND | OR 
    datatype program_rule = IF | WHILE | ASSIGN | SKIP | COMPOSE
    datatype rule = logic of logic_rule | prog of program_rule

    exception DerivationError of string

     fun print_goal (progg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass, idx : int) : unit = 
        TextIO.print ((Int.toString idx) ^ " )\t{" ^ (IMP.ASS.toString pre) ^ "} " 
                        ^ (IMP.toString progg) ^ " {" ^ (IMP.ASS.toString post) ^ "}\n")

    fun subgoals (nil : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) (_ : int) : unit = ()
        | subgoals ((prg, pre, post) :: l) idx = (print_goal (prg, pre, post, idx); subgoals l (idx + 1))

    fun next_goal (der_list : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) : int = 
        ( subgoals der_list 1;
        TextIO.print "Select next goal: ";
        valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
        handle Option => let val _ : unit = TextIO.print "Input error, try again\n"
                            in next_goal der_list end )

    fun derive IMP.skip pre post = 
        if IMP.ASS.sEq pre post 
            then () 
            else let val (_ : IMP.program, new : IMP.ASS.ass) = step (IMP.skip, post)
                                            in derive IMP.skip pre new end
        | derive program pre post = let val (prog2 : IMP.program, new : IMP.ASS.ass) = step (program, post)
                                            in derive prog2 pre new end

    and step (IMP.skip, start_assertion) = (IMP.skip, start_assertion) (* da implementare *)
        | step (IMP.cons (prog1, prog2), start_assertion) = (IMP.skip, start_assertion)
        | step (IMP.if_then_else (exp1, prog1, prog2), start_assertion) = (IMP.skip, start_assertion)
        | step (IMP.while_do (exp, sub), start_assertion) = (IMP.skip, start_assertion)
        | step (IMP.assign (name, exp), start_assertion) = (IMP.skip, start_assertion)

    (* fun ask () : rule * IMP.ASS.ass option = (logic TRUTH, NONE) da implementare *)
    (* fun print () : unit = () *)
    and parallel der_list = ()
end