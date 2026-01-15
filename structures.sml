structure Exp :> EXPRESSION = struct
    datatype exp = k of int | var of string
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp

    fun expn _ 0 = k 1
        | expn exp 1 = exp
        | expn exp n = times(exp, expn exp (n-1))

    fun subst (k n) _ _ = k n
        | subst (var s) name new = if s = name then new else var s
        | subst (plus (exp1, exp2)) name new = plus (subst exp1 name new, subst exp2 name new)
        | subst (times (exp1, exp2)) name new = times (subst exp1 name new, subst exp2 name new)
        | subst (neg exp) name new = neg (subst exp name new)
        | subst (inv exp) name new = inv (subst exp name new)

  fun normalize (neg (k n)) = k (0 - n)
        | normalize (neg (plus (exp1, exp2))) = plus (neg exp1, neg exp2)
        | normalize (neg (times (exp1, exp2))) = times (neg exp1, exp2)
        | normalize (neg (neg exp)) = exp
        | normalize (neg (inv exp)) = inv (neg exp)
        | normalize (inv (times (exp1, exp2))) = times (exp1, inv exp2)
        | normalize (inv (inv exp)) = exp
        | normalize exp = exp

    fun enclose (e : exp) : string = let val str : string = toString_sub e
                                            in case e of
                                                plus (_, _) => "(" ^ str ^ ")"
                                                | _ => str end
    and toString_sub (k n : exp): string = let val str : string = Int.toString n
                                            in if n >= 0 then str else "(" ^ str ^ ")" end
        | toString_sub (var x) = x
        | toString_sub (plus (exp1, exp2)) = toString_sub exp1 ^ " + " ^ toString_sub exp2
        | toString_sub (times (exp1, exp2)) = enclose exp1 ^ " * " ^ enclose exp2
        | toString_sub (neg exp) = "- (" ^ toString_sub exp ^ ")"
        | toString_sub (inv exp) = "1 / (" ^ toString_sub exp ^ ")"

    fun toString exp = toString_sub (normalize exp)

    fun parse _ = NONE (* da implementare *)
end

structure Ass :> ASSERTION = struct
    structure EXP = Exp

    datatype ass = t | f | imply of ass * ass
        | less of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp

    fun not ass = imply (ass, f)

    fun isNot (imply (ass, f) : ass) : ass option = SOME ass
        | isNot _ = NONE

    fun orr ass1 ass2 = imply (not ass1, ass2)

    fun isOrr (imply (ass1, ass2)) = ( case isNot ass1 of
                                                                    SOME assn => SOME (assn, ass2)
                                                                    | NONE => NONE )
        | isOrr _ = NONE

    fun andd ass1 ass2 = not (imply (ass1, not ass2))

    fun isAndd (imply (ass1, ass2)) = ( case isNot ass2 of
                                                                    SOME assn => SOME (ass1, assn)
                                                                    | NONE => NONE )
        | isAndd _ = NONE

    fun more exp1 exp2 = not (orr (less (exp1, exp2)) (eq (exp1, exp2)))

    fun isMore (ass : ass) : (EXP.exp * EXP.exp) option =
        ( case isNot ass of
            SOME assn => ( case isOrr assn of
                            SOME ((less (exp1, exp2)),
                                    (eq (exp3, exp4))) => if exp1 = exp3 andalso exp2 = exp4
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



    fun parse _ = NONE (* da implementare *)
end

structure Imp :> IMPERATIVE = struct
    structure ASS = Ass

    datatype program = skip | assign of string * ASS.EXP.exp | cons of program * program
        | if_then_else of ASS.ass * program * program | while_do of ASS.ass * program

    fun toString skip = "skip"
        | toString (assign (var, ass)) = var ^ ":=" ^ ASS.EXP.toString ass
        | toString (cons (p, q)) = toString p ^ ";" ^ toString q
        | toString (if_then_else (b, p, q)) = "if(" ^ ASS.toString b ^ ") then" ^ toString p ^
        "else" ^ toString q
        | toString (while_do (b, p)) = "while(" ^ ASS.toString b ^ ") do " ^ toString p

    fun parse _ = NONE (* da implementare *)

end
