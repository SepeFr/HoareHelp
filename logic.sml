structure Hoare :> LOGIC = struct
    structure IMP = Imp

    datatype logic_rule = TRUTH | FALSEHOOD | STRENGTHENING | WEAKENING | AND | OR 
    datatype program_rule = IF | WHILE | ASSIGN | COMPOSE (* la regola skip non si può usare, serve per terminare il programma *)
    datatype rule = logic of logic_rule | prog of program_rule

    exception DerivationError of string

    fun print_goal (prg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass, idx : int) : unit = 
        TextIO.print ((Int.toString idx) ^ " )\t{" ^ (IMP.ASS.toString pre) ^ "} " 
                        ^ (IMP.toString prg) ^ " {" ^ (IMP.ASS.toString post) ^ "}\n")

    fun subgoals (nil : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) (_ : int) : unit = ()
        | subgoals ((prg, pre, post) :: l) idx = (print_goal (prg, pre, post, idx); subgoals l (idx + 1))

    fun numeric_input () : int = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
                                handle Option.Option => ( TextIO.print "Wrong input, try again\n";
                                                            numeric_input () ) (* viene gestita solo l'espressione 
                                                                        più esterna (?) *)

    fun next_goal (der_list : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) : int = 
        ( subgoals der_list 1;
        TextIO.print "Select next goal: ";
        let val return : int = numeric_input ()
        in if return > 0 andalso return <= length der_list
            then return
            else ( TextIO.print "Input number too high, try again\n"; next_goal der_list) 
        end )

    fun rule_input () : rule = 
        let val str : string = ( TextIO.print ("Choose derivation rule to follow, from\n"
                                            ^ "TRUTH, FALSEHOOD, STRENGTHENING, WEAKENING, AND, OR,\n"
                                            ^ "IF, WHILE, ASSIGN, COMPOSE: ");
                                valOf (TextIO.inputLine TextIO.stdIn) )
            val upper : string = String.map Char.toUpper str
            val trimmed : string = Utils.trim_space upper
        in 
            case trimmed of
                "TRUTH" => logic TRUTH
                | "FALSEHOOD" => logic FALSEHOOD
                | "STRENGTHENING" => logic STRENGTHENING
                | "WEAKENING" => logic WEAKENING
                | "AND" => logic AND
                | "OR" => logic OR
                | "IF" => prog IF
                | "WHILE" => prog WHILE
                | "ASSIGN" => prog ASSIGN
                | "COMPOSE" => prog COMPOSE
                | _ => (TextIO.print "Undefined rule, try again\n"; rule_input ())
        end

    fun assertion_input () : IMP.ASS.ass = 
        let val input : string = valOf (TextIO.inputLine TextIO.stdIn)
        in valOf (IMP.ASS.parse input)
            handle Option.Option =>  ( TextIO.print "Wrong input, try again\n";
                                                            assertion_input () )
        end

    fun interact (fragment : IMP.program) (r : IMP.ASS.ass) : rule * IMP.ASS.ass option = 
        let val r_input : rule = rule_input()
        in 
            case r_input of
                prog IF => ( case fragment of
                                IMP.if_then_else (q, c1, c2) => 
                                    let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide a P such that"
                                                        ^ "1) {P ∧ " ^ IMP.ASS.toString q ^ "} " ^ IMP.toString c1 ^ "{" ^ IMP.ASS.toString r ^ "}\n"
                                                        ^ "2) {P ∧ " ^ IMP.ASS.toString (IMP.ASS.not q) ^ "} " ^ IMP.toString c2 ^ "{" ^ IMP.ASS.toString r ^ "}\n");
                                                        assertion_input () )
                                    in (r_input, SOME a_input) end
                                | _ => ( TextIO.print "Unapplicable rule, try again\n";
                                        interact fragment r)
                            )
                | prog WHILE => ( case fragment of
                                    IMP.while_do (q, c) => 
                                        let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide a P such that"
                                                            ^ "{P ∧ " ^ IMP.ASS.toString q ^ "} " ^ IMP.toString c ^ "{" ^ IMP.ASS.toString r ^ "}\n");
                                                            assertion_input () )
                                        in (r_input, SOME a_input) end
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n";
                                            interact fragment r)
                                )
                | prog ASSIGN => ( case fragment of
                                    IMP.assign (_, _) => (r_input, NONE)
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n";
                                            interact fragment r)
                                )
                | prog COMPOSE => ( case fragment of
                                    IMP.cons (_, _) => (r_input, NONE)
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n";
                                            interact fragment r)
                                )
                | _ => (r_input, NONE) (* regole logiche si possono sempre usare *)
        end

    fun derive IMP.skip pre post =
        if pre = post 
            then () 
            else let val (_ : IMP.program, new : IMP.ASS.ass) = step (IMP.skip, post)
                                            in derive IMP.skip pre new end
        | derive program pre post = let val (prog2 : IMP.program, new : IMP.ASS.ass) = step (program, post)
                                            in derive prog2 pre new end

    and step (prg : IMP.program, post : IMP.ASS.ass) : IMP.program * IMP.ASS.ass = (* da implementare *)
            let val (rule : rule, help : IMP.ASS.ass option) = interact prg post 
            in 
                case rule of
                    prog IF => raise Fail "Not implemented\n"
                    | prog WHILE => raise Fail "Not implemented\n"
                    | prog ASSIGN => raise Fail "Not implemented\n"
                    | prog COMPOSE => raise Fail "Not implemented\n"
                    | logic TRUTH => raise Fail "Not implemented\n"
                    | logic FALSEHOOD => raise Fail "Not implemented\n"
                    | logic STRENGTHENING => raise Fail "Not implemented\n"
                    | logic WEAKENING => raise Fail "Not implemented\n"
                    | logic AND => raise Fail "Not implemented\n"
                    | logic OR => raise Fail "Not implemented\n"
            end

    (* fun ask () : rule * IMP.ASS.ass option = (logic TRUTH, NONE) da implementare *)
    (* fun print () : unit = () *)
    and parallel (nil : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) : unit = ()
        | parallel der_list = 
            let val idx : int = (next_goal der_list) - 1
                val (prg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass) = List.nth (der_list, idx)
                val begin : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list = List.take (der_list, idx)
                val endd : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list = List.drop (der_list, idx + 1)
            in (derive prg pre post; parallel (begin @ endd)) end
end