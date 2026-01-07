structure Hoare : LOGIC = struct
    structure IMP = Imp

    datatype logic_rule = TRUTH | FALSEHOOD | WEAKENING | AND | OR 
    datatype rule = logic of logic_rule | PROGRAM
    (* L'utente deve fornire in input la regola PRECISA da usare o lo facciamo capire al programma?
        Tanto l'utente, tra tutte le regole speciali, ne può applicare una specifica, che dovrebbe essere
        controllata comunque dal programma, in caso sarebbe da aggiungere un altro costruttore, niente di che *)
    (* Mettere skip all'inizio o inserire anche strenghtening? *)

    exception DerivationError of string

    fun string_goal (prg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass) : string =
        "{" ^ IMP.ASS.toString pre ^ "} " ^ IMP.toString prg ^ " {" ^ IMP.ASS.toString post ^ "}"

    fun consume_goal (triple : IMP.program * IMP.ASS.ass * IMP.ASS.ass) (idx : int) : unit = 
        TextIO.print (Int.toString idx ^ " )\t" ^ string_goal triple ^ "\n")
              
    fun current_goal (prg : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass) : unit = 
        TextIO.print ("Current subgoal:\t" ^ string_goal (prg, pre, post) ^ "\n")

    fun subgoals (nil : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) (_ : int) : unit = ()
        | subgoals (node :: l) idx = (consume_goal node idx; subgoals l (idx + 1))

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
            else ( TextIO.print "Input number out of range, try again\n"; next_goal der_list) 
        end )

    fun rule_input () : rule = 
        let val str : string = ( TextIO.print ("Choose derivation rule to follow, from\n"
                                            ^ "TRUTH, FALSEHOOD, WEAKENING,\n"
                                            ^ "AND, OR, PROGRAM: ");
                                valOf (TextIO.inputLine TextIO.stdIn) )
            val upper_trimmed : string = Utils.trim_space (String.map Char.toUpper str)
        in 
            case upper_trimmed of
                "TRUTH" => logic TRUTH
                | "FALSEHOOD" => logic FALSEHOOD
                | "WEAKENING" => logic WEAKENING
                | "AND" => logic AND
                | "OR" => logic OR
                | "PROGRAM" => PROGRAM
                | _ => (TextIO.print "Undefined rule, try again\n"; rule_input ())
        end

    fun assertion_input () : IMP.ASS.ass = 
        valOf (IMP.ASS.parse (valOf (TextIO.inputLine TextIO.stdIn)))
            handle Option.Option => ( TextIO.print "Unparseable assertion, try again\n";
                                                            assertion_input () )

    fun interact (fragment : IMP.program) (r : IMP.ASS.ass) : rule * IMP.ASS.ass option = 
        case rule_input () of
            PROGRAM => ( case fragment of
                            IMP.if_then_else (q, c1, c2) => 
                                let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide any P such that\n"
                                                    ^ "1)\t{P ∧ " ^ IMP.ASS.toString q ^ "} " ^ IMP.toString c1 ^ " {" ^ IMP.ASS.toString r ^ "}\n"
                                                    ^ "2)\t{P ∧ " ^ IMP.ASS.toString (IMP.ASS.not q) ^ "} " ^ IMP.toString c2 ^ " {" ^ IMP.ASS.toString r ^ "}:\n");
                                                    assertion_input () )
                                in (PROGRAM, SOME a_input) end
                            | IMP.while_do (q, c) => 
                                    let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide any P such that\n"
                                                        ^ "1)\t{P ∧ " ^ IMP.ASS.toString q ^ "} " ^ IMP.toString c ^ " {P}\n"
                                                        ^ "2)\t{P ∧ " ^ IMP.ASS.toString (IMP.ASS.not q) ^ "} skip {" ^ IMP.ASS.toString r ^ "}:\n");
                                                        assertion_input () )
                                    in (PROGRAM, SOME a_input) end
                            | _ => (PROGRAM, NONE)
                        )
            | logic TRUTH => ( case r of 
                                IMP.ASS.t => 
                                    let val a_input : IMP.ASS.ass = ( TextIO.print "Please provide any P such that P is an assertion:\n";
                                                        assertion_input () )
                                    in (logic TRUTH, SOME a_input) end
                                | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment r) 
                                )
            | logic FALSEHOOD => (logic FALSEHOOD, NONE)
            | logic WEAKENING => let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide any P such that\n" 
                                                                                    ^ "P ⊃ " ^ IMP.ASS.toString r ^ ":\n");
                                                                        assertion_input () )
                                    in (logic WEAKENING, SOME a_input) end
            | logic AND => let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide any P such that P ⊃ Qi,\n" 
                                                                        ^ "considering " ^ IMP.ASS.toString r ^ " as a conjunction of Qis:\n");
                                                                        assertion_input () )
                            in (logic AND, SOME a_input) end
            | logic OR => let val a_input : IMP.ASS.ass = ( TextIO.print ("Please provide any P such that\n" 
                                                                        ^ "Pi ⊃ " ^ IMP.ASS.toString r ^ ",\n"
                                                                        ^ "considering P as a disjunction of Pis:\n");
                                                                        assertion_input () )
                            in (logic OR, SOME a_input) end

    fun distribute_and (prg : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass) : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list  = 
        case IMP.ASS.isAndd post of 
                SOME (l, r) => (distribute_and prg pre l) @ (distribute_and prg pre r)
                | NONE => [(prg, pre, post)]
    
    fun distribute_or (prg : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass) : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list  = 
        case IMP.ASS.isOrr pre of 
                SOME (l, r) => (distribute_or prg l post) @ (distribute_or prg r post)
                | NONE => [(prg, pre, post)]

    fun derive IMP.skip pre post =
        if pre = post (* se pretty print ambiguo, uguale su toString *)
            then TextIO.print "QED\n"
            else derive IMP.skip pre (step IMP.skip pre post)
        | derive program pre post = derive IMP.skip pre (step program pre post)

    and step (prg : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass): IMP.ASS.ass = (* da implementare, print current subgoal a ogni an unapplicable rulechiamata *)
            let val (rule : rule, help : IMP.ASS.ass option) = (current_goal prg pre post; interact prg post) 
            in 
                case rule of
                    PROGRAM => ( case prg of 
                                IMP.skip => post
                                | IMP.assign (x, e) => IMP.ASS.subst post x e
                                | IMP.cons (c1, c2) => step c1 pre (step c2 pre post)
                                | IMP.if_then_else (q, c1, c2) => ( case help of
                                                                        SOME p => (parallel [(c1, IMP.ASS.andd p q, post),
                                                                                                (c2, IMP.ASS.andd p (IMP.ASS.not q), post)]; p) 
                                                                        | NONE => raise DerivationError "Unknown precondition for IF derivation" ) 
                                | IMP.while_do (q, c) => ( case help of 
                                                            SOME p => (parallel [(c, IMP.ASS.andd p q, p),
                                                                                    (IMP.skip, IMP.ASS.andd p (IMP.ASS.not q), post)]; p) 
                                                            | NONE => raise DerivationError "Unknown precondition for WHILE derivation" ) 
                                )
                    | logic TRUTH => ( case post of
                                        IMP.ASS.t => ( case help of 
                                                        SOME p => p
                                                        | NONE => raise DerivationError "Unknown precondition for TRUTH derivation" )
                                        | _ => raise DerivationError "The program was asked to apply the TRUTH rule where it's inapplicable" )
                    | logic FALSEHOOD => IMP.ASS.f
                    | logic WEAKENING => ( case help of 
                                            SOME p => step prg pre p
                                            | NONE => raise DerivationError "Unknown precondition for WEAKENING derivation" )
                    | logic AND => ( case help of 
                                        SOME p => (parallel (distribute_and prg p post); p)
                                        | NONE => raise DerivationError "Unknown precondition for AND derivation" )
                    | logic OR => ( case help of 
                                        SOME p => (parallel (distribute_or prg p post); p)
                                        | NONE => raise DerivationError "Unknown precondition for OR derivation" )
            end

    and parallel (nil : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list) : unit = raise DerivationError "The program can't find the next derivation to complete"
        | parallel ((prg, pre, post) :: nil) = derive prg pre post
        | parallel der_list = 
            let val idx : int = (next_goal der_list) - 1
                val (prg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass) = List.nth (der_list, idx)
                val begin : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list = List.take (der_list, idx)
                val endd : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list = List.drop (der_list, idx + 1)
            in (derive prg pre post; parallel (begin @ endd)) end

    fun get_program_str () : string = 
        let val input : string = valOf (TextIO.inputLine TextIO.stdIn)
        in 
            case Utils.trim_space input of
                "ENDPROGRAM" => ""
                | _ => input ^ " " ^ get_program_str ()
        end

    fun get_program () : IMP.program =
        case IMP.parse (get_program_str ()) of
            SOME p => p
            | NONE => ("Unparseable program, try again\n"; get_program())
    
    fun main () : unit =
        let val program : IMP.program = (
                TextIO.print "HoareHelp - Matteo & Francesco 2025\n";
                TextIO.print "Welcome to the HoareHelp proof helper,\n";
                TextIO.print "please input a program written in the Imp (While) language,\n";
                TextIO.print "ending with the line \"ENDPROGRAM\"\n";
                get_program ()
            )
            val pre : IMP.ASS.ass = (
                TextIO.print "Got it, now please input a precondition for such program:\n";
                assertion_input ()
            )
            val post : IMP.ASS.ass = (
                TextIO.print "Got it, now please input a postcondition:\n";
                assertion_input ()
            )
        in 
            (
                TextIO.print "Great, the proofing subroutine will now be started.\n";
                derive program pre post
            )
        end
        
end