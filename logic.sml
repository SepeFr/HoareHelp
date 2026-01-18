local open ParseSupport

in
    structure Hoare = struct

        datatype logic_rule = TRUTH | FALSEHOOD | WEAKENING | STRENGTHENING | AND | OR
        datatype program_rule = SKIP | ASSIGN | IF | WHILE | COMP | GENERIC
        datatype rule = logic of logic_rule | program of program_rule | HELP
        (* L'utente deve fornire in input la regola PRECISA da usare o lo facciamo capire al programma? Tanto l'utente, tra tutte le regole speciali, ne può applicare una specifica, che dovrebbe essere
            controllata comunque dal programma, in caso sarebbe da aggiungere un altro costruttore, niente di che *)
        (* Mettere skip all'inizio o inserire anche strenghtening? *)

        exception DerivationError of string

        type triple = Imp.program * Imp.ASS.ass * Imp.ASS.ass
        type weak_triple = Imp.program * (Imp.ASS.ass option) * Imp.ASS.ass
        type der_chain = triple list
        type weak_chain = weak_triple list

        fun string_goal (prg : Imp.program, pre : Imp.ASS.ass, post : Imp.ASS.ass) : string =
            id "Pre-Condition : " ^ "{ " ^ Imp.ASS.toString pre ^ " }\n" ^
            Imp.toString prg ^ "\n" ^
            id "Post-Condition :" ^ "{ " ^ Imp.ASS.toString post ^ " }"

        fun consume_goal (triple : triple) (idx : int) : unit =
            TextIO.print (Int.toString idx ^ " )\t" ^ string_goal triple ^ "\n")

        fun current_goal (prg : Imp.program) (pre : Imp.ASS.ass option) (post : Imp.ASS.ass) : unit =
            TextIO.print (
                        "\nCurrent subgoal\n" ^
                            ( case pre of
                                SOME p => string_goal (prg, p, post)
                                | NONE =>
                                  id "Pre-Condition : " ^ "{ ??? }\n" ^
                                  Imp.toString prg ^ "\n" ^
                                  id "Post-Condition :" ^ "{ " ^ Imp.ASS.toString post ^ " }"
                                ))

        fun subgoals (nil : der_chain) (_ : int) : unit = ()
            | subgoals (node :: l) idx = (consume_goal node idx; subgoals l (idx + 1))

        fun numeric_input () : int = ( case TextIO.inputLine TextIO.stdIn of
                                        SOME thing => ( valOf (Int.fromString thing)
                                                        handle Option.Option =>
                                                            ( TextIO.print "Wrong input, try again\n";
                                                                numeric_input () ) )
                                        | NONE => numeric_input () )

        fun next_goal (der_list : der_chain) : int =
            ( subgoals der_list 1;
            TextIO.print "Select next goal: ";
            (*TextIO.flushOut TextIO.stdOut;*)
            let val return : int = numeric_input ()
            in if return > 0 andalso return <= length der_list
                then return
                else ( TextIO.print "Input number out of range, try again\n"; next_goal der_list)
            end )

        fun prompt (printable : string option): string =
            ( case printable of
                SOME thing => TextIO.print thing (*;TextIO.flushOut TextIO.stdOut*)
                | NONE => ();
            Utils.trim_newline ( valOf (TextIO.inputLine TextIO.stdIn ) )
            handle Option.Option => (TextIO.print "Empty input, try again\n"; prompt printable) )

        fun rule_input () : rule =
            let val str : string = prompt (SOME (
                "\nChoose derivation rule\n" ^
                id "Logical rules: " ^
                "  " ^ kw "TRUTH" ^ ", " ^ kw "FALSEHOOD" ^ ", " ^ kw "WEAKENING" ^ ", " ^
                      kw "STRENGTHENING" ^ ", " ^ kw "AND" ^ ", " ^ kw "OR" ^ "\n" ^
                id "Program rules: " ^
                "  " ^ kw "SKIP" ^ ", " ^ kw "ASSIGN" ^ ", " ^ kw "IF" ^ ", " ^
                      kw "WHILE" ^ ", " ^ kw "COMPOSITION" ^ "\n" ^
                id "Other: " ^
                "  " ^ kw "HELP" ^ "\n" ^
                "Your choice: "
                                                ))
                val upper_trimmed : string = Utils.trim_newline (String.map Char.toUpper str)
            in
                case upper_trimmed of
                    "TRUTH" => logic TRUTH
                    | "FALSEHOOD" => logic FALSEHOOD
                    | "WEAKENING" => logic WEAKENING
                    | "STRENGTHENING" => logic STRENGTHENING
                    | "AND" => logic AND
                    | "OR" => logic OR
                    | "SKIP" => program SKIP
                    | "ASSIGN" => program ASSIGN
                    | "IF" => program IF
                    | "WHILE" => program WHILE
                    | "COMPOSITION" => program COMP
                    | "HELP" => HELP
                    | _ => (TextIO.print "Undefined rule, try again\n"; rule_input ())
            end

        fun assertion_input () : Imp.ASS.ass =
            valOf (parseAssString (prompt NONE))
            handle Option.Option | Fail _ => ( TextIO.print "Unparseable assertion, try again\n";
                                        assertion_input () )

        fun interact (fragment : Imp.program) (post : Imp.ASS.ass) (block : bool) : rule * Imp.ASS.ass option =
            case rule_input () of
                program SKIP => ( case fragment of
                                        Imp.skip => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program ASSIGN => ( case fragment of
                                        Imp.assign (_, _) => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program IF => ( case fragment of
                                    Imp.if_then_else (_, _, _) => (program GENERIC, NONE)
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program WHILE => ( case fragment of
                                        Imp.while_do (_, _) => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program COMP => ( case fragment of
                                        Imp.cons (_, _) => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program GENERIC => raise DerivationError "Program failure in \"step\" function"
                | logic TRUTH => ( case post of
                                    Imp.ASS.t => (logic TRUTH, NONE)
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | logic WEAKENING => (logic WEAKENING,
                            SOME (TextIO.print ("Please input a Q such that\nQ ⊃ " ^ Imp.ASS.toString post ^ ":\n"); assertion_input ()) )
                | HELP => if block
                            then (TextIO.print "Unapplicable rule, try again\n"; interact fragment post block)
                            else (HELP, SOME ( TextIO.print "Please input a plausible precondition for the current step:\n"; assertion_input () ))
                | els => (els, NONE)

        fun distribute_and (prg : Imp.program) (pre : Imp.ASS.ass) (post : Imp.ASS.ass) : (Imp.program * Imp.ASS.ass * Imp.ASS.ass) list  =
            case Imp.ASS.isAndd post of
                    SOME (l, r) => (distribute_and prg pre l) @ (distribute_and prg pre r)
                    | NONE => [(prg, pre, post)]

        fun distribute_or (prg : Imp.program) (pre : Imp.ASS.ass) (post : Imp.ASS.ass) : (Imp.program * Imp.ASS.ass * Imp.ASS.ass) list  =
            case Imp.ASS.isOrr pre of
                    SOME (l, r) => (distribute_or prg l post) @ (distribute_or prg r post)
                    | NONE => [(prg, pre, post)]

        fun confirm (message : string) : bool =
            let val res : string = prompt (SOME ("Do you confirm that " ^ message ^ "? (yes/no)\n"))
                val upperTrimmed : string = Utils.trim_newline (String.map Char.toUpper res)
            in
                case upperTrimmed of
                    "YES" => true
                    | "NO" => false
                    | _ => (TextIO.print "Wrong input, try again\n"; confirm message)
            end

        fun derive prog pre post =
            let val result : Imp.ASS.ass = (step prog (SOME pre) post)
            in
                if Imp.ASS.toString pre = Imp.ASS.toString result
                    then TextIO.print "\027[32mHooray!!!\027[0m\n"
                    else (TextIO.print "\027[31mDerivation failed, try again\027[0m\n"; derive prog pre post)
                    (*else (TextIO.print "Derivation failed, try again\n"; loop
                    * prog pre post trace)*)
            end

        and step (prg : Imp.program) (pre : Imp.ASS.ass option) (post : Imp.ASS.ass): Imp.ASS.ass = (* da implementare, print current subgoal a ogni an unapplicable rulechiamata *)
                let val (rule : rule, help : Imp.ASS.ass option) = (current_goal prg pre post; interact prg post (isSome pre))
                    fun retry () : Imp.ASS.ass = step prg pre post
                in
                    case rule of
                        program GENERIC => ( case prg of
                                    Imp.skip => post
                                    | Imp.assign (x, e) => Imp.ASS.subst post x e
                                    | Imp.cons (c1, c2) => let val res : Imp.ASS.ass = step c2 NONE post
                                                            in
                                                                step c1 pre res
                                                            end
                                    | Imp.if_then_else (q, c1, c2) => ( case pre of
                                                                            SOME p => (parallel [(c1, Imp.ASS.andd p q, post),
                                                                                                    (c2, Imp.ASS.andd p (Imp.ASS.not q), post)]; p)
                                                                            | NONE =>  (TextIO.print (err "Precondition unknown\n"); retry () ) )
                                    | Imp.while_do (q, c) => ( case pre of
                                                                SOME p => if confirm (Imp.ASS.toString (Imp.ASS.andd p (Imp.ASS.not q))
                                                                                        ^ " ≡ " ^ Imp.ASS.toString post)
                                                                            then (derive c (Imp.ASS.andd p q) p; p)
                                                                            else retry ()
                                                                | NONE => (TextIO.print (err "Precondition unknown\n"); retry () ) )
                                    )
                        | program _ => raise DerivationError "Program failure in \"step\" function"
                        | logic TRUTH => ( case post of
                                            Imp.ASS.t => ( case pre of
                                                            SOME p => p
                                                            | NONE => (TextIO.print (err "Precondition unknown\n"); retry () ) )
                                            | _ => raise DerivationError "The program was asked to apply the TRUTH rule where it's inapplicable" )
                        | logic FALSEHOOD => Imp.ASS.f
                        | logic WEAKENING => ( case help of
                                                SOME h => step prg pre h
                                                | NONE => raise DerivationError "Unknown precondition for WEAKENING derivation" )
                        | logic STRENGTHENING => ( case pre of
                                                    SOME p => let val res : Imp.ASS.ass = step prg pre post
                                                                in
                                                                    if confirm (Imp.ASS.toString p ^ " ⊃ " ^ Imp.ASS.toString res)
                                                                    then p
                                                                    else retry ()
                                                                end
                                                    | NONE => (TextIO.print (err "Precondition unknown\n"); retry ()) )
                        | logic AND => ( case pre of
                                            SOME p => ( parallel (distribute_and prg p post); p )
                                            | NONE => (TextIO.print (err "Precondition unknown\n"); retry ()) )
                        | logic OR => ( case pre of
                                            SOME p => ( parallel (distribute_or prg p post); p )
                                            | NONE => (TextIO.print (err "Precondition unknown\n"); retry ()) )
                        | HELP => ( case help of
                                        SOME _ => ( case pre of
                                                        SOME _ => (TextIO.print (err "Precondition already known\n"); retry () )
                                                        | NONE => step prg help post ) (* derive prg h post; h *)
                                        | NONE => raise DerivationError "Unknown precondition for HELPED derivation" )
                end

        and parallel (nil : der_chain) : unit = raise DerivationError "The program can't find the next derivation to complete"
            | parallel ((prg, pre, post) :: nil) = derive prg pre post
            | parallel der_list =
                let val idx : int = (next_goal der_list) - 1
                    val (prg : Imp.program, pre : Imp.ASS.ass, post : Imp.ASS.ass) = List.nth (der_list, idx)
                    val begin : der_chain = List.take (der_list, idx)
                    val endd : der_chain = List.drop (der_list, idx + 1)
                in (derive prg pre post; parallel (begin @ endd)) end

        fun get_program_str () : string =
            case prompt NONE of
                "ENDPROGRAM" => ""
                | els => els ^ " " ^ get_program_str ()

        fun get_program () : Imp.program =
            valOf (parseImpString (get_program_str ()))
            handle Option.Option | Fail _ => (TextIO.print "Unparseable program, try again\n"; get_program())

        fun main () : unit =
            let val prog : Imp.program = (
                    TextIO.print (
                      ANSI.Delimiter ^
                      "──────────────────────────────────────────────\n" ^
                      ANSI.Keyword ^ "HoareHelp" ^
                      ANSI.gray ^ " - Hoare Logic Proof Assistant\n" ^
                      ANSI.Identifier ^ "Matteo & Francesco (2025)\n" ^
                      ANSI.Delimiter ^
                      "──────────────────────────────────────────────\n" ^
                      ANSI.reset
                    );

                      TextIO.print (
                        ANSI.Identifier ^
                        "Welcome to the HoareHelp proof helper.\n" ^
                        ANSI.reset ^
                        "Please input a program written in the " ^
                        ANSI.Keyword ^ "Imp (While)" ^
                        ANSI.reset ^ " language,\n" ^
                        "ending with the line " ^
                        ANSI.Operator ^ "\"ENDPROGRAM\"" ^
                        ANSI.reset ^ ".\n\n"
                      );
                    get_program ()
                )
                val pre : Imp.ASS.ass = (
                    TextIO.print ("Got Program\n" ^
                    Imp.toString prog ^ "\n");
                    (TextIO.print "now please input a precondition for such program:\n");
                    assertion_input ()
                )
                val post : Imp.ASS.ass = (
                    TextIO.print ("Got Pre-Condition\n" ^
                    Imp.ASS.toString pre ^ "\n");
                    TextIO.print "now please input a postcondition:\n";
                    assertion_input ()
                )
            in
                (
                    TextIO.print ("Got Post-Condition\n" ^
                    Imp.ASS.toString post ^ "\n");
                    TextIO.print "Great, the proofing subroutine will now be started.\n\n";
                    derive prog pre post
                )
            end
    end
end

val _ : unit = ( Hoare.main();
                QED.rainbow_qed() )
