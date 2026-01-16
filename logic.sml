local open ParseSupport

in
    structure Hoare : LOGIC = struct
        structure IMP = Imp

        datatype logic_rule = TRUTH | FALSEHOOD | WEAKENING | STRENGTHENING | AND | OR
        datatype program_rule = SKIP | ASSIGN | IF | WHILE | COMP | GENERIC
        datatype rule = logic of logic_rule | program of program_rule | HELP | BACK
        (* L'utente deve fornire in input la regola PRECISA da usare o lo facciamo capire al programma? Tanto l'utente, tra tutte le regole speciali, ne può applicare una specifica, che dovrebbe essere
            controllata comunque dal programma, in caso sarebbe da aggiungere un altro costruttore, niente di che *)
        (* Mettere skip all'inizio o inserire anche strenghtening? *)

        exception DerivationError of string

        type triple = IMP.program * IMP.ASS.ass * IMP.ASS.ass
        type weak_triple = IMP.program * (IMP.ASS.ass option) * IMP.ASS.ass
        type der_chain = triple list
        type weak_chain = weak_triple list

        fun string_goal (prg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass) : string =
            "{" ^ IMP.ASS.toString pre ^ "} " ^ IMP.toString prg ^ " {" ^ IMP.ASS.toString post ^ "}"

        fun consume_goal (triple : triple) (idx : int) : unit =
            TextIO.print (Int.toString idx ^ " )\t" ^ string_goal triple ^ "\n")

        fun current_goal (prg : IMP.program) (pre : IMP.ASS.ass option) (post : IMP.ASS.ass) : unit =
            TextIO.print ("Current subgoal:\t" ^ 
                            ( case pre of 
                                SOME p => string_goal (prg, p, post) 
                                | NONE => "??? " ^ IMP.toString prg ^ " {" ^ IMP.ASS.toString post ^ "}" ) ^ "\n")

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
            Utils.trim_space ( valOf (TextIO.inputLine TextIO.stdIn ) )
            handle Option.Option => (TextIO.print "Empty input, try again\n"; prompt printable) )

        fun rule_input () : rule =
            let val str : string = prompt (SOME ("Choose derivation rule to follow, from\n"
                                                ^ "TRUTH, FALSEHOOD, WEAKENING, STRENGTHENING, AND, OR,\n" 
                                                ^ "SKIP, ASSIGN, IF, WHILE, COMP,\n" 
                                                ^ "HELP, BACK: "))
                val upper_trimmed : string = Utils.trim_space (String.map Char.toUpper str)
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
                    | "COMP" => program COMP
                    | "BACK" => BACK
                    | "HELP" => HELP
                    | _ => (TextIO.print "Undefined rule, try again\n"; rule_input ())
            end

        fun assertion_input () : IMP.ASS.ass =
            valOf (parseAssString (prompt NONE))
            handle Option.Option | Fail _ => ( TextIO.print "Unparseable assertion, try again\n";
                                        assertion_input () )

        fun interact (fragment : IMP.program) (post : IMP.ASS.ass) (block : bool) : rule * IMP.ASS.ass option =
            case rule_input () of
                program SKIP => ( case fragment of
                                        IMP.skip => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program ASSIGN => ( case fragment of
                                        IMP.assign (_, _) => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program IF => ( case fragment of
                                    IMP.if_then_else (_, _, _) => (program GENERIC, NONE)
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program WHILE => ( case fragment of
                                        IMP.while_do (_, _) => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program COMP => ( case fragment of
                                        IMP.cons (_, _) => (program GENERIC, NONE)
                                        | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | program GENERIC => raise DerivationError "Program failure in \"step\" function"
                | logic TRUTH => ( case post of
                                    IMP.ASS.t => (logic TRUTH, NONE)
                                    | _ => ( TextIO.print "Unapplicable rule, try again\n"; interact fragment post block ) )
                | logic WEAKENING => (logic WEAKENING, 
                            SOME (TextIO.print ("Please input a Q such that\nQ ⊃ " ^ IMP.ASS.toString post ^ ":\n"); assertion_input ()) )
                | HELP => if block 
                            then (TextIO.print "Unapplicable rule, try again\n"; interact fragment post block)
                            else (HELP, SOME ( TextIO.print "Please input a plausible precondition for the current step:\n"; assertion_input () ))
                | els => (els, NONE)

        fun distribute_and (prg : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass) : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list  =
            case IMP.ASS.isAndd post of
                    SOME (l, r) => (distribute_and prg pre l) @ (distribute_and prg pre r)
                    | NONE => [(prg, pre, post)]

        fun distribute_or (prg : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass) : (IMP.program * IMP.ASS.ass * IMP.ASS.ass) list  =
            case IMP.ASS.isOrr pre of
                    SOME (l, r) => (distribute_or prg l post) @ (distribute_or prg r post)
                    | NONE => [(prg, pre, post)]

        fun confirm (message : string) : bool =
            let val res : string = prompt (SOME ("Do you confirm that " ^ message ^ "? ([yes]/no)"))
                val upperTrimmed : string = Utils.trim_space (String.map Char.toUpper res)
            in 
                case upperTrimmed of
                    "YES" => true
                    | "NO" => false
                    | _ => ("Wrong input, try again\n"; confirm message)
            end

        fun derive prog pre post = loop prog pre post nil

        and loop (prog : IMP.program) (pre : IMP.ASS.ass) (post : IMP.ASS.ass) (trace : weak_chain) : unit =
            let val (result : IMP.ASS.ass, _ : weak_chain) = (step prog (SOME pre) post trace)
            in
                if IMP.ASS.toString pre = IMP.ASS.toString result
                    then TextIO.print "\027[32mHooray!!!\027[0m\n"
                    else (TextIO.print "\027[31mDerivation failed, try again\027[0m\n"; loop prog pre post trace)
                    (*else (TextIO.print "Derivation failed, try again\n"; loop
                    * prog pre post trace)*)
            end

        and step (prg : IMP.program) (pre : IMP.ASS.ass option) (post : IMP.ASS.ass) (history : weak_chain): IMP.ASS.ass * weak_chain = (* da implementare, print current subgoal a ogni an unapplicable rulechiamata *)
                let val (rule : rule, help : IMP.ASS.ass option) = (current_goal prg pre post; interact prg post (isSome pre)) 
                    val next : weak_chain = (prg, pre, post)::history
                    val retry : unit -> IMP.ASS.ass * weak_chain =  fn () => step prg pre post history
                in
                    case rule of
                        program GENERIC => ( case prg of
                                    IMP.skip => (post, next)
                                    | IMP.assign (x, e) => (IMP.ASS.subst post x e, next)
                                    | IMP.cons (c1, c2) => let val (res : IMP.ASS.ass, _ : weak_chain) = step c2 NONE post next
                                                            in
                                                                step c1 pre res next
                                                            end
                                    | IMP.if_then_else (q, c1, c2) => ( case pre of
                                                                            SOME p => (parallel [(c1, IMP.ASS.andd p q, post),
                                                                                                    (c2, IMP.ASS.andd p (IMP.ASS.not q), post)]; (p, next))
                                                                            | NONE =>  (TextIO.print "Precondition unknown\n"; retry () ) )
                                    | IMP.while_do (q, c) => ( case pre of
                                                                SOME p => if confirm (IMP.ASS.toString (IMP.ASS.andd p (IMP.ASS.not q))
                                                                                        ^ " ≡ " ^ IMP.ASS.toString post)
                                                                            then (parallel [(c, IMP.ASS.andd p q, p),
                                                                                        (IMP.skip, IMP.ASS.andd p (IMP.ASS.not q), post)]; (p, next))
                                                                            else retry ()
                                                                | NONE => (TextIO.print "Precondition unknown\n"; retry () ) )
                                    )
                        | program _ => raise DerivationError "Program failure in \"step\" function"
                        | BACK => ( case history of
                                    (lprg, lpre, lpost) :: rest => (TextIO.print "Reverting back to previous step\n"; step lprg lpre lpost rest)
                                    | nil => (TextIO.print "Nothing to go back to\n"; retry ()) )
                        | logic TRUTH => ( case post of
                                            IMP.ASS.t => ( case pre of
                                                            SOME p => (p, next)
                                                            | NONE => (TextIO.print "Precondition unknown\n"; retry () ) )
                                            | _ => raise DerivationError "The program was asked to apply the TRUTH rule where it's inapplicable" )
                        | logic FALSEHOOD => (IMP.ASS.f, next)
                        | logic WEAKENING => ( case help of (*!!!*)
                                                SOME h => step prg pre h next
                                                | NONE => raise DerivationError "Unknown precondition for WEAKENING derivation" )
                        | logic STRENGTHENING => ( case pre of
                                                    SOME p => let val (res : IMP.ASS.ass, _ : weak_chain) = step prg pre post next
                                                                in 
                                                                    if confirm (IMP.ASS.toString (IMP.ASS.imply (p, res)))
                                                                    then (p, next)
                                                                    else retry ()
                                                                end
                                                    | NONE => (TextIO.print "Precondition unknown\n"; retry ()) )
                        | logic AND => ( case pre of 
                                            SOME p => ( parallel (distribute_or prg p post); (p, next) )
                                            | NONE => (TextIO.print "Precondition unknown\n"; retry ()) )
                        | logic OR => ( case pre of 
                                            SOME p => ( parallel (distribute_or prg p post); (p, next) )
                                            | NONE => (TextIO.print "Precondition unknown\n"; retry ()) )
                        | HELP => ( case help of 
                                        SOME _ => ( case pre of
                                                        SOME _ => (TextIO.print "Precondition already known\n"; retry () )
                                                        | NONE => step prg help post history ) 
                                        | NONE => raise DerivationError "Unknown precondition for HELPED derivation" )
                end

        and parallel (nil : der_chain) : unit = raise DerivationError "The program can't find the next derivation to complete"
            | parallel ((prg, pre, post) :: nil) = derive prg pre post
            | parallel der_list =
                let val idx : int = (next_goal der_list) - 1
                    val (prg : IMP.program, pre : IMP.ASS.ass, post : IMP.ASS.ass) = List.nth (der_list, idx)
                    val begin : der_chain = List.take (der_list, idx)
                    val endd : der_chain = List.drop (der_list, idx + 1)
                in (derive prg pre post; parallel (begin @ endd)) end

        fun get_program_str () : string =
            case prompt NONE of
                "ENDPROGRAM" => ""
                | els => els ^ " " ^ get_program_str ()

        fun get_program () : IMP.program =
            valOf (parseImpString (get_program_str ()))
            handle Option.Option | Fail _ => (TextIO.print "Unparseable program, try again\n"; get_program())

        fun main () : unit =
            let val prog : IMP.program = (
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
                    derive prog pre post
                )
            end
    end
end

val _ : unit = Hoare.main()