structure Imp :> IMPERATIVE = struct
    structure ASS = Ass

    datatype program = skip | assign of string * ASS.EXP.exp | cons of program * program
        | if_then_else of ASS.ass * program * program | while_do of ASS.ass * program

    (*fun toString skip = "skip"
        | toString (assign (var, ass)) = var ^ " := " ^ ASS.EXP.toString ass
        | toString (cons (p, q)) = toString p ^ "; " ^ toString q
        | toString (if_then_else (b, p, q)) = "if (" ^ ASS.toString b ^ ") then {" ^ toString p ^
        "} else {" ^ toString q ^ "}"
        | toString (while_do (b, p)) = "while (" ^ ASS.toString b ^ ") do {" ^
        toString p ^ "}"*)


    fun toString prog =
    let
      fun indMore ind = "  " ^ ind

      fun pp ind skip =
            ind ^ kw "skip"

        | pp ind (assign (var, ass)) =
            ind ^ id var ^ " " ^ ope ":=" ^ " " ^ ASS.EXP.toString ass

        | pp ind (cons (p, q)) =
            pp ind p ^ par ";" ^ "\n" ^ pp ind q

        | pp ind (if_then_else (b, p, q)) =
            ind ^ kw "if" ^ " " ^ par "(" ^ ASS.toString b ^ par ")" ^ " " ^
            kw "then" ^ " " ^ par "{\n" ^
            pp (indMore ind) p ^ "\n" ^
            ind ^ par "}" ^ " " ^ kw "else" ^ " " ^ par "{\n" ^
            pp (indMore ind) q ^ "\n" ^
            ind ^ par "}"

        | pp ind (while_do (b, p)) =
            ind ^ kw "while" ^ " " ^ par "(" ^ ASS.toString b ^ par ")" ^ " " ^
            kw "do" ^ " " ^ par "{\n" ^
            pp (indMore ind) p ^ "\n" ^
            ind ^ par "}"
    in
      pp "" prog
    end

end
