structure Imp :> IMPERATIVE = struct
    structure ASS = Ass

    datatype program = skip | assign of string * ASS.EXP.exp | cons of program * program
        | if_then_else of ASS.ass * program * program | while_do of ASS.ass * program

    fun toString skip = "skip"
        | toString (assign (var, ass)) = var ^ " := " ^ ASS.EXP.toString ass
        | toString (cons (p, q)) = toString p ^ "; " ^ toString q
        | toString (if_then_else (b, p, q)) = "if (" ^ ASS.toString b ^ ") then {" ^ toString p ^
        "} else {" ^ toString q ^ "}"
        | toString (while_do (b, p)) = "while (" ^ ASS.toString b ^ ") do {" ^
        toString p ^ "}"


end
