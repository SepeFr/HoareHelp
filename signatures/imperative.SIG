signature IMPERATIVE = sig
    structure ASS : ASSERTION

    datatype program = skip | assign of string * ASS.EXP.exp | cons of program * program
        | if_then_else of ASS.ass * program * program | while_do of ASS.ass * program

    val toString : program -> string
    val parse : string -> program option
end
