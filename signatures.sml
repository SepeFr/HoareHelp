signature EXPRESSION = sig
    datatype exp = k of int | var of string 
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp (* ceil / floor / mod ...*)
    
    val expn : exp -> int -> exp
    val subst : exp -> string -> exp -> exp
    val toString : exp -> string
end

signature ASSERTION = sig
    structure EXP : EXPRESSION

    datatype ass = t | f | imply of ass * ass
        | less of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp
    
    val not : ass -> ass
    val orr : ass -> ass -> ass
    val andd : ass -> ass -> ass
    val more : EXP.exp -> EXP.exp -> ass
    val subst : ass -> string -> EXP.exp -> ass
    val toString : ass -> string
end

signature IMPERATIVE = sig
    structure ASS : ASSERTION

    datatype program = skip | cons of program * program 
        | if_then_else of ASS.EXP.exp * program * program | while_do of ASS.ass * program 
        | var_is_in of string * program * program | assign of string * ASS.EXP.exp
    
    val toString : program -> string
end