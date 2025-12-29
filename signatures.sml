signature EXPRESSION = sig
    datatype exp = k of int | var of string 
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp (* ceil / floor / mod ...*)
    
    val expn : exp -> int -> exp
    val subst : exp -> string -> exp -> exp
    val toString : exp -> string
    val sEq : exp -> exp -> bool
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
    val parse : string -> ass
end

signature IMPERATIVE = sig
    structure ASS : ASSERTION

    datatype program = skip | cons of program * program 
        | if_then_else of ASS.EXP.exp * program * program | while_do of ASS.ass * program 
        | var_is_in of string * program * program | assign of string * ASS.EXP.exp
    
    val toString : program -> string
    val parse : string -> program
end

signature LOGIC = sig
    structure IMP : IMPERATIVE

    datatype logic_rule = TRUTH | FALSEHOOD | STRENGTHENING | WEAKENING | AND | OR 
    datatype program_rule = IF | WHILE | ASSIGN | SKIP | COMPOSE
    datatype rule = logic of logic_rule | program of program_rule
    
    val precond : IMP.program -> IMP.ASS.ass -> IMP.ASS.ass -> IMP.ASS.ass
    val step : IMP.program * IMP.ASS.ass -> IMP.ASS.ass option -> IMP.program * IMP.ASS.ass
    val ask : unit -> IMP.ASS.ass
end