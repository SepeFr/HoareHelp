signature UTILS = sig
    val trim_space : string -> string
end

signature EXPRESSION = sig
    datatype exp = k of int | var of string 
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp (* ceil / floor / mod ...*)
    
    val expn : exp -> int -> exp
    val subst : exp -> string -> exp -> exp
    val toString : exp -> string
    val parse : string -> exp option
end

signature ASSERTION = sig
    structure EXP : EXPRESSION

    datatype ass = t | f | imply of ass * ass
        | less of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp
    
    val not : ass -> ass
    val orr : ass -> ass -> ass
    val isOrr : ass -> (ass * ass) option
    val andd : ass -> ass -> ass
    val isAndd : ass -> (ass * ass) option
    val more : EXP.exp -> EXP.exp -> ass
    val subst : ass -> string -> EXP.exp -> ass
    val toString : ass -> string
    val parse : string -> ass option
end

signature IMPERATIVE = sig
    structure ASS : ASSERTION

    datatype program = skip | assign of string * ASS.EXP.exp | cons of program * program 
        | if_then_else of ASS.ass * program * program | while_do of ASS.ass * program 
    
    val toString : program -> string
    val parse : string -> program option
end

signature LOGIC = sig
    structure IMP : IMPERATIVE
    
    exception DerivationError of string
    
    val derive : IMP.program -> IMP.ASS.ass -> IMP.ASS.ass -> unit
end