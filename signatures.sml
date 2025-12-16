signature EXPRESSION = sig
    datatype exp = k of int | var of string 
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp (* ceil / floor / mod ...*)
    
    val expn : exp -> exp
    val subst : exp -> string -> exp -> exp
    val toString : exp -> string
end

signature DISJUNCTION = sig
    structure EXP : EXPRESSION

    datatype cond = less of EXP.exp * EXP.exp
        | more of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp
        | orr of cond * cond
    
    val subst : cond -> string -> EXP.exp -> cond
    val toString : cond -> string
end

signature CONJUNCTION = sig
    structure EXP : EXPRESSION
    structure DISJ : DISJUNCTION

    datatype form = t | f | term of DISJ.cond | andd of DISJ.cond * form

    val neg : form -> form (* !!! *)
    val subst : form -> string -> EXP.exp -> form
    val toString : form -> string
end

signature IMPERATIVE = sig
    structure EXP : EXPRESSION

    datatype program = unkn | skip | cons of program * program 
        | if_then_else of EXP.exp * program * program | while_do of program * program 
        | var_is_in of string * program * program | assign of string * EXP.exp
    
    val toString : program -> string
end