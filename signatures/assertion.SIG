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
    val isMore : (ass) -> (EXP.exp * EXP.exp) option
end
