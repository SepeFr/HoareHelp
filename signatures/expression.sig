signature EXPRESSION = sig
    datatype exp = k of int | var of string
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp (* ceil / floor / mod ...*)

    val expn : exp -> int -> exp
    val subst : exp -> string -> exp -> exp
    val toString : exp -> string
end
