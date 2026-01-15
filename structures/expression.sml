structure Exp :> EXPRESSION = struct
    datatype exp = k of int | var of string
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp

    fun expn _ 0 = k 1
        | expn exp 1 = exp
        | expn exp n = times(exp, expn exp (n-1))

    fun subst (k n) _ _ = k n
        | subst (var s) name new = if s = name then new else var s
        | subst (plus (exp1, exp2)) name new = plus (subst exp1 name new, subst exp2 name new)
        | subst (times (exp1, exp2)) name new = times (subst exp1 name new, subst exp2 name new)
        | subst (neg exp) name new = neg (subst exp name new)
        | subst (inv exp) name new = inv (subst exp name new)

  fun normalize (neg (k n)) = k (0 - n)
        | normalize (neg (plus (exp1, exp2))) = plus (neg exp1, neg exp2)
        | normalize (neg (times (exp1, exp2))) = times (neg exp1, exp2)
        | normalize (neg (neg exp)) = exp
        | normalize (neg (inv exp)) = inv (neg exp)
        | normalize (inv (times (exp1, exp2))) = times (exp1, inv exp2)
        | normalize (inv (inv exp)) = exp
        | normalize exp = exp

    fun enclose (e : exp) : string = let val str : string = toString_sub e
                                            in case e of
                                                plus (_, _) => "(" ^ str ^ ")"
                                                | _ => str end
    and toString_sub (k n : exp): string = let val str : string = Int.toString n
                                            in if n >= 0 then str else "(" ^ str ^ ")" end
        | toString_sub (var x) = x
        | toString_sub (plus (exp1, exp2)) = toString_sub exp1 ^ " + " ^ toString_sub exp2
        | toString_sub (times (exp1, exp2)) = enclose exp1 ^ " * " ^ enclose exp2
        | toString_sub (neg exp) = "- (" ^ toString_sub exp ^ ")"
        | toString_sub (inv exp) = "1 / (" ^ toString_sub exp ^ ")"

    fun toString exp = toString_sub (normalize exp)

    fun parse _ = NONE (* da implementare *)
end