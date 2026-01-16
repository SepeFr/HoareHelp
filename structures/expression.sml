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

  fun normalize (neg (k n)) = k (~n)
        | normalize (times (k 1, exp) ) = normalize exp
        | normalize (times (exp, k 1) ) = normalize exp

        | normalize (plus (k 0, exp)) = normalize exp
        | normalize (plus (exp, k 0)) = normalize exp

        | normalize (times(k 0, _)) = k 0
        | normalize (times(_, k 0)) = k 0


        | normalize (neg (plus (exp1, exp2))) = plus (neg (normalize exp1), neg (normalize
        exp2))
        | normalize (neg (times (exp1, exp2))) = times (neg (normalize exp1), normalize exp2)
        | normalize (neg (neg exp)) = normalize exp
        | normalize (neg (inv exp)) = inv (neg (normalize exp))
        | normalize (inv (k 1)) = k 1
        | normalize (inv (k ~1)) = k ~1
        | normalize (inv (times (exp1, exp2))) = times (normalize exp1, inv (normalize
        exp2))
        | normalize (inv (inv exp)) = normalize exp

        | normalize (plus (exp1, exp2)) = plus ((normalize exp1), (normalize
        exp2))
        | normalize (times (exp1, exp2)) = times ((normalize exp1), (normalize
        exp2))
        | normalize (inv exp1) = inv (normalize exp1)
        | normalize (neg exp1) = neg (normalize exp1)


        | normalize exp = exp

    fun powerCount base e =
        case e of
               times(x, y) => if x = base then 1 + powerCount base y else
                 if y = base then 1 + powerCount base x else 0
             | _ => if e = base then 1 else 0

    fun enclose (e : exp) : string = let val str : string = toString_sub e
                                            in case e of
                                                plus (_, _) => "(" ^ str ^ ")"
                                                | _ => str end

    and toString_sub (k n : exp): string = let val str : string = Int.toString n
                                            in if n >= 0 then str else "(" ^ str ^ ")" end
        | toString_sub (var x) = x
        | toString_sub (plus (exp1, k i)) = if i > 0 then toString_sub exp1 ^ " + " ^ Int.toString i
                                              else if i < 0 then toString_sub exp1 ^ " - " ^ Int.toString (~i)
                                                else  toString_sub exp1
        | toString_sub (plus (exp1, neg exp2)) = toString_sub exp1 ^ " - " ^ toString_sub exp2
        | toString_sub (plus (exp1, exp2)) = toString_sub exp1 ^ " + " ^ toString_sub exp2
        | toString_sub (times(exp1, inv exp2)) = enclose exp1 ^ "/" ^
        enclose exp2
(*        | toString_sub (times (exp1, exp2)) = enclose exp1 ^ " * " ^ enclose
*        exp2*)
        | toString_sub (times (base, rest)) =
          let
            val pow = powerCount base rest
          in
            if pow > 0
            then toString_sub base ^ "^" ^ Int.toString(pow +1)
            else enclose base ^ "*" ^ enclose rest
          end

        | toString_sub (neg exp) = "- (" ^ toString_sub exp ^ ")"
        | toString_sub (inv exp) = "1 / (" ^ toString_sub exp ^ ")"

    fun toString exp = toString_sub (normalize exp)
    
end
