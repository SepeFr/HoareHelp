structure Exp :> EXPRESSION = struct
    datatype exp = k of int | var of string 
        | plus of exp * exp | times of exp * exp
        | neg of exp | inv of exp

    fun expn _ 0 = k 1
        | expn exp 1 = exp
        | expn exp n = times(exp, expn exp (n-1))

    fun subst (k n) _ _ = k n 
        | subst (var s) name new = if String.compare (s, name) = EQUAL 
                                        then new else var s
        | subst (plus (exp1, exp2)) name new = plus (subst exp1 name new, subst exp2 name new)
        | subst (times (exp1, exp2)) name new = times (subst exp1 name new, subst exp2 name new)
        | subst (neg exp) name new = neg (subst exp name new)
        | subst (inv exp) name new = inv (subst exp name new)

    fun toString _ = "welp" (* da implementare *)
end

structure Ass :> ASSERTION = struct
    structure EXP = Exp

    datatype ass = t | f | imply of ass * ass
        | less of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp

    fun not ass = imply (ass, f)

    fun orr ass1 ass2 = imply (not ass1, ass2)

    fun andd ass1 ass2 = not (imply (ass1, not ass2))
    
    fun more exp1 exp2 = not (orr (less (exp1, exp1)) (eq (exp1, exp2)))

    fun subst t _ _ = t
        | subst f _ _ = f
        | subst (imply (ass1, ass2)) name new = imply (subst ass1 name new, subst ass2 name new)
        | subst (less (exp1, exp2)) name new = less (EXP.subst exp1 name new, EXP.subst exp2 name new)
        | subst (eq (exp1, exp2)) name new = eq (EXP.subst exp1 name new, EXP.subst exp2 name new)

    fun toString _ = "welp" (* da implementare *)
end

structure Imp :> IMPERATIVE = struct
    structure ASS = Ass

    datatype program = skip | cons of program * program 
        | if_then_else of ASS.EXP.exp * program * program | while_do of ASS.ass * program 
        | var_is_in of string * program * program | assign of string * ASS.EXP.exp
    
    fun toString _ = "welp" (* da implementare *)
end