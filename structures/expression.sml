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

  fun normalize_sub (neg (k n) : exp) : exp = k (~n)
        | normalize_sub (times (k 1, exp) ) = normalize_sub exp
        | normalize_sub (times (exp, k 1) ) = normalize_sub exp

        | normalize_sub (plus (k 0, exp)) = normalize_sub exp
        | normalize_sub (plus (exp, k 0)) = normalize_sub exp

        | normalize_sub (times(k 0, _)) = k 0
        | normalize_sub (times(_, k 0)) = k 0
        | normalize_sub ( times (plus (exp1, exp2), exp3) ) = plus (times (normalize_sub exp1, normalize_sub exp3), 
                                                                    times (normalize_sub exp2, normalize_sub exp3) )
        | normalize_sub ( times (exp1, plus (exp2, exp3)) ) = plus (times (normalize_sub exp1, normalize_sub exp2), 
                                                                    times (normalize_sub exp1, normalize_sub exp3) )
        | normalize_sub ( times (times (exp1, exp2), exp3 ) ) = if exp1 = exp2
                                                                then times ( normalize_sub exp1, times (normalize_sub exp2, normalize_sub exp3) )
                                                                else times (times (normalize_sub exp1, normalize_sub exp2), normalize_sub exp3 )
        
        | normalize_sub (neg (plus (exp1, exp2))) = plus (neg (normalize_sub exp1), neg (normalize_sub
        exp2))
        | normalize_sub (neg (times (exp1, exp2))) = times (neg (normalize_sub exp1), normalize_sub exp2)
        | normalize_sub (neg (neg exp)) = normalize_sub exp
        | normalize_sub (neg (inv exp)) = inv (neg (normalize_sub exp))
        | normalize_sub (inv (k 1)) = k 1
        | normalize_sub (inv (k ~1)) = k ~1
        | normalize_sub (inv (times (exp1, exp2))) = times (normalize_sub exp1, inv (normalize_sub
        exp2))
        | normalize_sub (inv (inv exp)) = normalize_sub exp

        | normalize_sub (plus (exp1, exp2)) = plus ((normalize_sub exp1), (normalize_sub
        exp2))
        | normalize_sub (times (exp1, exp2)) = times ((normalize_sub exp1), (normalize_sub
        exp2))
        | normalize_sub (inv exp1) = inv (normalize_sub exp1)
        | normalize_sub (neg exp1) = neg (normalize_sub exp1)


        | normalize_sub exp = exp

    fun normalize exp = 
        let val res : exp = normalize_sub exp
        in
            if exp = res
            then exp
            else normalize res 
        end

    fun powerCount base e =
        case e of
               times(x, y) => if x = base then 1 + powerCount base y else
                 if y = base then 1 + powerCount base x else 0
             | _ => if e = base then 1 else 0

    fun kw s  = ANSI.Keyword ^ s ^ ANSI.reset
    fun id s  = ANSI.Identifier ^ s ^ ANSI.reset
    fun num s = ANSI.Number ^ s ^ ANSI.reset
    fun ope s = ANSI.Operator ^ s ^ ANSI.reset
    fun par s = ANSI.Delimiter ^ s ^ ANSI.reset

    fun intTok (n:int) : string =
      let val s = Int.toString n
      in if n >= 0 then num s else par "(" ^ num s ^ par ")" end

    fun enclose (e : exp) : string = let val str : string = toString_sub e
                                            in case e of
                                                plus (_, _) => "(" ^ str ^ ")"
                                                | _ => str end


    and toString_sub (k n : exp): string = intTok n
      | toString_sub (var x) = id x

      | toString_sub (plus (exp1, k i)) =
          if i > 0 then toString_sub exp1 ^ " " ^ ope "+" ^ " " ^ num (Int.toString i)
          else if i < 0 then toString_sub exp1 ^ " " ^ ope "-" ^ " " ^ num (Int.toString (~i))
          else toString_sub exp1

      | toString_sub (plus (exp1, neg exp2)) =
          toString_sub exp1 ^ " " ^ ope "-" ^ " " ^ toString_sub exp2

      | toString_sub (plus (exp1, exp2)) =
          toString_sub exp1 ^ " " ^ ope "+" ^ " " ^ toString_sub exp2

      | toString_sub (times (exp1, inv exp2)) =
          enclose exp1 ^ ope "/" ^ enclose exp2

      | toString_sub (times (base, rest)) =
          let
            val pow = powerCount base rest
          in
            if pow > 0
            then toString_sub base ^ ope "^" ^ num (Int.toString (pow + 1))
            else enclose base ^ ope "*" ^ enclose rest
          end

      | toString_sub (neg exp) =
          ope "-" ^ " " ^ par "(" ^ toString_sub exp ^ par ")"

      | toString_sub (inv exp) =
          num "1" ^ " " ^ ope "/" ^ " " ^ par "(" ^ toString_sub exp ^ par ")"


    fun toString exp = toString_sub (normalize exp)

end
