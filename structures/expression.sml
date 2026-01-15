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



structure ParseSupport =
struct
  structure T = SimpleToken

  fun isNum s =
    List.all Char.isDigit (String.explode s)

  fun isId str =
    size str > 0 andalso
    let val char_0 = String.sub(str,0) in
      Char.isAlpha char_0 orelse char_0 = #"_"
    end andalso List.all (fn char_i => Char.isAlphaNum char_i orelse char_i = #"_") (String.explode str)


  (* The library makes a difference between two types of tokens *)
  (* sep_char -> separation characters that are single *)
  (* symb_chars -> separation characters that may be concatenated to form new*)
  (* meaninng eg (`>=` is the combination of two symbol chars `>` and `=`) *)
  (* is_num : ie. what i consider a num in my implementation *)
  (* is_id : ie. what i consider an identifies (variables) *)
  val tokenise =
    T.tokenise {
      sep_chars = "();+-/*",
      symb_chars = ":=<>=",
      is_num = isNum,
      is_id = isId
    }

  fun fromString input =
    tokenise { srcname = "stdin", input = input }


  structure Parser = Parse(type token = T.token
                      val pp_token = T.pp_token)
  open Parser

  infixr >>= <|> *> <*
  infix >>> ?? ??? ??* <*> <$> <$$>

  fun quoting str = "'" ^ str ^ "'"

  fun parse_symbol (str : string) : unit p =
    next >>= (fn tok => case tok of T.Symb tok_str => if tok_str = str then accept() else
      reject ("Expecting Symbol " ^ quoting str) | _ => reject ("Expecting Symbol"
      ^ quoting str))

  fun parse_keyword (str : string ) : unit p =
    next >>= (fn tok => case tok of T.Id tok_str => if tok_str = str then
      accept() else reject ("Expecting Identifier" ^ quoting str) | _ => reject
      ("Expecting Identifier" ^ quoting str))

  val reserved_words =
        ["skip","if","then","else","while","do","TRUE","FALSE","inv"]

  val parse_variable : string p =
    next >>= (fn tok =>
      case tok of
          T.Id tok_str =>
            if List.exists(fn word => word = tok_str ) reserved_words
            then reject ("reserved word " ^ quoting tok_str ^ " cannot be a variable")
            else accept tok_str
          | _=> reject "expecting identifier")

  val parse_int : int p =
    next >>= (fn tok =>
      case tok of
          T.Num tok_str =>
            (
              case Int.fromString tok_str of SOME number => accept number | NONE =>
                reject "expecting int"
            )
          | _=> reject "expecting int ")

  fun parens p = enclose (parse_symbol "(") (parse_symbol ")") p
  fun braces p = enclose (parse_symbol "{") (parse_symbol "}") p

  (*EXP PARSER -> EXP*)
  structure E = Exp

  (**
  * Assuming this grammar for parsing
  * exp := term ("+" term)
  * term := unary ("*" unary)
  * unary := "-" | "inv" "(" exp ")" | atom
  * atom := int | var | "(" exp ")"
  *)

    fun parse_atom () : E.exp p =
          (E.k <$> parse_int)
      <|> (E.var <$> parse_variable)
      <|> parens (delay parse_expression ())
    and
      parse_unary() : E.exp p =
        (*try to parse - as symbol; if it succedes then; apply the function
        * E.neg which is a constructor to parse_unary() in a lazy way*)
          (parse_symbol "-" *> (E.neg <$> delay parse_unary ()))
      <|> (parse_keyword "inv" *> parens (E.inv <$> delay parse_expression ()))
      <|> parse_atom()
    and parse_term() : E.exp p =
      let
          (* left -> ast built so far; right is the next E.exp and _ is the
          * operator*)
        fun combine (left, (_, right)) = E.times(left, right)
      in
        ((delay parse_unary ()) ??* ((parse_symbol "*" *> accept "*") >>> delay parse_unary ())) combine
      end
    and parse_expression() : E.exp p =
      let
        fun combine (left, (_, right)) = E.plus(left, right)
      in
        ((delay parse_term()) ??* ((parse_symbol "+" *> accept "+") >>> delay
        parse_term ())) combine
      end

    fun parseExpString (s: string) : Exp.exp option =
      let
        val ts = fromString s
      in
        case Parser.parse (delay parse_expression () <* eof) ts of
             OK e => SOME e
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end

  fun parsePrintE (s: string) : Exp.exp option =
    let
      val ts = fromString s
    in
      case Parser.parse (delay parse_expression () <* eof) ts of
        OK e =>
          (print ("OK: " ^ Exp.toString e ^ "\n");
           SOME e)
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end

end
