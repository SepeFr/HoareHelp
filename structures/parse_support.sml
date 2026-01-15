
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
      symb_chars = ":=<>=>",
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
        ["skip","if","then","else","while","do","TRUE","FALSE","inv", "and", "or"]

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
  (**
  * Assuming this grammar for parsing
  * exp := term ("+" term)
  * term := unary ("* or /" unary)
  * unary := "-" |  atom
  * atom := int | var | "(" exp ")"
  *)

    fun parse_atom () : Imp.ASS.EXP.exp p =
          (Imp.ASS.EXP.k <$> parse_int)
      <|> (Imp.ASS.EXP.var <$> parse_variable)
      <|> parens (delay parse_expression ())
    and
      parse_unary() : Imp.ASS.EXP.exp p =
        (*try to parse - as symbol; if it succedes then; apply the function
        * Imp.ASS.EXP.neg which is a constructor to parse_unary() in a lazy way*)
          (parse_symbol "-" *> (Imp.ASS.EXP.neg <$> delay parse_unary ()))
      (*<|> (parse_keyword "inv" *> parens (Imp.ASS.EXP.inv <$> delay parse_expression
      * ()))*)
      <|> parse_atom()
    and parse_term() : Imp.ASS.EXP.exp p =
      let
        val parse_choice : string p =
            (parse_symbol "*" *> accept "*")
        <|> (parse_symbol "/" *> accept "/")
          (* left -> ast built so far; right is the next Imp.ASS.EXP.exp and _ is the
          * operator*)
        fun combine (left, (operator, right)) =
          case operator of
               "*" => Imp.ASS.EXP.times(left, right)
             | "/" => Imp.ASS.EXP.times(left, Imp.ASS.EXP.inv right)
             | _ => left
      in
          ((delay parse_unary ()) ??* (parse_choice >>> delay parse_unary ())) combine
      end
    and parse_expression() : Imp.ASS.EXP.exp p =
      let
        fun combine (left, (_, right)) = Imp.ASS.EXP.plus(left, right)
      in
        ((delay parse_term()) ??* ((parse_symbol "+" *> accept "+") >>> delay
        parse_term ())) combine
      end

    fun parseExpString (s: string) : Imp.ASS.EXP.exp option =
      let
        val ts = fromString s
      in
        case Parser.parse (delay parse_expression () <* eof) ts of
             OK e => SOME e
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end

  fun parsePrintE (s: string) : Imp.ASS.EXP.exp option =
    let
      val ts = fromString s
    in
      case Parser.parse (delay parse_expression () <* eof) ts of
        OK e =>
          (print ("OK: " ^ Imp.ASS.EXP.toString e ^ "\n");
           SOME e)
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end

    (* part of ass *)

   (*
      prop := TRUE | FALSE | exp "<" exp | exp "=" exp | "(" ass ")" | exp ">"
      exp
      ass = prop | (ass and ass) | (ass or ass) | (ass -> ass)
   * *)

  (* structure A = Ass *)

    fun parse_prop () : Imp.ASS.ass p =
          (parse_keyword "TRUE" *> accept Imp.ASS.t)
      <|> (parse_keyword "FALSE" *> accept Imp.ASS.f)
      <|> (delay parse_expression () >>= (fn e1 =>
            (parse_symbol "<" *> delay parse_expression () >>= (fn e2 => accept (Imp.ASS.less(e1, e2))))
        <|> (parse_symbol ">" *> delay parse_expression () >>= (fn e2 => accept
        (Imp.ASS.more e1 e2)))
        <|> (parse_symbol "=" *> delay parse_expression () >>= (fn e2 => accept (Imp.ASS.eq(e1, e2))))))

      <|> parens (delay parse_ass ())
    and parse_ass() : Imp.ASS.ass p =
      let
        val parse_choice : string p =
            (parse_keyword "and" *> accept "and")
        <|> (parse_keyword "or" *> accept "or")
        <|> (parse_symbol "=>" *> accept "=>")

        fun combine (left, (operator, right)) =
          case operator of
               "and" => Imp.ASS.andd left right
             | "or" => Imp.ASS.orr left right
             | "=>" => Imp.ASS.imply(left, right)
             | _ => left
      in
        (delay parse_prop () ??* (parse_choice >>> delay parse_prop ())) combine
      end

    fun parseAssString (s: string) : Imp.ASS.ass option =
      let
        val ts = fromString s
      in
        case Parser.parse (delay parse_ass() <* eof) ts of
             OK e => SOME e
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end


  fun parsePrintA (s: string) : Imp.ASS.ass option =
    let
      val ts = fromString s
    in
      case Parser.parse (delay parse_ass() <* eof) ts of
        OK e =>
          (print ("OK: " ^ Imp.ASS.toString e ^ "\n");
           SOME e)
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end

    (* part of imp *)
   (*
      prog := stmt (";" stmt)*

      stmt := skip | x := exp | if ass then prog else prog | while ass do prog |
      "(" + prog + ")"
   * *)


    fun parse_program () : Imp.program p =
      let
        fun combine (left, (_, right)) = Imp.cons(left, right)
      in
        (delay parse_imperative () ??* ( (parse_symbol ";" *> accept ";") >>>
        delay parse_imperative ())) combine
      end
    and parse_imperative() : Imp.program p =
          (parse_keyword "skip" *> accept Imp.skip)
      <|> (Imp.while_do <$>
            (( parse_keyword "while" *> delay parse_ass () ) >>>
            (parse_keyword "do" *> delay parse_program () ))
          )
      <|> (parse_keyword "if" *> delay parse_ass () >>= ( fn cond =>
            parse_keyword "then" *> delay parse_program () >>= ( fn p1 =>
            parse_keyword "else" *> delay parse_program () >>= ( fn p2 =>
            accept (Imp.if_then_else(cond, p1,p2))))))
      <|> ( (parse_variable <* parse_symbol ":=") >>= (fn var =>
            delay parse_expression () >>= ( fn exp =>
              accept (Imp.assign(var, exp)))))
      <|> (parens (delay parse_program ()))


    fun parseImpString (s: string) : Imp.program option =
      let
        val ts = fromString s
      in
        case Parser.parse (delay parse_program() <* eof) ts of
             OK e => SOME e
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end


  fun parsePrintI (s: string) : Imp.program option =
    let
      val ts = fromString s
    in
      case Parser.parse (delay parse_program() <* eof) ts of
        OK e =>
          (print ("OK: " ^ Imp.toString e ^ "\n");
           SOME e)
      | NO (loc, msg) =>
          (print (Region.ppLoc loc ^ ": " ^ msg() ^ "\n");
           NONE)
    end
end
