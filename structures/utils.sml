structure Utils :> UTILS = struct
    fun trim_space str =
        String.implode (List.filter (fn c : char => not (Char.isSpace c)) (String.explode str))
end

structure ANSI =
struct
  val reset = "\027[0m"

  val black = "\027[30m"
  val red = "\027[31m"
  val green = "\027[32m"
  val yellow = "\027[33m"
  val blue = "\027[34m"
  val magenta = "\027[35m"
  val cyan = "\027[36m"
  val white = "\027[37m"
  val gray = "\027[90m"

  val gold = "\027[38;5;220m"
  val purple = "\027[38;5;141m"
  val pink = "\027[38;5;203m"
  val teal = "\027[38;5;80m"
  val lime = "\027[38;5;114m"
  val sky = "\027[38;5;81m"

  val Keyword = sky
  val Identifier = lime
  val Number = gold
  val Operator = pink
  val Delimiter = purple
  val Boolean = teal


  val Error     = red
  val Caret     = yellow
  val Location  = blue
  val Message   = magenta
end
