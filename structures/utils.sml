structure Utils :> UTILS = struct
    fun trim_space str =
        String.implode (List.filter (fn c : char => c <> #" ") (String.explode str))
    fun trim_newline str =
        String.implode (List.filter (fn c : char => c <> #"\n") (String.explode str))
end

structure Utils :> UTILS = struct
    fun trim_space str =
        String.implode (List.filter (fn c : char => c <> #" ") (String.explode str))
    fun trim_newline str =
        String.implode (List.filter (fn c : char => c <> #"\n") (String.explode str))
end

structure ANSI =
struct
  val reset = "\027[0m"

  (* Primary Palette: Nordic Slate *)
  val gray    = "\027[38;5;246m" (* muted cool gray *)
  val ice     = "\027[38;5;153m" (* soft frosty blue *)
  val leaf    = "\027[38;5;150m" (* muted sage green *)
  val sand    = "\027[38;5;223m" (* soft cream/tan *)
  val rose    = "\027[38;5;174m" (* muted dusty rose *)
  val dusk    = "\027[38;5;146m" (* muted lavender gray *)
  val mint    = "\027[38;5;116m" (* soft seafoam *)

  (* Semantic Colors *)
  val red     = "\027[38;5;167m" (* soft terracota *)
  val green   = "\027[38;5;108m" (* deep sage *)
  val sky     = "\027[38;5;110m" (* calm steel blue *)

  (* Logical Mappings *)
  val Keyword    = ice
  val Identifier = leaf
  val Number     = sand
  val Operator   = rose
  val Delimiter  = dusk
  val Boolean    = mint

  val Error      = red
  val Caret      = sand
  val Location   = sky
  val Message    = dusk
end

fun kw s  = ANSI.Keyword ^ s ^ ANSI.reset
fun id s  = ANSI.Identifier ^ s ^ ANSI.reset
fun num s = ANSI.Number ^ s ^ ANSI.reset
fun ope s = ANSI.Operator ^ s ^ ANSI.reset
fun par s = ANSI.Delimiter ^ s ^ ANSI.reset
fun bol s = ANSI.Boolean ^ s ^ ANSI.reset

fun sky s = ANSI.sky ^ s ^ ANSI.reset
fun gray s = ANSI.gray ^ s ^ ANSI.reset
fun err s = ANSI.Error ^ s ^ ANSI.reset

fun dusk s = ANSI.dusk ^ s ^ ANSI.reset

fun bol s = ANSI.Boolean ^ s ^ ANSI.reset
fun sky s = ANSI.sky ^ s ^ ANSI.reset

structure QED =
struct
  val colors = [ANSI.red, ANSI.sand, ANSI.green, ANSI.sky, ANSI.ice, ANSI.rose]

  fun rainbow_qed() =
  let
    val text = "Congratulations! You Successfully Concluded The Proof 𝔔.𝔈.𝔇."
    val text_length = size text
    val num_of_colors = length colors

    fun frame f =
      if f > 20 then ()
      else (
        TextIO.print "\r"; (* reset current line *)

        let
          fun loop i =
            if i >= text_length then ()
            else
              let
                val ch = String.substring(text, i, 1)
                val color = List.nth(colors,(i + f) mod num_of_colors)
              in
                TextIO.print (color ^ ch);
                loop (i + 1)
              end
        in
          loop 0 ;
          TextIO.print ANSI.reset;
          TextIO.flushOut TextIO.stdOut;
          OS.Process.sleep (Time.fromReal 0.150);
          frame (f + 1)
        end
      )
  in
    frame 0;
    TextIO.print "\n"
  end

end
