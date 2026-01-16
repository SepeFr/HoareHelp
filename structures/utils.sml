structure Utils :> UTILS = struct
    fun trim_space str =
        String.implode (List.filter (fn c : char => not (Char.isSpace c)) (String.explode str))
end
