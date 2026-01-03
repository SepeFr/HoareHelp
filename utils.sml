structure Utils :> UTILS = struct
    fun trim_space str =
        String.implode (List.filter (fn x : char => x <> #" ") (String.explode str))
end