signature LOGIC = sig
    structure IMP : IMPERATIVE

    exception DerivationError of string

    val derive : IMP.program -> IMP.ASS.ass -> IMP.ASS.ass -> unit
end
