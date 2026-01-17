structure Ass :> ASSERTION = struct
    structure EXP = Exp

    datatype ass = t | f | imply of ass * ass
        | less of EXP.exp * EXP.exp | eq of EXP.exp * EXP.exp

    fun not ass = imply (ass, f)

    fun isNot (imply (ass, f) : ass) : ass option = SOME ass
        | isNot _ = NONE

    fun orr ass1 ass2 = imply (not ass1, ass2)

    fun isOrr (imply (ass1, ass2)) = ( case isNot ass1 of
                                                                    SOME assn => SOME (assn, ass2)
                                                                    | NONE => NONE )
        | isOrr _ = NONE

    fun andd ass1 ass2 = not (imply (ass1, not ass2))

    fun isAndd assn =
            case isNot assn of
                SOME (imply (ass1, ass2)) => ( case isNot ass2 of
                                                SOME ass3 => SOME (ass1, ass3)
                                                | NONE => NONE )
                | _ => NONE

    fun more exp1 exp2 = not (orr (less (exp1, exp2)) (eq (exp1, exp2)))

    fun isMore (ass : ass) : (EXP.exp * EXP.exp) option =
        ( case isNot ass of
            SOME assn => ( case isOrr assn of
                            SOME ((less (exp1, exp2)),
                                    (eq (exp3, exp4))) => if exp1 = exp3 andalso exp2 = exp4
                                                        then SOME (exp1, exp2) else NONE
                            | _ => NONE )
            | NONE => NONE )

    fun subst t _ _ = t
        | subst f _ _ = f
        | subst (imply (ass1, ass2)) name new = imply (subst ass1 name new, subst ass2 name new)
        | subst (less (exp1, exp2)) name new = less (EXP.subst exp1 name new, EXP.subst exp2 name new)
        | subst (eq (exp1, exp2)) name new = eq (EXP.subst exp1 name new, EXP.subst exp2 name new)


    fun normalize (ass : ass) : ass =
        ( case isAndd ass of
                SOME (assn, t) => assn
                | SOME (t, assn) => assn
                | SOME (f, _) => f
                | SOME (_, f) => f
                | SOME (ass1, ass2) => if ass1 = ass2
                                        then ass1 else andd ass1 ass2
                | NONE => ( case isOrr ass of
                            SOME (_, t) => t
                            | SOME (t, _) => t
                            | SOME (f, assn) => assn
                            | SOME (assn, f) => assn
                            | SOME (ass1, ass2) => if ass1 = ass2
                                        then ass1 else orr ass1 ass2
                            | NONE => ( case isNot ass of
                                            SOME assn => ( case isNot assn of
                                                            SOME assn2 => assn2
                                                            | NONE => not assn )
                                            | NONE => ( case ass of
                                                        eq (exp1, exp2) => if EXP.toString exp1 = EXP.toString exp2  
                                                            then t else eq (exp1, exp2)
                                                        | imply (t, assn) => normalize assn
                                                        | imply (_, t) => t
                                                        | imply (f, _) => t
                                                        | imply (ass1, ass2) => imply (normalize ass1, normalize ass2)
                                                        | c => c ))))

    fun toString_sub (ass : ass) : string =
        ( case isMore ass of
            SOME (exp1, exp2) => EXP.toString exp1 ^ " > " ^ EXP.toString exp2
            | NONE =>
                ( case isAndd ass of
                    SOME (ass1, ass2) => "( " ^ toString_sub ass1 ^ " ) ∧ ( " ^ toString_sub ass2 ^ " )"
                    | NONE =>
                        ( case isOrr ass of
                            SOME (ass1, ass2) => "( " ^ toString_sub ass1 ^ " ) ∨ ( " ^ toString_sub ass2 ^ " )"
                            | NONE =>
                                ( case isNot ass of
                                    SOME assn => "¬( " ^ toString_sub assn ^ " )"
                                    | NONE =>
                                        ( case ass of
                                            t => "TRUE"
                                            | f => "FALSE"
                                            | imply (ass1, ass2) => "( " ^ toString_sub ass1 ^ " ) ⊃ ( " ^ toString_sub ass2 ^ " )"
                                            | less (exp1, exp2) => EXP.toString exp1 ^ " < " ^ EXP.toString exp2
                                            | eq (exp1, exp2) => EXP.toString exp1 ^ " = " ^ EXP.toString exp2
                                            )))))

    fun toString ass = toString_sub (normalize ass)

end
