# HoareHelp

A small interactive proof helper for Hoare logic over an IMP-style while language.

It parses a program, asks for its precondition and postcondition, then guides a manual derivation using Hoare rules.

## Requirements

- MLton

## Commands

```sh
mlton -default-ann 'allowExtendedTextConsts true' \
-default-ann 'allowOrPats true' \
HoareHelp.mlb

./HoareHelp
```

## Language sketch

Programs use assignments, sequencing, conditionals, and loops:

```text
x := 1
x := x + 1; y := x * 2
if x > 0 then y := 1 else y := 0
while x > 0 do (x := x - 1)
```

Assertions support `TRUE`, `FALSE`, comparisons (`<`, `=`, `>`), `not`, `and`, `or`, and implication (`=>`). Arithmetic expressions support `+`, `-`, `*`, `/`, parentheses, and `exp(base, exponent)`.

## Example: repeated-subtraction division

This example proves that the program computes the Euclidean division.

```text
(* x is the dividend and y is the positive divisor. *)
q := 0; (* quotient *)
r := x; (* remainder *)
while (y < r or y = r) do (
  r := r - y;
  q := q + 1
)
ENDPROGRAM
```

The program accepts SML-style comments.

`ENDPROGRAM` is a delimiter.

### Precondition and Postcondition

Enter these conditions when prompted:

```text
(x > 0 or x = 0) and y > 0
```

and then

```text
x = y * q + r and (r > 0 or r = 0) and r < y
```

They mean:

```math
P \equiv x \geq 0 \land y > 0
\qquad\text{and}\qquad
Q \equiv x = yq + r \land 0 \leq r < y.
```

Thus, the target is:

```math
\{P\}\ \texttt{q := 0; r := x; while } y \leq r\ \texttt{do } B\ \{Q\},
```

where $B$ is `r := r - y; q := q + 1`.

Note: The parser has no `<=` token, so the loop inequality is written as `y < r or y = r`.

### Step-by-step proof

1. Start the program.

```sh
./HoareHelp
```

2. At `Please input a program ...`, enter the program:

```text
(* x is the dividend and y is the positive divisor. *)
q := 0; (* quotient *)
r := x; (* remainder *)
while (y < r or y = r) do (
  r := r - y;
  q := q + 1
)
ENDPROGRAM
```

3. At `now please input a precondition ...`, enter:

```text
(x > 0 or x = 0) and y > 0
```

4. At `now please input a postcondition ...`, enter:

```text
x = y * q + r and (r > 0 or r = 0) and r < y
```

5. At the first `Current subgoal`, containing the complete program, enter:

```text
COMPOSITION
```

This applies:

```math
[\mathrm{Composition}]
\quad
\dfrac{
\{P\}\ C_1\ \{I\}
\qquad
\{I\}\ C_2\ \{Q\}
}{
\{P\}\ C_1;C_2\ \{Q\}
}
```

From now on, we must prove $\lbrace P \rbrace\ C_1\ \lbrace I \rbrace$ and $\lbrace I \rbrace\ C_2\ \lbrace Q \rbrace$. The prover works backwards, so it first asks us to prove $\lbrace I \rbrace\ C_2\ \lbrace Q \rbrace$. At the next step, we provide the precondition $I$ for the loop.

6. At the next `Current subgoal`, containing only the `while` loop and `Pre-Condition : { ??? }`, enter:

```text
HELP
```

7. At `Please input a plausible precondition for the current step:`, enter:

```text
x = y * q + r and (r > 0 or r = 0) and y > 0
```

This is the loop invariant:

```math
I \equiv x = yq + r \land 0 \leq r \land y > 0.
```

8. At the next `Current subgoal`, containing the `while` loop with invariant $I$, enter:

```text
WHILE
```

This applies:

```math
[\mathrm{While}]
\quad
\dfrac{
\{I \land G\}\ B\ \{I\}
}{
\{I\}\ \texttt{while }G\texttt{ do }B\ \{I \land \neg G\}
}
```

Here $G \equiv y \leq r$ and $B$ is the loop body.

9. At `Do you confirm that ... ≡ ...?`, enter:

```text
yes
```

This confirms the loop-exit condition. Since $0 \leq r < y$ implies $y > 0$, the exit assertion is equivalent to the postcondition:

```math
I \land \neg(y \leq r) \equiv I \land r < y \equiv Q.
```

10. At the next `Current subgoal`, containing `r := r - y; q := q + 1`, enter:

```text
STRENGTHENING
```

This applies the consequence rule:

```math
[\mathrm{Strengthening}]
\quad
\dfrac{
P \Longrightarrow P'
\qquad
\{P'\}\ C\ \{Q\}
}{
\{P\}\ C\ \{Q\}
}
```

11. At the repeated body subgoal, enter:

```text
COMPOSITION
```

12. At the next subgoal, containing `q := q + 1`, enter:

```text
ASSIGN
```

13. At the next subgoal, containing `r := r - y`, enter:

```text
ASSIGN
```

Both assignments use:

```math
[\mathrm{Assignment}]
\quad
\dfrac{
}{
\{R[E/z]\}\ z := E\ \{R\}
}
```

14. At `Do you confirm that ... ⊃ ...?`, enter:

```text
yes
```

This confirms invariant preservation:

```math
I \land (y \leq r) \Longrightarrow
\bigl(x = y(q+1) + (r-y) \land 0 \leq r-y \land y > 0\bigr).
```

We have now proved $\lbrace I \rbrace\ C_2\ \lbrace Q \rbrace$. We must now prove $\lbrace P \rbrace\ C_1\ \lbrace I \rbrace$.

15. At the next `Current subgoal`, containing `q := 0; r := x`, enter:

```text
COMPOSITION
```

Here $C_1$ is `q := 0; r := x`.

16. At the next subgoal, containing `r := x`, enter:

```text
ASSIGN
```

17. At the next subgoal, containing `q := 0`, enter:

```text
STRENGTHENING
```

18. At the repeated `q := 0` subgoal, enter:

```text
ASSIGN
```

19. At the final `Do you confirm that ... ⊃ ...?`, enter:

```text
yes
```

This confirms invariant initialisation:

```math
x \geq 0 \land y > 0 \Longrightarrow
\bigl(x = y\cdot0 + x \land x \geq 0 \land y > 0\bigr).
```
