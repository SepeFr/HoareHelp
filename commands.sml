use "lib/github.com/diku-dk/sml-parse/REGION.sig";
use "lib/github.com/diku-dk/sml-parse/Region.sml";

use "lib/github.com/diku-dk/sml-parse/PARSE.sig";
use "lib/github.com/diku-dk/sml-parse/Parse.sml";

use "lib/github.com/diku-dk/sml-parse/SIMPLE_TOKEN.sig";
use "lib/github.com/diku-dk/sml-parse/SimpleToken.sml";

use "signatures/utils.sig";
use "structures/utils.sml";

use "signatures/expression.sig";
use "structures/expression.sml";

use "signatures/assertion.sig";
use "structures/assertion.sml";

use "signatures/imperative.sig";
use "structures/imperative.sml";


use "structures/parse_support.sml";



List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore
(ParseSupport.parsePrintI s))) [
"x := 9; while (x > 0 or x = 0) do (x := x + 1;  y := y * 2)",
"if (x > 0) then (while not(x = 0) do (x := x-1)) else x := 10"
                               ];

                          (*
let val out = ParseSupport.parsePrintA "(a > b)"
in
print (Imp.ASS.toString (Option.valOf out))
end
*)


(*List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore (ParseSupport.parsePrintE s))) [
"1", "x", "(y)", "exp(x,0)", "exp(1,1)", "exp(exp(x,2),3)", "-(1)", "-(-x)", "1+2", "a-b",
"x*y", "c/d", "1+2+3", "a*b*c", "1+2*3", "(1+2)*3", "1*2+3", "1-(2-3)", "1-2-3", "exp(x+y,2)",
"exp(2,exp(2,2))", "x+y*z/w-v", "-(x+y)", "-exp(x,2)", "exp(-x,2)", "((((x))))", "1*2/3*4",
"a/b/c", "a/(b/c)", "exp(x,1)+exp(y,1)", "100/10/2", "100/(10/2)", "x+0", "0*x", "x-x",
"exp(x,10)", "1+1+1+1+1", "a*a*a*a*a", "-(1+-(2+-(3)))", "exp(x,2)-y*z", "x*y+exp(z,3)",
"exp((x+1),2)", "exp(x,2+1)", "((1+2)*(3+4))", "x/y*z", "x/(y*z)", "exp(a,2)+exp(b,2)",
"x- -1", "x+ -y", "exp(exp(exp(x,2),2),2)", "1*1+1/1-1", "a+b-c+d-e", "((a+b)-c)", "a+(b-c)",
"exp(x,3)/exp(y,2)", "1+exp(x,2)*3", "(1+exp(x,2))*3", "exp(1+2*3,4)", "x*y/z+a-b",
"-(x*y/z)", "exp(x,2)*exp(y,2)", "1+2+3+4+5+6+7+8+9+10", "1*2*3*4*5", "exp(x,2+3*4)",
"x-y-z-w", "x-(y-(z-w))", "exp(x,2)+(y-z)*exp(a,3)", "1/(2/(3/4))", "1/2/3/4", "-(exp(x,y))",
"exp(-1,-1)", "exp(x,2)+exp(y,2)+exp(z,2)", "a*b+c*d+e*f", "(a*b)+(c*d)", "x+exp(y,3)-z/2",
"exp(x,2)*y-z/exp(w,4)", "1+2*3-4/5+exp(x,6)", "((x))+(y)", "exp(x,1)*exp(x,1)",
"x*x*x*x*x*x*x*x*x*x", "exp(exp(x,2)+y,2)", "1-2+3-4+5-6", "a/(b*c*d)", "exp(1,2)+3",
"4*exp(x,5)", "exp(x,2)- -exp(y,2)", "((1))", "x+y+z+w+a+b+c", "exp(x,2)/2", "x*exp(y,2)",
"-(a-b)", "exp(x,2+exp(y,2))", "1+1-1+1-1+1-1", "a*b/a*b", "exp(x,2)/exp(x,2)", "x-0+y*1",
"exp(x,2)+0", "1*x+y/1", "exp(x,5)*exp(x,5)", "exp(x,100)", "x * x * x * y"
];*)

(*
val test_cases = [
    (* --- Basic Statements & Assignments --- *)
    "skip",
    "x := 1",
    "y := 0",
    "long_variable_name := 999",
    "_underscore_var := 42",
    "x := y",
    "x := (1 + 2)",
    "x := x + 1; y := x",
    "a := 1; b := 2; c := 3",
    "skip; skip; skip",

    (* --- Arithmetic Expressions (EXP) --- *)
    "x := 1 + 2 * 3",
    "x := (1 + 2) * 3",
    "x := 10 / 2 - 1",
    "x := exp(y, 2)",
    "x := exp(2, 3) + exp(3, 2)",
    "x := -5 + 10",
    "x := -(x + y)",
    "x := exp(exp(x, 2), 3)",
    "x := 1 + 2 + 3 + 4 + 5",
    "x := 1 * 2 * 3 * 4 * 5",
    "x := a - b - c",
    "x := a - (b - c)",
    "x := x / y / z",
    "x := x / (y / z)",
    "x := exp(x + y, 10)",
    "x := exp(2, 0)",
    "x := 0 * x + y / 1",
    "x := ((((x))))",
    "x := - - - x",
    "x := 100 * (x + y) / exp(z, 2)",

    (* --- Boolean Assertions (ASS) --- *)
    "if TRUE then skip else skip",
    "if FALSE then skip else skip",
    "while (x < 10) do x := x + 1",
    "while (x = 0) do skip",
    "if (x > y) then x := y else y := x",
    "if (x < y and y < z) then skip else skip",
    "if (x = 1 or x = 2) then skip else skip",
    "if not(x = 0) then x := 1 else skip",
    "if (x < y => y > x) then skip else skip",
    "while (TRUE and not(x = 0)) do x := x - 1",
    "if (exp(x, 2) > 100) then x := 10 else x := x + 1",
    "if (x = y and (y = z or z = a)) then skip else skip",
    "if (not(x < y) or (a = b)) then skip else skip",
    "while (FALSE) do skip",
    "if ((x = 1) => (y = 2)) then skip else skip",

    (* --- Control Flow & Nesting --- *)
    "if (x > 0) then (x := x - 1; y := y + 1) else skip",
    "while (x > 0) do (y := y * 2; x := x - 1)",
    "if (x = 0) then if (y = 0) then z := 1 else z := 2 else z := 3",
    "while (x < 10) do while (y < 10) do (y := y + 1; x := x + 1)",
    "if (x < 0) then skip else (while (x > 0) do x := x - 1)",
    "x := 1; if (x = 1) then x := 2 else x := 3; x := 4",
    "(x := 1; y := 2); (z := 3; w := 4)",
    "while (x > 0) do (if (x = 5) then y := 1 else y := 0; x := x - 1)",
    "if (TRUE) then (if (FALSE) then skip else skip) else skip",
    "while (x < 100) do (x := x + 1; if (x = 50) then skip else y := y + 1)",

    (* --- Complex Whitespace & Formatting --- *)
    "x:=1;y:=2",
    "while(x>0)do(x:=x-1)",
    "if TRUE then skip else skip",
    "x   :=   10   +   20",
    "while\n(x\n>\n0)\ndo\nskip",
    "x := 1; (* comment *) y := 2",
    "x := 1; (* multi \n line \n comment *) y := 2",

    (* --- Edge Cases & Stress --- *)
    "x := 0 - 1",
    "x := exp(x, 1)",
    "x := exp(1, 100)",
    "while (x = x) do skip",
    "if (not(not(not(TRUE)))) then skip else skip",
    "x := x + y + z + a + b + c + d + e + f + g",
    "if (x < 1) then x := 1 else if (x < 2) then x := 2 else if (x < 3) then x := 3 else skip",
    "x := 1; y := 2; z := 3; w := 4; a := 5; b := 6; c := 7",
    "while (x > 0) do (while (y > 0) do (while (z > 0) do skip))",
    "x := exp(y, exp(z, 2))",
    "if (1 < 2 and 2 < 3 and 3 < 4 and 4 < 5) then skip else skip",
    "x := (1); y := ((2)); z := (((3)))",
    "x := y * -1",
    "x := -exp(y, 2)",
    "if (x = 0) then (skip; skip) else (skip; skip)",
    "while (x > 0) do (x := x - 1; if (x = 0) then skip else skip; y := y + 1)",
    "x := 1 + 2 * 3 / 4 - 5 + exp(6, 7)",
    "if (TRUE or FALSE and TRUE) then skip else skip",
    "a := b; b := c; c := d; d := a",
    "if (x < y) then (if (y < z) then a := 1 else a := 2) else (if (x < z) then a := 3 else a := 4)",
    "while (not(x = 0)) do (x := x - 1; y := y + 1; z := z + 2)",
    "x := 123456789",
    "if (x = 1) then x := x else x := x",
    "while (x > 0) do (x := x - 1); y := 10",
    "x := 10; (y := 20; z := 30)",
    "if (x < 10) then (x := x + 1) else (x := x - 1)",
    "x := exp(1+1, 2)",
    "while (a > b or c < d and e = f) do skip",
    "x := - (1 + 2)",
    "if (x = 10) then skip else x := 10",
    "y := x / x; z := x - x",
    "if TRUE then (while FALSE do skip) else skip",
    "x := exp(x, 2) + exp(y, 2) - exp(z, 2)",
    "while (x > 0) do (x := x - 1; skip; skip; x := x + 0)",
    "if (x > 0) then (x := x - 1; if (x = 0) then skip else x := 1) else skip",
    "a := 1; a := 2; a := 3; a := 4; a := 5",
    "x := exp(10, 2) / 5 + 3",
    "while (not(TRUE)) do x := 1",
    "x := 0; while (x < 10) do (x := x + 1); skip",
    (*custom*)
    "while ((* COMMENTS !!!!!!!!!!!!!!*)TRUE and not(x = 0)) do (x := x - 1)"
];
*)
