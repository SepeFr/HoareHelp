use "lib/github.com/diku-dk/sml-parse/REGION.sig";
use "lib/github.com/diku-dk/sml-parse/Region.sml";

use "lib/github.com/diku-dk/sml-parse/PARSE.sig";
use "lib/github.com/diku-dk/sml-parse/Parse.sml";

use "lib/github.com/diku-dk/sml-parse/SIMPLE_TOKEN.sig";
use "lib/github.com/diku-dk/sml-parse/SimpleToken.sml";

use "signatures/expression.SIG";
use "structures/expression.sml";

use "signatures/assertion.SIG";
use "structures/assertion.sml";

use "signatures/imperative.SIG";
use "structures/imperative.sml";

use "structures/parse_support.sml";



List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore
(ParseSupport.parsePrintI s))) [
"x := 9; while (x > 0 or x = 0) do (x := x + 1;  y := y * 2)"
                               ];

                          (*
let val out = ParseSupport.parsePrintA "(a > b)"
in
print (Imp.ASS.toString (Option.valOf out))
end
*)


List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore (ParseSupport.parsePrintE s))) [
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
"exp(x,2)+0", "1*x+y/1", "exp(x,5)*exp(x,5)", "exp(x,100)"
];
