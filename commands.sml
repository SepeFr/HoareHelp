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

List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore
(ParseSupport.parsePrintA s))) [
"(a > b)"
                               ];

List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore
(ParseSupport.parsePrintA s))) [
"(a > b)"
                               ];


