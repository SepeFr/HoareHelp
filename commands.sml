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



List.app (fn s => (print ("Testing: " ^ s ^ "\n"); ignore (ParseSupport.parsePrintA s))) [ "TRUE", "FALSE", "1 < 2", "x = y", "((a+x) < (a+x))", "(TRUE and TRUE)", "(TRUE or FALSE)", "(TRUE => FALSE)", "(x = 1 and y = 2)", "(a < b or b < c)", "(a = 1 => b = 2)", "((a < b) and (b < c))", "((x = 0) or (x = 1))", "(FALSE and (FALSE or TRUE))", "(TRUE => (TRUE => TRUE))", "((a = 1) and (b = 1))", "((a < b) => (c < d))", "(((a = b)))", "(TRUE or (FALSE and TRUE))", "( (a+x) = (a+x) )", "(x < 5 => x < 10)", "(a = b => b = a)", "((a < b) and (b < c))", "(x = 0 => TRUE)", "(FALSE => (x = 1))", "((a < b) => (c < d))", "(TRUE => (TRUE or FALSE))", "((x < 0) or (x > 0))", "(TRUE and (TRUE and (TRUE and TRUE)))", "(FALSE or (FALSE or (FALSE or FALSE)))", "((a = b) => (c = d))", "( (x + y) < (z * w) )", "( (1 + 2 + 3) = 6 )", "( (a + b) < (c + d) )", "( (x + 2) = 0 )", "( (2 * x + y) < 10 )", "( ((a + b) * (c + d)) < 100 )", "( x = (y + 1) )", "( (a = 0) and (b = 0) )", "( (x * x) = 4 )", "( (10 / 2) = 5 )", "( (x + 1) < (y + 1) )", "( (a + b) = c )", "( (a * b) = 0 )", "( (x + y + z) = 0 )", "( (1 + 1) = 2 )", "myVar < 10", "counter = 5", "a1 = b2", "long_name = short_name", "temp = (x + y)", "IsActive = TRUE", "InputVal < Threshold", "x_1 = y_1", "(Count = 0 or Count > 0)", "((a = b) and (b = c))", "(TRUE and FALSE)", "(a < b)", "a = b", "(TRUE or TRUE)", "(TRUE => TRUE)", "( (a+x) < 10 )", "( 10 = (a+x) )", "(TRUE and (x = y))", "( (x = y) or FALSE )", "( (a < b) => TRUE )", "( FALSE => (a < b) )", "( (1+1=2) and (2+2=4) )", "( (x < y) or (y < x) )", "( (x = 0) => (y = 0) )", "( (a = 1) and (b = 2) )", "( (a = 1) or (b = 2) )", "( (a = 1) => (b = 2) )", "( (x < y) and (y < z) )", "( (x < y) or (y < z) )", "( (x < y) => (y < z) )", "( (a = b) and (c = d) )", "( (a = b) or (c = d) )", "( (a = b) => (c = d) )", "( (TRUE and TRUE) and TRUE )", "( TRUE and (TRUE and TRUE) )", "( (FALSE or FALSE) or FALSE )", "( FALSE or (FALSE or FALSE) )", "( (TRUE => TRUE) => TRUE )", "( TRUE => (TRUE => TRUE) )", "( (a < b) and (c < d) )", "( (a < b) or (c < d) )", "( (a < b) => (c < d) )", "( (x = y) and (y = z) )", "( (x = y) or (y = z) )", "( (x = y) => (y = z) )", "( (a+b < c) and (c < d+e) )", "( (a+b = c) or (c = d+e) )", "( (a+b < c) => (c < d+e) )", "( (TRUE and FALSE) or (FALSE and TRUE) )", "( (TRUE or FALSE) and (FALSE or TRUE) )", "( (TRUE => FALSE) => (FALSE => TRUE) )", "( ((a=b)) and ((c=d)) )", "( (a=b) => ((c=d) or (e=f)) )", "( (a < b) and ( (b < c) and (c < d) ) )" ];
