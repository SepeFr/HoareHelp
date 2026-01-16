(* --- Parser library (external) --- *)
use "lib/github.com/diku-dk/sml-parse/REGION.sig";
use "lib/github.com/diku-dk/sml-parse/Region.sml";
use "lib/github.com/diku-dk/sml-parse/PARSE.sig";
use "lib/github.com/diku-dk/sml-parse/Parse.sml";
use "lib/github.com/diku-dk/sml-parse/SIMPLE_TOKEN.sig";
use "lib/github.com/diku-dk/sml-parse/SimpleToken.sml";

(* --- Utilities --- *)
use "signatures/utils.sig";
use "structures/utils.sml";

(* --- Core language definitions --- *)
use "signatures/expression.sig";
use "structures/expression.sml";

use "signatures/assertion.sig";
use "structures/assertion.sml";

use "signatures/imperative.sig";
use "structures/imperative.sml";

(* --- Parsing for IMP --- *)
use "structures/parse_support.sml";


(* --- Logic system (Hoare) --- *)
use "signatures/logic.sig";
use "logic.sml";

(* --- Run the Hoare proof assistant --- *)

Hoare.main();

(*ParseSupport.parsePrintI "x := 1; while (x < 10) do x := x + 1";*)
