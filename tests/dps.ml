(* Dependencies pairs related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check dependencies pairs computation.                                    *
 ****************************************************************************)


let check_compute_dps () =
  let eq rules dpl =
    try
      List.for_all2 eq_dp (compute_dps rules) dpl
    with Invalid_argument _ -> false in
  let chk = metachk false print_system print_dps eq in

  chk [] [];
  chk system_7_3 [(plus vX (succ vY), plus vX vY)];

  chk system_7_11
    [
     (minus (succ vX) (succ vY), minus vX vY);
     (div (succ vX) (succ vY), div (minus vX vY) (succ vY));
     (div (succ vX) (succ vY), minus vX vY);
   ];

  chk system_7_19
    [
     (intlist (append vX vY), intlist vY);
     (int zero (succ vY), int (succ zero) (succ vY));
     (int (succ vX) (succ vY), intlist (int vX vY));
     (int (succ vX) (succ vY), int vX vY)
   ];
;;


(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_compute_dps, "compute dependencies pairs")
 ] in
printf "Dependencies pairs functions test suite.@\n";
exit (run_tests tests)
;;
