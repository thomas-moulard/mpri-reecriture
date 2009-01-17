(* Generic functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check swapping of the nth element of a list.                             *
 ****************************************************************************)

let check_swap_nth_child () =
  () (* FIXME: *)
;;


(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_swap_nth_child, "check list element swapping")
 ] in
printf "Generic functions test suite.@\n";
exit (run_tests tests)
;;
