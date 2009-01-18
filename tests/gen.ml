(* Generic functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check swapping of the nth element of a list.                             *
 ****************************************************************************)

let check_swap_nth_child () =
  let eq n value l1 l2 =
    try
      List.for_all2 (==) (swap_nth_child l1 n value) l2
    with Invalid_argument _ -> false
  and print = print_list print_int_ws in
  let chk n value = metachk false print print (eq n value) in

  printf "With n = -1:@\n";
  chk (-1) 0 [] [];
  chk (-1) 0 [0] [0];
  chk (-1) 0 [0;1] [0;1];

  printf "With n = 0, val = 99:@\n";
  chk 0 99 [0] [99];
  chk 0 99 [0;1] [99;1];

  printf "With n = 2, val = 99:@\n";
  chk 2 99 [0;1;2] [0;1;99];
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
