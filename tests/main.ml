(* Generic functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check main function.                                                     *
 ****************************************************************************)

let rec print_graphs = function
  | [] -> ()
  | e::l -> print_graph e; print_graphs l
;;


let check_main () =
  let eq n rules list = try
    List.for_all2 eq_graph (main rules n) list
  with Invalid_argument _ -> false in
  let chk n = metachk false print_system print_graphs (eq n) in

  (* FIXME: insert result *)
  chk 5 system_7_3 [];
  chk 5 system_7_11 [];
  chk 5 system_7_19 [];
;;


(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_main, "check main")
 ] in
printf "Main function test suite.@\n";
exit (run_tests tests)
;;
