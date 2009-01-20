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

  let g = make_empty_graph 1 in
  g.mat.(0).(0) <- -1;
  chk 5 system_7_3 [g];

  let g = make_empty_graph 3 in
  g.mat.(0).(0) <- -1; g.mat.(0).(1) <- -1; g.mat.(0).(2) <- -1;
  g.mat.(1).(0) <- -1; g.mat.(1).(1) <-  1; g.mat.(1).(2) <-  1;
  g.mat.(2).(0) <- -1; g.mat.(2).(1) <-  1; g.mat.(2).(2) <-  1;

  chk 5 system_7_11 [g];

  let g = make_empty_graph 4 in
  g.mat.(0).(0) <-  1; g.mat.(0).(1) <-  1; g.mat.(0).(2) <-  1; g.mat.(0).(3) <- -1;
  g.mat.(1).(0) <-  1; g.mat.(1).(1) <-  1; g.mat.(1).(2) <-  1; g.mat.(1).(3) <- -1;
  g.mat.(2).(0) <-  1; g.mat.(2).(1) <-  1; g.mat.(2).(2) <-  1; g.mat.(2).(3) <- -1;
  g.mat.(3).(0) <- -1; g.mat.(3).(1) <- -1; g.mat.(3).(2) <- -1; g.mat.(3).(3) <- -1;
  chk 5 system_7_19 [g];
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
