(* Projection related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Projection graph functions.                                              *
 ****************************************************************************)


let cst_proj n =
  fun symbl -> n
;;


let check_project () =
  let eq proj t1 t2 = eq_term (project proj t1) t2 in
  let chk p = metachk false print_term print_term (eq p) in

  printf "constant projection = 0@\n";
  chk (cst_proj 0) vX vX;
  chk (cst_proj 0) (succ vX) vX;
  chk (cst_proj 0) (plus vX vY) vX;
  chk (cst_proj 0) (Term ("?", [vX; vY; Var 2; Var 3])) vX;

  printf "constant projection = 1@\n";
  chk (cst_proj 1) (Term ("?", [vX; vY; Var 2; Var 3])) vY;

  printf "constant projection = 2@\n";
  chk (cst_proj 2) (Term ("?", [vX; vY; Var 2; Var 3])) (Var 2);

  printf "constant projection = 3@\n";
  chk (cst_proj 3) (Term ("?", [vX; vY; Var 2; Var 3])) (Var 3);
;;

let check_removable () =
  () (* FIXME: *)
;;

let check_gen_projs () =
  () (* FIXME: *)
;;

let check_check_proj_comp () =
  () (* FIXME: *)
;;

let check_find_component () =
  () (* FIXME: *)
;;

let check_check_proj () =
  () (* FIXME: *)
;;

let check_proj_list_to_fun () =
  () (* FIXME: *)
;;

let check_proj_fun_to_list () =
  () (* FIXME: *)
;;

let check_find_projection () =
  let eq np n rules (list2, dp2) = try
    let (list1, dp1) = find_projection
        rules
        (compute_graph (compute_symbols rules) (compute_dps rules))
        (compute_symbols rules)
        n in
    let res = List.for_all2
        (fun (s1, int1) (s2, int2) -> eq_string s1 s2 && int1 == int2)
        list1 list2 in
    res && dp1 == dp2
  with Invalid_argument _ -> false
  and print (l, dp) =
    printf "@[";
    printf "@]" in
  let chk np n = metachk false print_system print (eq np n) in


  (* FIXME: insert result. *)
  chk 0 5 system_7_3 ([], (vX, vX));
  chk 0 5 system_7_11 ([], (vX, vX));
  chk 0 5 system_7_19 ([], (vX, vX));
;;

(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_project, "project");
   (check_removable, "removable");
   (check_gen_projs, "generate projections");
   (check_check_proj_comp, "check compute projection");
   (check_find_component, "find component");
   (check_check_proj, "check projection");
   (check_proj_list_to_fun, "projection list to projection function");
   (check_proj_fun_to_list, "projection function to projection lit");
   (check_find_projection, "find projection")
 ] in
printf "Projection functions test suite.@\n";
exit (run_tests tests)
;;
