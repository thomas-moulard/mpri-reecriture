(* Projection related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Projection graph functions.                                              *
 ****************************************************************************)

(*
let null_proj term =
  fun e -> 0
;;

let test_project term =
  let proj = null_proj term in
  print_string "\tTerm: ";
  print_term term;
  print_newline ();
  print_string "\tProjection:\n";
  print_proj proj (build_symblist term);
  print_newline ();
  print_string "\tProjected term: ";
  print_term (project proj term);
  print_newline ();
;;

let test_subterms () =
  ()
;;

let test_removable () =
  ()
;;

let test_find_proj () =
  ()
;;


print_string "* Test project (terms from system 7.3)\n";
List.iter test_project terms_7_3;

print_string "* Test subterms\n";
print_string "* Test removable\n";
print_string "* Test find_proj\n";
*)

let check_project () =
  () (* FIXME: *)
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
  () (* FIXME: *)
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
