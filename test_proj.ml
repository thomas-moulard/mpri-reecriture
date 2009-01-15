(* Projection test *)

(*
val project : (string -> int) -> term -> term

val strict_subterms : term -> term list;;
val subterms : term -> term list;;
val removable : system -> (string -> int) -> dp -> int -> can_be_removed


val find_projection : system -> graph -> symb list -> int -> ((symb * int) list * dp)
*)

open Reecriture;;
open Examples;;

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
