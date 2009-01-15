(* Test compute_dps with some subterms *)

open Reecriture;;

let test_print () =
  print_string "** Test print.\n";
  print_string "* Ref: + (X0, X1)\n";
  print_term (Term ("+", [Var 0; Var 1]));
  print_newline ();

  print_string "* Ref: + (- (X0, X1, X2), X1)\n";
  print_term (Term ("+", [Term ("-", [Var 0; Var 1; Var 2]); Var 1]));
  print_newline ();
  print_newline ();
;;

let test_system sys =
  print_system sys; print_newline ();

  print_string "D: ";
  print_symblist print_str_ws (compute_symbols sys); print_newline ();

  print_string "DPs:\n";
  print_dps (compute_dps sys);
  print_newline (); print_newline (); print_newline ()
;;

let test_compute_dps () =
  print_string "** Test compute_dps.\n";

  print_string "* Empty system.\n";
  (let sys = []
  in print_dps (compute_dps sys));
  print_newline ();

  print_string "* System 7.3.\n";
  test_system Examples.system_7_3;

  print_string "* System 7.11.\n";
  test_system Examples.system_7_11;


  print_newline ();
;;

test_print ();
test_compute_dps ()
