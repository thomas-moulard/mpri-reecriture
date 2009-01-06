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
  print_symblist (compute_dp_d sys); print_newline ();

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
  let zero = Term ("0", [])
  in
  (let sys = [
    (* x + 0 -> x *)
    (Term ("+", [Var 0; zero]), Var 0);
    (* x+ S(y) -> S(x+y) *)
    (Term ("+", [Var 0; Term ("S", [Var 1])]),
     Term ("S", [Term("+", [Var 0; Var 1])]))
  ]
  in test_system sys);

  print_string "* System 7.11.\n";
  let zero = Term ("0", [])
  in
  (let sys = [
    (* x - 0 -> x *)
    (
     Term ("-", [Var 0; zero]),
     Var 0
    );
    (* 0 / S(y) -> 0 *)
    (
     Term ("/", [zero; Term ("S", [Var 1])]),
     zero
    );
    (* S(x) - S(y) -> x - y *)
    (
     Term ("-", [Term ("S", [Var 0]); Term ("S", [Var 1])]),
     Term ("-", [Var 0; Var 1])
    );
    (* S(x) / S(y) -> S((x - y) / S(y)) *)
    (
     Term ("/", [Term ("S", [Var 0]); Term ("S", [Var 1])]),
     Term ("S", [Term ("/", [Term ("-", [Var 0; Var 1]); Var 1])])
    );
  ]
  in test_system sys);


  print_newline ();
;;

test_print ();
test_compute_dps ()
