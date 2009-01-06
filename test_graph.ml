(* Test graph generation *)

open Reecriture;;

let my_print_graph g =
  print_graph Format.std_formatter g
;;

let test_graph_init () =
  print_string "* Graph init (system 7.3)\n";
  let zero = Term ("0", [])
  in let sys = [
    (* x + 0 -> x *)
    (Term ("+", [Var 0; zero]), Var 0);
    (* x+ S(y) -> S(x+y) *)
    (Term ("+", [Var 0; Term ("S", [Var 1])]),
     Term ("S", [Term("+", [Var 0; Var 1])]))
  ]
  in my_print_graph (compute_graph (compute_symb sys) (compute_dps sys));
  print_newline ();

  print_string "* Graph init (system 7.11)\n";
  let zero = Term ("0", [])
  in let sys = [
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
  in my_print_graph (compute_graph (compute_symb sys) (compute_dps sys));
  print_newline ()
;;

test_graph_init ()
