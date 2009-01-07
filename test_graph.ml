(* Test graph generation *)

open Reecriture;;

let print_write_graph sys file =
  let g = (compute_graph (compute_dp_d sys) (compute_dps sys))
  in
  print_graph Format.std_formatter g;
  write_graph_dot file g
;;

let test_graph_init () =
  print_string "* Graph init (system 7.3)\n";
  print_write_graph Examples.system_7_3 "graph_7-3.dot";
  print_newline ();

  print_string "* Graph init (system 7.11)\n";
  print_write_graph Examples.system_7_11 "graph_7-11.dot";
  print_newline ();

  print_string "* Graph init (system 7.19)\n";
  print_write_graph Examples.system_7_19 "graph_7-19.dot";
  print_newline ();
;;

test_graph_init ()
