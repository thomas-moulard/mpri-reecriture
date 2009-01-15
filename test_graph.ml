(*
  Test graph-related features.

  - strong components detection,
  - graph_init generation
 *)

open Reecriture;;
open Examples;;

let print_int_space e  = print_int e; print_string " ";;

let print_write_graph sys file =
  let g = (compute_graph (compute_symbols sys) (compute_dps sys))
  in
  print_graph Format.std_formatter g;
  print_newline ();
  write_graph_dot file g
;;

let test_graph_init () =
  print_string "** Graph init (system 7.3)\n";
  print_write_graph system_7_3 "graph_7-3.dot";

  print_string "** Graph init (system 7.11)\n";
  print_write_graph system_7_11 "graph_7-11.dot";

  print_string "** Graph init (system 7.19)\n";
  print_write_graph system_7_19 "graph_7-19.dot";
  print_newline ();
;;

let test_acc g =
  for i = 0 to graph_nb_nodes g - 1 do
    print_string "Node ";
    print_int i;
    print_string ":\n";
    print_string "\t Acc: ";
    print_list print_int_space (graph_acc g i);
    print_newline ();
    print_string "\t Coacc: ";
    print_list print_int_space (graph_coacc g i);
    print_newline ();
  done
;;


print_string "* Test acc/coacc on handmade graphs.\n";
print_string "** System 7.11\n";
test_acc graph_7_11;
print_string "** System 7.19\n";
test_acc graph_7_19;
print_newline ();

print_string "* Test graph init.\n";
test_graph_init ();


write_graph_dot "handmade_graph_7-11.dot" graph_7_11;
write_graph_dot "handmade_graph_7-19.dot" graph_7_19;





