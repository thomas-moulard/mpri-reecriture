(* Test graph acc/coacc/extract_components *)

open Reecriture;;

let print_write_graph g file =
  print_graph Format.std_formatter g;
  write_graph_dot file g
;;

let print_write_graphs sys files =
  let gs = extract_components (compute_graph (compute_dp_d sys)
                                 (compute_dps sys))
  in let rec print n gs =
    match gs with
    | [] -> ()
    | e::l ->
        begin
          Format.fprintf Format.str_formatter files n;
          print_write_graph e (Buffer.contents Format.stdbuf);
          print (n+1) l;
        end
  in
  print 0 gs
;;


let test_graph_init () =
  print_string "* Graph components (system 7.3)\n";
  print_write_graphs Examples.system_7_3 "graph_7-3-comp-%d.dot";
  print_newline ();

  print_string "* Graph components (system 7.11)\n";
  print_write_graphs Examples.system_7_11 "graph_7-11-comp-%d.dot";
  print_newline ();

  print_string "* Graph components (system 7.19)\n";
  print_write_graphs Examples.system_7_19 "graph_7-19-comp-%d.dot";
  print_newline ();
;;

test_graph_init ()
