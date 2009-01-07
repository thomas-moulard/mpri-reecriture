(* Test graph acc/coacc/extract_components *)

open Reecriture;;

let print_write_graph g file =
  print_graph Format.std_formatter g;
  write_graph_dot file g
;;

let print_write_graphs sys files =
  let g = compute_graph (compute_dp_d sys) (compute_dps sys) in
  let gs = extract_components g in
  begin
    for i = 0 to pred (graph_nb_nodes g) do
      print_string "Acc: "; print_list print_int (graph_acc g i);
      print_newline ();
      print_string "Co-acc: "; print_list print_int (graph_coacc g i);
      print_newline ();
    done;
    let rec print n gs =
      match gs with
      | [] -> ()
      | e::l ->
          begin
            print_write_graph e (Format.sprintf files n);
            print (n+1) l;
          end;
    in
    print 0 gs
  end
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
