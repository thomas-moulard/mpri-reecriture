(* Test graph acc/coacc/extract_components *)

open Reecriture;;
open Examples;;

let print_write_graph g file =
  print_graph Format.std_formatter g;
  write_graph_dot file g
;;

let print_write_extr_comps g files =
  let print_int_space e = print_int e; print_string " " in
  let gs = extract_components g in
  begin
    for i = 0 to pred (graph_nb_nodes g) do
      print_string "\t Acc: "; print_list print_int_space (graph_acc g i);
      print_newline ();
      print_string "\t Co-acc: "; print_list print_int_space (graph_coacc g i);
      print_newline ();
    done;
    print_newline ();
    let rec print n gs =
      match gs with
      | [] -> ()
      | e::l ->
          begin
            print_string "\t *** Component n°"; print_int n; print_newline ();
            print_write_graph e (Format.sprintf files n);
            print (n+1) l;
          end;
    in
    print 0 gs;
    print_string "\n";
  end
;;

let print_write_extr_comps_from_sys sys files =
  let g = compute_graph (compute_dp_d sys) (compute_dps sys) in
  print_write_extr_comps g files
;;


let test_graph_extr_comps () =
  print_string "* Searchs strong comps on handmade graphs.\n";
  print_string "** System 7.11\n";
  print_write_extr_comps graph_7_11 "handmade-graph_7-11-comp-%d.dot";
  print_string "** System 7.19\n";
  print_write_extr_comps graph_7_19 "handmade-graph_7-19-comp-%d.dot";
  print_newline ();

  print_string "* Search using g_init.\n";
  print_string "** Graph components (system 7.3)\n";
  print_write_extr_comps_from_sys system_7_3 "graph_7-3-comp-%d.dot";

  print_string "** Graph components (system 7.11)\n";
  print_write_extr_comps_from_sys system_7_11 "graph_7-11-comp-%d.dot";

  print_string "** Graph components (system 7.19)\n";
  print_write_extr_comps_from_sys system_7_19 "graph_7-19-comp-%d.dot";
;;

test_graph_extr_comps ()
