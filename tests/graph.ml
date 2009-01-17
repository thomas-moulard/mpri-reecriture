(* Graph related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check graph functions.                                                   *
 ****************************************************************************)

(*let print_int_space e  = print_int e; print_string " ";;

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
*)

(*
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
  let g = compute_graph (compute_symbols sys) (compute_dps sys) in
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

*)

let check_graph_nb_nodes () =
  () (* FIXME: *)
;;

let check_make_empty_graph () =
  () (* FIXME: *)
;;

let check_add_edge () =
  () (* FIXME: *)
;;

let check_graph_acc () =
  () (* FIXME: *)
;;

let check_graph_coacc () =
  () (* FIXME: *)
;;

let check_graph_strong_connectivity () =
  () (* FIXME: *)
;;

let check_extract_components () =
  () (* FIXME: *)
;;

let check_compute_graph () =
  () (* FIXME: *)
;;


(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_graph_nb_nodes, "number of graph's nodes");
   (check_make_empty_graph, "generate empty graph");
   (check_add_edge, "add an edge");
   (check_graph_acc, "state's accessbility list");
   (check_graph_coacc, "state's coaccessbility list");
   (check_graph_strong_connectivity,
    "colorize graph depending on strongly connected components");
   (check_extract_components, "graph's strongly connected components");
   (check_compute_graph, "compute initial graph")
 ] in
printf "Graph functions test suite.@\n";
exit (run_tests tests)
;;
