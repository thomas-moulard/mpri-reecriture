(* Graph related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check graph functions.                                                   *
 ****************************************************************************)

let check_graph_nb_nodes () =
  let eq g n = (graph_nb_nodes g) == n
  and print = printf "%d" in
  let chk = metachk false print_graph print eq in

  let g = make_empty_graph 0 in
  chk g 0;

  let g = make_empty_graph 1 in
  chk g 1;

  let g = make_empty_graph 5 in
  chk g 5;
;;


let rec print_graphs = function
  | [] -> ()
  | e::l -> print_graph e; print_graphs l
;;


let check_make_empty_graph () =
  let chk = metachk false print_graph print_graph
      eq_graph in

  let g = {
    nb_nodes = 0;
    mat = Array.create_matrix 0 0 0;
    nb_succ = Array.create 0 0;
    nb_pred = Array.create 0 0
  } in
  chk (make_empty_graph 0) g;

  let g = {
    nb_nodes = 5;
    mat = Array.create_matrix 5 5 0;
    nb_succ = Array.create 5 0;
    nb_pred = Array.create 5 0
  } in
  chk (make_empty_graph 5) g;
;;

let check_add_edge () =
  let chk = metachk false print_graph print_graph
      eq_graph in

  let g1 = make_empty_graph 5
  and g2 = make_empty_graph 5 in
  add_edge 0 0 g1;
  add_edge 2 3 g1;

  g2.mat.(0).(0) <- 1;
  g2.mat.(2).(3) <- 1;
  g2.nb_pred.(0) <- 1;
  g2.nb_succ.(0) <- 1;

  g2.nb_pred.(3) <- 1;
  g2.nb_succ.(2) <- 1;

  chk g1 g2;
;;

let check_graph_acc () =
  let eq node g l2 = try
    List.for_all2 (==) (graph_acc g node) l2
    with Invalid_argument _ -> false
  and print = print_list
      (fun e -> print_int e; printf "@ ") in
  let chk node = metachk false print_graph print (eq node) in

  printf "With state 0 (7.11, 7.19):@\n";
  chk 0 (make_empty_graph 3) [0];
  chk 0 graph_7_11 [0];
  chk 0 graph_7_19 [0];

  printf "With state 1 (7.11, 7.19):@\n";
  chk 1 (make_empty_graph 3) [1];
  chk 1 graph_7_11 [0;2;1];
  chk 1 graph_7_19 [3;2;0;1];

  printf "With state 2 (7.11, 7.19):@\n";
  chk 2 (make_empty_graph 3) [2];
  chk 2 graph_7_11 [0;2];
  chk 2 graph_7_19 [0;2];

  printf "With state 3 (7.19):@\n";
  chk 3 graph_7_19 [1;0;2;3];
;;

let check_graph_coacc () =
  let eq node g l2 = try
    List.for_all2 (==) (graph_coacc g node) l2
    with Invalid_argument _ -> false
  and print = print_list
      (fun e -> print_int e; printf "@ ") in
  let chk node = metachk false print_graph print (eq node) in

  printf "With state 0 (7.11, 7.19):@\n";
  chk 0 (make_empty_graph 3) [0];
  chk 0 graph_7_11 [1;2;0];
  chk 0 graph_7_19 [1;3;2;0];

  printf "With state 1 (7.11, 7.19):@\n";
  chk 1 (make_empty_graph 3) [1];
  chk 1 graph_7_11 [1];
  chk 1 graph_7_19 [3;1];

  printf "With state 2 (7.11, 7.19):@\n";
  chk 2 (make_empty_graph 3) [2];
  chk 2 graph_7_11 [1;2];
  chk 2 graph_7_19 [1;3;2];

  printf "With state 3 (7.19):@\n";
  chk 3 graph_7_19 [1;3];
;;

let check_graph_strong_connectivity () =
  let eq g l2 = try
    List.for_all2 (==) (Array.to_list (graph_strong_connectivity g)) l2
    with Invalid_argument _ -> false
  and print = print_list
      (fun e -> print_int e; printf "@ ") in
  let chk = metachk false print_graph print eq in

  chk (make_empty_graph 2) [1;2];
  chk graph_7_11 [1;2;3];
  chk graph_7_19 [1;2;3;2];
;;

let check_extract_components () =
  let eq g l2 = try
    List.for_all2 eq_graph (extract_components g) l2
    with Invalid_argument _ -> false
  and print gl =
    printf "@[<v>";
    print_list (fun e -> print_graph e; printf "@\n") gl;
    printf "@]" in
  let chk = metachk false print_graph print eq in

  chk (make_empty_graph 2) [make_empty_graph 2; make_empty_graph 2];

  let g1 = make_empty_graph 3
  and g2 = make_empty_graph 3 in
  add_edge 1 1 g1;
  add_edge 0 0 g2;

  chk graph_7_11 [make_empty_graph 3; g1; g2];

  write_graph_dot "graph-7-11-1.dot" g1;
  write_graph_dot "graph-7-11-2.dot" g2;

  let g1 = make_empty_graph 4
  and g2 = make_empty_graph 4 in
  add_edge 1 3 g1;
  add_edge 3 1 g1;
  add_edge 3 3 g1;
  add_edge 0 0 g2;

  write_graph_dot "graph-7-19-1.dot" g1;
  write_graph_dot "graph-7-19-2.dot" g2;

  chk graph_7_19 [make_empty_graph 4; g1; g2];
;;

let check_compute_graph () =
  let eq rules = eq_graph (compute_graph (compute_symbols rules) (compute_dps rules)) in
  let chk = metachk false print_system print_graph eq in

  let g = make_empty_graph 1 in
  add_edge 0 0 g;

  write_graph_dot "graph_init_7_3.dot"
    (compute_graph (compute_symbols system_7_3) (compute_dps system_7_3));
  chk system_7_3 g;

  let g = make_empty_graph 3 in
  add_edge 0 0 g;
  add_edge 0 1 g;
  add_edge 0 2 g;
  add_edge 1 0 g;
  add_edge 1 1 g;
  add_edge 1 2 g;
  add_edge 2 0 g;
  add_edge 2 1 g;
  add_edge 2 2 g;

  write_graph_dot "graph_init_7_11.dot"
    (compute_graph (compute_symbols system_7_11) (compute_dps system_7_11));
  chk system_7_11 g;

  let g = make_empty_graph 4 in
  add_edge 0 0 g;
  add_edge 0 1 g;
  add_edge 0 2 g;
  add_edge 0 3 g;
  add_edge 1 0 g;
  add_edge 1 1 g;
  add_edge 1 2 g;
  add_edge 1 3 g;
  add_edge 2 0 g;
  add_edge 2 1 g;
  add_edge 2 2 g;
  add_edge 2 3 g;
  add_edge 3 0 g;
  add_edge 3 1 g;
  add_edge 3 2 g;
  add_edge 3 3 g;

  write_graph_dot "graph_init_7_19.dot"
    (compute_graph (compute_symbols system_7_19) (compute_dps system_7_19));
  chk system_7_19 g;
;;


(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_graph_nb_nodes, "number of graph's nodes");
   (check_make_empty_graph, "generate empty graph");
   (check_add_edge, "add an edge");
   (check_graph_acc, "state's accessibility list");
   (check_graph_coacc, "state's coaccessibility list");
   (check_graph_strong_connectivity,
    "colorize graph depending on strongly connected components");
   (check_extract_components, "graph's strongly connected components");
   (check_compute_graph, "compute initial graph")
 ] in
printf "Graph functions test suite.@\n";

write_graph_dot "handmade-graph_7_11.dot" graph_7_11;
write_graph_dot "handmade-graph_7_19.dot" graph_7_19;

exit (run_tests tests)
;;
