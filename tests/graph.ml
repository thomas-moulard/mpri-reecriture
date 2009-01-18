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


let eq_graph g1 g2 =
  if graph_nb_nodes g1 != graph_nb_nodes g2 then
    false
  else
    begin
      let n = graph_nb_nodes g1
      and res = ref true in
      for i = 0 to pred n do
        for j = 0 to pred n do
          if g1.mat.(i).(j) != g2.mat.(i).(j) then
            res := false
        done;
      done;
      !res
    end
;;


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

  chk (make_empty_graph 2) [];
  chk graph_7_11 [];
  chk graph_7_19 [];
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
