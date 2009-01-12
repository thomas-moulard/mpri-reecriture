(* Test the whole software *)

open Printexc;;
open Reecriture;;

let print_write_graph g file =
  print_graph Format.std_formatter g;
  write_graph_dot file g
;;

let print_write_graphs sys files =
  try
    let gs = main sys in
    begin
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
  with e -> print_string (to_string e)
;;


let test_graph_init () =
  print_string "* Main (system 7.3)\n";
  print_write_graphs Examples.system_7_3 "graph_7-3-main-%d.dot";
  print_newline ();

  print_string "* Main (system 7.11)\n";
  print_write_graphs Examples.system_7_11 "graph_7-11-main-%d.dot";
  print_newline ();

  print_string "* Main (system 7.19)\n";
  print_write_graphs Examples.system_7_19 "graph_7-19-main-%d.dot";
  print_newline ();
;;

test_graph_init ()

