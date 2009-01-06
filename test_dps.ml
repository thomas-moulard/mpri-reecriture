(* Test compute_dps with some subterms *)

open Reecriture;;

let print_symblist sl =
  let rec p sl =
    match sl with
    | [] -> ()
    | e::l ->
        print_string e;
        print_string " ";
        p l
  in
  if sl == [] then
    print_string "Empty\n"
  else
    p sl
;;

let rec print_term t =
  match t with
  | Var x ->
      print_string "X";
      print_int x
  | Term (s, tl) ->
      print_string s;
      print_string (" (");
      (
       match tl with
       | [] -> ()
       | e::[] -> print_term e
       | u::v::l ->
           print_term u;
           List.iter (fun e -> print_string ", "; print_term e) (v::l)
      );
      print_string (")");
;;

let print_dp dp =
  let (left, right) = dp
  in
  print_term left;
  print_string ", ";
  print_term right
;;

let print_dps dpl =
  let rec p dpl =
    match dpl with
    | [] -> ()
    | e::l ->
        print_dp e;
        print_newline ();
        p l
  in
  if dpl == [] then
    print_string "empty\n"
  else
    p dpl
;;

let rec print_system sys =
  match sys with
  | [] -> print_string ""
  | (t1, t2)::l ->
      print_term t1; print_string " -> "; print_term t2;
      print_newline ();
      print_system l
;;

let test_print () =
  print_string "** Test print.\n";
  print_string "* Ref: + (X0, X1)\n";
  print_term (Term ("+", [Var 0; Var 1]));
  print_newline ();

  print_string "* Ref: + (- (X0, X1, X2), X1)\n";
  print_term (Term ("+", [Term ("-", [Var 0; Var 1; Var 2]); Var 1]));
  print_newline ();
  print_newline ();
;;

let test_system sys =
  print_system sys; print_newline ();

  print_string "D: ";
  print_symblist (compute_dp_d sys); print_newline ();

  print_string "DPs:\n";
  print_dps (compute_dps sys);
  print_newline (); print_newline (); print_newline ()
;;

let test_compute_dps () =
  print_string "** Test compute_dps.\n";

  print_string "* Empty system.\n";
  (let sys = []
  in print_dps (compute_dps sys));
  print_newline ();

  print_string "* System 7.3.\n";
  let zero = Term ("0", [])
  in
  (let sys = [
    (* x + 0 -> x *)
    (Term ("+", [Var 0; zero]), Var 0);
    (* x+ S(y) -> S(x+y) *)
    (Term ("+", [Var 0; Term ("S", [Var 1])]),
     Term ("S", [Term("+", [Var 0; Var 1])]))
  ]
  in test_system sys);

  print_string "* System 7.11.\n";
  let zero = Term ("0", [])
  in
  (let sys = [
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
  in test_system sys);


  print_newline ();
;;

test_print ();
test_compute_dps ()
