(* This file contains some rewriting systems used by tests. *)

open Reecriture;;

let zero = Term ("0", []);;
let empty = [];;

let vX = Var 0;;
let vY = Var 1;;

let vU = Var 10;;
let vV = Var 11;;

let succ x =
  Term ("S", [x])
;;

let term_binop s a b =
  Term (s, [a; b])
;;
let plus = term_binop "+";;
let minus = term_binop "-";;
let times = term_binop "x";;
let div = term_binop "/";;


let system_7_3 =
  [
   (* x + 0 -> x *)
   (plus vX zero, vX);
   (* x + S(y) -> S(x + y) *)
   (plus vX (succ vY), (succ (plus vX vY)))
 ]
;;

let terms_7_3 =
  [
   plus vU zero;
   plus vU (succ vV);
   plus vU (succ (succ vU));
   plus vU (succ (zero));
   plus (succ (succ vU)) vV
 ]
;;

let system_7_11 =
  [
   (* x - 0 -> x *)
   (minus vX zero, vX);
   (* 0 / S(y) -> 0 *)
   (div zero (succ vY), zero);
   (* S(x) - S(y) -> x - y *)
   (minus (succ vX) (succ vY), minus vX vY);
   (* S(x) / S(y) -> S((x - y) / S(y)) *)
   (div (succ vX) (succ vY), succ (div (minus vX vY) (succ vY)))
 ]
;;


let intlist a = Term ("intlist", [a]);;
let int a b = Term ("int", [a;b]);;
let append a b = Term (":", [a;b]);;
let empty_list = Term ("[]", []);;

let system_7_19 =
  let empty = empty_list in
  [
   (* intlist ([]) -> [] *)
   (intlist empty, empty);
   (* int (0, 0) -> 0 : [] *)
   (int zero zero, append zero empty);
   (* int (s x, 0) -> 0 : [] *)
   (int (succ vX) zero, empty);
   (* intlist (x : y) -> s x : (intlist y) *)
   (intlist (append vX vY), append (succ vX) (intlist vY));
   (* int (0, s y) -> 0 : int (s 0, s y) *)
   (int zero (succ vY), append zero (int (succ zero) (succ vY)));
   (* int (s x, s y) -> intlist (int (x, y)) *)
   (int (succ vX) (succ vY), intlist (int vX vY))
 ]
;;


(* Hand-made graphs for graph-related test. *)

(* System 7.11 *)
let graph_7_11 =
  let g = make_empty_graph 3 in
  add_edge 0 0 g;
  add_edge 1 1 g;
  add_edge 2 0 g;
  add_edge 1 2 g;
  g
;;

(* System 7.19 *)
let graph_7_19 =
  let g = make_empty_graph 4 in
  add_edge 0 0 g;
  add_edge 3 3 g;

  add_edge 2 0 g;
  add_edge 3 2 g;
  add_edge 1 2 g;
  add_edge 1 3 g;
  add_edge 3 1 g;
  g
;;
