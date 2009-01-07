(* This file contains some rewriting systems used by tests. *)

open Reecriture;;

let zero = Term ("0", []);;

let empty = [];;

let system_7_3 =
  [
   (* x + 0 -> x *)
   (Term ("+", [Var 0; zero]), Var 0);
   (* x+ S(y) -> S(x+y) *)
   (Term ("+", [Var 0; Term ("S", [Var 1])]),
    Term ("S", [Term("+", [Var 0; Var 1])]))
 ]
;;

let system_7_11 =
  [
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
;;
