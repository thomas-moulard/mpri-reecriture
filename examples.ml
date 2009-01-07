(* This file contains some rewriting systems used by tests. *)

open Reecriture;;

let zero = Term ("0", []);;
let empty = [];;

let vX = Var 0;;
let vY = Var 1;;

let system_7_3 =
  [
   (* x + 0 -> x *)
   (Term ("+", [vX; zero]), vX);
   (* x+ S(y) -> S(x+y) *)
   (Term ("+", [vX; Term ("S", [vY])]),
    Term ("S", [Term("+", [vX; vY])]))
 ]
;;

let system_7_11 =
  [
   (* x - 0 -> x *)
   (
    Term ("-", [vX; zero]),
    vX
   );
   (* 0 / S(y) -> 0 *)
   (
    Term ("/", [zero; Term ("S", [vY])]),
    zero
   );
   (* S(x) - S(y) -> x - y *)
   (
    Term ("-", [Term ("S", [vX]); Term ("S", [vY])]),
    Term ("-", [vX; vY])
   );
   (* S(x) / S(y) -> S((x - y) / S(y)) *)
   (
    Term ("/", [Term ("S", [vX]); Term ("S", [vY])]),
    Term ("S", [Term ("/", [Term ("-", [vX; vY]); vY])])
   );
 ]
;;

let system_7_19 =
  let empty = Term ("[]", []) in
  [
(* intlist ([]) -> [] *)
   (
   Term ("intlist", [empty]),
   empty
   );
(* int (0, 0) -> 0 : [] *)
   (
   Term ("int", [zero; zero]),
   Term (":", [zero; empty])
   );
(* int (s x, 0) -> 0 : [] *)
   (
   Term ("int", [Term ("s", [vX]); zero]),
   empty
   );
(* intlist (x : y) -> s x : (intlist y) *)
   (
   Term ("intlist", [Term (":", [vX; vY])]),
   Term (":", [Term ("s", [vX]); Term ("intlist", [vY])])
   );
(* int (0, s y) -> 0 : int (s 0, s y) *)
   (
   Term ("int", [zero; Term ("s", [vY])]),
   Term (":", [zero; Term ("int", [Term ("s", [zero]); Term ("s", [vY])])])
   );
(* int (s x, s y) -> intlist (int (x, y)) *)
   Term ("int", [Term ("s", [vX]); Term ("s", [vY])]),
   Term ("intlist", [Term ("int", [vX; vY])])
 ]
;;
