(****************************************************************************
 * This modules gathers useful functions for software test.                 *
 ****************************************************************************)

(****************************************************************************
 * Types.                                                                   *
 ****************************************************************************)

type test = (unit -> unit) * string;;

(****************************************************************************
 * Functions.                                                               *
 ****************************************************************************)

val test_begin : test -> unit;;
val test_end : test -> unit;;

val run_tests : test list -> int;;

val check : bool -> unit;;
val fail_check : bool -> unit;;


(* Check whether (predicate x y) or ~(predicate x y).
   metachk b printl printr predicate x y

   b : expected failure ? (if yes then test fails if the
   test does not fail)

   printl, printr : print respectively x and y
   predicate : test x and y.
*)
val metachk : bool -> ('a -> unit) -> ('b -> unit)
  -> ('a -> 'b -> bool) -> 'a -> 'b -> unit;;
