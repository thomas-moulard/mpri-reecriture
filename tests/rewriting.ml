(* Rewriting functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check rewriting.                                                         *
 ****************************************************************************)

let eq_option eq a b =
  match (a, b) with
  | (None, None) -> true
  | (None, _)  | (_, None) -> false
  | (Some x, Some y) -> eq x y
;;

let print_terms tl =
  printf "@[";
  print_list (fun t -> printf "@\n"; print_term t) tl;
  printf "@]"
;;



let check_apply_rule () =
  let eq rule t1 opt = eq_option eq_term (apply_rule t1 rule) opt in
  let chk rule = metachk false print_term (print_option print_term)
      (eq rule) in

  printf "Using rule X-0 -> 0@\n";
  let rule = (minus vX zero, vX) in
  chk rule vX None;
  chk rule (minus vX zero) (Some vX);
  chk rule (minus vY zero) (Some vY);
;;

let check_apply_rules () =
  let eq rules t l2 = try
    List.for_all2 eq_term (apply_rules t rules) l2
  with Invalid_argument _ -> false
  and print = print_terms in
  let chk rules = metachk false print_term print (eq rules) in

  printf "* System 7.3:";
  print_system system_7_3; printf "@\n";
  chk system_7_3 vX [vX];

  printf "@\n";
  printf "* System 7.11:";
  print_system system_7_11; printf "@\n";
  chk system_7_11 vX [vX; zero];

  printf "@\n";
  printf "* System 7.19:";
  print_system system_7_19; printf "@\n";
  chk system_7_19 vX [append zero empty_list; vX; empty_list];
;;

let check_compute_step () =
  let eq rules t l2 = try
    List.for_all2 eq_term (compute_step rules t) l2
  with Invalid_argument _ -> false
  and print = print_terms in
  let chk rules = metachk false print_term print (eq rules) in

  printf "* System 7.3:";
  print_system system_7_3; printf "@\n";
  chk system_7_3 vX [vX];

  let t = succ (minus vY zero) in
  chk system_7_3 t [succ (minus vY zero)];

  printf "@\n";
  printf "* System 7.11:";
  print_system system_7_11; printf "@\n";
  chk system_7_11 vX [vX; zero];

  printf "@\n";
  printf "* System 7.19:";
  print_system system_7_19; printf "@\n";
  chk system_7_19 vX [append zero empty_list; vX; empty_list];
;;

let check_compute_n_step_reds () =
  let eq rules n t l2 = try
    List.for_all2 eq_term (compute_n_step_reds n rules t) l2
  with Invalid_argument _ -> false
  and print = print_terms in
  let chk rules n = metachk false print_term print (eq rules n) in

  let n = 5 in

  printf "* System 7.3 (%d steps):" n;
  print_system system_7_3; printf "@\n";
  chk n system_7_3 vX [vX];

  let t = succ (minus vY zero) in
  chk n system_7_3 t [succ (minus vY zero)];

  printf "@\n";
  printf "* System 7.11 (%d steps):" n;
  print_system system_7_11; printf "@\n";
  chk n system_7_11 vX [zero; vX];

  printf "@\n";
  printf "* System 7.19 (%d steps):" n;
  print_system system_7_19; printf "@\n";

  chk n system_7_19 vX [empty_list; vX; append zero empty_list];
;;


(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_apply_rule, "apply a rule to a term");
   (check_apply_rules, "apply several rules to a term");
   (check_compute_step, "compute one rewriting step");
   (check_compute_n_step_reds, "compute n rewriting steps")
 ] in
printf "Rewriting functions test suite.@\n";
exit (run_tests tests)
;;
