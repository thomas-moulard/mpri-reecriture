(* Tests term related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Factorized tests code.                                                   *
 ****************************************************************************)

(* Check whether (predicate x y) or ~(predicate x y). *)
let metachk expected_failure printl printr predicate x y =
  let res = predicate x y in
  let res_str =
    if expected_failure then
      (if res then "KO" else "OK")
    else
      (if res then "OK" else "KO")
  and ope = if res then "=" else "!=" in
  printf "@[<h> [%s] " res_str; printl x;
  printf "@ %s@ " ope;
  printr y; printf "@]@\n";
  if expected_failure then
    fail_check res
  else
    check res
;;

let print_terms tl =
  printf "@[";
  print_list (fun t -> printf "@\n"; print_term t) tl;
  printf "@]"
;;

(****************************************************************************
 * Check term equality.                                                     *
 ****************************************************************************)
let check_eq_term () =
  let chk = metachk false print_term print_term eq_term
  and fail_chk = metachk true print_term print_term eq_term in

  chk zero zero;
  chk vX vX;
  fail_chk vX vY;

  chk (succ vX) (succ vX);
  fail_chk (succ vX) (succ vY);
  fail_chk vX (succ vY);
  fail_chk (succ vY) vX;

  let t =
    plus (succ vX) (succ (plus (succ vY) (succ vX))) in
  chk t t
;;

(****************************************************************************
 * Check dependencies pairs equality.                                       *
 ****************************************************************************)
let check_eq_dp () =
  let chk = metachk false print_dp print_dp eq_dp
  and fail_chk = metachk true print_dp print_dp eq_dp in
  let chk_id x = chk x x in

  chk_id (vX, vX);
  chk_id (vX, vY);

  fail_chk (vX, vX) (vX, vY);
  fail_chk (vY, vX) (vX, vX);

  let t1 = minus (plus vX vY) (succ (succ vX))
  and t2 = plus (plus vX vY) (succ vX) in
  chk_id (t1, t2)
;;

(****************************************************************************
 * Check duplicated terms removal function.                                 *
 ****************************************************************************)
let check_uniq_term () =
  let eq_list l1 l2 =
    try
      List.for_all2 eq_term (uniq_term l1) l2
    with Invalid_argument _ -> false
  and print_terms = print_list (fun e -> print_term e; printf "@ ") in
  let chk = metachk false print_terms print_terms eq_list in

  chk [] [];
  chk [vX] [vX];
  chk [vX; vX] [vX];
  chk [vX; vX; vX] [vX];
  chk [vX; vY; vX] [vY; vX];

  let t = plus (succ vX) (minus (succ vY) vX) in
  chk [t] [t];
  chk (uniq_term [t; t]) [t];
;;

(****************************************************************************
 * Check all symbols list computation.                                      *
 ****************************************************************************)
let check_build_symblist () =
  let eq l1 l2 =
    try
      List.for_all2 eq_string (build_symblist l1) l2
    with Invalid_argument _ -> false
  and print_symbs = print_list (fun str -> printf "%s@ " str) in
  let chk = metachk false print_term print_symbs eq in

  chk vX [];
  chk (succ vX) ["S"];
  chk (plus (succ vX) vY) ["S"; "+"];

  let t = plus (succ vX) (minus (succ vX) (plus (succ (succ vY)) vX)) in
  chk t ["-"; "S"; "+"];
;;

(****************************************************************************
 * Check symbols computation.                                               *
 ****************************************************************************)
let check_compute_symbols () =
  let eq l1 l2 =
    try
      List.for_all2 eq_string (compute_symbols l1) l2
    with Invalid_argument _ -> false
  and print_symbs = print_list (fun str -> printf "%s@ " str) in
  let chk = metachk false print_system print_symbs eq in

  chk [] [];
  chk [(vX,vX)] [];
  chk system_7_3 ["+"];
  chk system_7_11 ["/"; "-"];
  chk system_7_19 ["int"; "intlist"];
;;

let check_get_var_max () =
  let eq a b = (get_var_max a) == b in
  let chk = metachk false print_term print_int eq in

  chk (Var 0) 0;
  chk (Term ("zero", [])) 0;
  chk (Var 10) 10;

  chk (minus (succ (Var 40)) (Var 50)) 50;
  chk (minus (plus (Var 10) (Var 20)) (Var 4)) 20;
;;

let check_mk_fresh_var () =
  let n = ref 0 in
  printf "max var id: %d@\n" !n;
  printf "new var: ";
  print_term (mk_fresh_var n);
  printf "@\n";
  check (!n == 1);
  printf "max var id: %d@\n" !n;
  printf "new var: ";
  print_term (mk_fresh_var n);
  printf "@\n";
  printf "max var id: %d@\n" !n;
  check (!n == 2);
;;

let check_cap () =
  let symbl = ["+"; "-"] in
  let eq t1 t2 = eq_term (cap symbl t1) t2 in
  let chk = metachk false print_term print_term eq in

  chk zero zero;

  let t = succ vX in chk t t;
  let t = plus (Var 0) (Var 1) in chk t (Var 2);
  let t = succ (plus (Var 0) (Var 1)) in chk t (succ (Var 2));
;;

let check_ren () =
  let eq t1 t2 = eq_term (ren t1) t2 in
  let chk = metachk false print_term print_term eq
  and not_chk = metachk true print_term print_term eq in

  chk zero zero;
  not_chk vX vX;

  chk (Var 0) (Var 1);
  chk (plus (Var 22) (Var 10)) (plus (Var 23) (Var 24));
;;

let check_subterms () =
  let eq l1 l2 =
    try
      List.for_all2 eq_term (subterms l1) l2
    with Invalid_argument _ -> false in
  let chk = metachk false print_term print_terms eq in

  chk zero [zero];
  chk vX [vX];

  chk (succ vX) [vX; succ vX];

  chk (plus (succ vX) vY) [vX; succ vX; vY; plus (succ vX) vY];
;;

let check_strict_subterms () =
  let eq l1 l2 =
    try
      List.for_all2 eq_term (strict_subterms l1) l2
    with Invalid_argument _ -> false in
  let chk = metachk false print_term print_terms eq in

  chk zero [];
  chk vX [];

  chk (succ vX) [vX];
  chk (plus (succ vX) vY) [vY; succ vX; vX];
;;

let check_symbol_arity () =
  let eq symbl a b = (symbol_arity symbl a) == b in
  let chk symbl = metachk false print_term print_int (eq symbl) in

  printf "Using symbol ``+'':@\n";
  chk "+" zero (-1);
  chk "+" (plus vX vY) 2;
  chk "+" (succ (plus vX vY)) 2;
  chk "+" (minus (succ (plus (succ vX) vY)) vX) 2;

  printf "Using symbol ``S'':@\n";
  chk "S" zero (-1);
  chk "S" (succ vX) 1;
  chk "S" (succ (plus vX vY)) 1;
  chk "S" (minus (succ (plus vX vY)) vX) 1;
;;

let check_system_symbol_arity () =
  let eq symbl a b = (system_symbol_arity a symbl) == b in
  let chk symbl = metachk false print_system print_int (eq symbl) in

  printf "Using symbol ``+'':@\n";
  chk "+" [(zero, zero)] (-1);
  chk "+" system_7_3 2;

  printf "Using symbol ``-'':@\n";
  chk "-" system_7_11 2;
  printf "Using symbol ``-'':@\n";
  chk "/" system_7_11 2;

  printf "Using symbol ``intlist'':@\n";
  chk "intlist" system_7_19 1;
  printf "Using symbol ``int'':@\n";
  chk "int" system_7_19 2;

  printf "Using symbol ``S'':@\n";
  chk "S" [(zero, zero)] (-1);
  chk "S" system_7_3 1;
  chk "S" system_7_11 1;
  chk "S" system_7_19 (-1);
;;

(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_eq_term, "term equality");
   (check_eq_dp, "dependency pair equality");
   (check_uniq_term, "uniq list of terms");
   (check_build_symblist, "build list of all symbols");
   (check_compute_symbols, "compute symbols");
   (check_get_var_max, "get the highest used variable id");
   (check_mk_fresh_var, "make a fresh variable");
   (check_cap, "cap function");
   (check_ren, "ren function");
   (check_subterms, "subterms");
   (check_strict_subterms, "strict subterms");
   (check_symbol_arity, "symbol arity");
   (check_system_symbol_arity, "sytem's symbols arities")
 ] in
printf "Term related functions test suite.@\n";
exit (run_tests tests)
;;
