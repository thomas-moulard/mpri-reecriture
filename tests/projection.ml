(* Projection related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Projection graph functions.                                              *
 ****************************************************************************)


let cst_proj n =
  fun symbl -> n
;;


let check_project () =
  let eq proj t1 t2 = eq_term (project proj t1) t2 in
  let chk p = metachk false print_term print_term (eq p) in

  printf "constant projection = 0@\n";
  chk (cst_proj 0) vX vX;
  chk (cst_proj 0) (succ vX) vX;
  chk (cst_proj 0) (plus vX vY) vX;
  chk (cst_proj 0) (Term ("?", [vX; vY; Var 2; Var 3])) vX;

  printf "constant projection = 1@\n";
  chk (cst_proj 1) (Term ("?", [vX; vY; Var 2; Var 3])) vY;

  printf "constant projection = 2@\n";
  chk (cst_proj 2) (Term ("?", [vX; vY; Var 2; Var 3])) (Var 2);

  printf "constant projection = 3@\n";
  chk (cst_proj 3) (Term ("?", [vX; vY; Var 2; Var 3])) (Var 3);
;;

let check_removable () =
  () (* FIXME: *)
;;

let check_gen_projs () =
  () (* FIXME: *)
;;

let check_check_proj_comp () =
  () (* FIXME: *)
;;

let check_find_component () =
  () (* FIXME: *)
;;

let check_check_proj () =
  () (* FIXME: *)
;;

let check_proj_list_to_fun () =
  let eq symbl symbl_list fct_b =
    try
      let res = proj_list_to_fun symbl_list in
      List.for_all (fun s -> res s == fct_b s) symbl
    with Invalid_argument _ -> false in
  let rec printfct symbl symblfct =
    match symbl with
    | [] -> ()
    | e::l -> printf "%s -> %d@\n" e (symblfct e); printfct l symblfct in
  let rec print = function
    | [] -> ()
    | (s, n)::l -> printf "%s -> %d@\n" s n; print l in
  let chk symbl = metachk false print (printfct symbl) (eq symbl) in
  let fct = function
    | "+" -> 1
    | "-" -> 2
    | _ -> assert false in
  chk ["+"; "-"] [("+", 0); ("-", 1)] fct;
;;

let check_proj_fun_to_list () =
  let eq symbl symblfct l =
    try
      let res = proj_fun_to_list symblfct symbl in
      List.for_all2
        (fun (s1, n1) (s2, n2)-> eq_string s1 s2 && n1 == n2)
        res l
    with Invalid_argument _ -> false in
  let rec printfct symbl symblfct =
    match symbl with
    | [] -> ()
    | e::l -> printf "%s -> %d@\n" e (symblfct e); printfct l symblfct in
  let rec print = function
    | [] -> ()
    | (s, n)::l -> printf "%s -> %d@\n" s n; print l in
  let chk symbl = metachk false (printfct symbl) print (eq symbl) in

  let fct = function
    | "+" -> 1
    | "-" -> 2
    | _ -> assert false in
  chk ["+"; "-"] fct [("+", 1); ("-", 2)];
;;

let check_find_projection () =
  let eq np n rules (list2, dp2) = try
    let (list1, dp1) = find_projection
        rules
        (compute_graph (compute_symbols rules) (compute_dps rules))
        (compute_symbols rules)
        n in
    let res = List.for_all2
        (fun (s1, int1) (s2, int2) -> eq_string s1 s2 && int1 == int2)
        list1 list2 in
    res && dp1 == dp2
  with Invalid_argument _ -> false
  and print (l, dp) =
    printf "@[";
    printf "@]" in
  let chk np n = metachk false print_system print (eq np n) in


  (* FIXME: insert result. *)
  chk 0 5 system_7_3 ([], (vX, vX));
  chk 0 5 system_7_11 ([], (vX, vX));
  chk 0 5 system_7_19 ([], (vX, vX));
;;

(****************************************************************************
 * Execute test suite.                                                      *
 ****************************************************************************)
let tests =
  [
   (check_project, "project");
   (check_removable, "removable");
   (check_gen_projs, "generate projections");
   (check_check_proj_comp, "check compute projection");
   (check_find_component, "find component");
   (check_check_proj, "check projection");
   (check_proj_list_to_fun, "projection list to projection function");
   (check_proj_fun_to_list, "projection function to projection lit");
   (check_find_projection, "find projection")
 ] in
printf "Projection functions test suite.@\n";
exit (run_tests tests)
;;
