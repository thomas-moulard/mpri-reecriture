(* Projection related functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Projection graph functions.                                              *
 ****************************************************************************)

let print_removable = function
  | Strict -> printf "strict"
  | Large -> printf "large"
  | No -> printf "no"
;;

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
  let eq n symblfct dp rules r = removable rules symblfct dp n == r in
  let chk n symblfct dp = metachk false print_system print_removable
      (eq n symblfct dp) in

  (* FIXME: insert result *)
  let fct = function
    | "+" -> 1
    | "-" -> 2
    | _ -> assert false in
  chk 5 fct (vX, vX) system_7_3 Strict;
;;

let check_gen_projs () =
  let eq_list p a b = try
    List.for_all2 p a b
    with Invalid_argument _ -> false in
  let eq rules b = eq_list (eq_list (==))
      (gen_projs rules (build_system_symblist rules)) b
  and print x =
    printf "@[<v 1>@\n";
    print_list
      (fun e ->
        printf "@[<h>";
        print_list (fun e -> printf "|%d| " e) e;
        printf "@]@\n") x;
    printf "@]" in
  let chk = metachk false print_system print eq in

  chk system_7_3 [[0;0;-1];[0;1;-1]];

  chk system_7_11 [
  [0; 0; 0; -1];
  [0; 0; 1; -1];
  [0; 1; 0; -1];
  [0; 1; 1; -1]
];

  chk system_7_19 [
  [-1; 0; 0; 0; 0; -1];
  [-1; 0; 1; 0; 0; -1];
  [-1; 1; 0; 0; 0; -1];
  [-1; 1; 1; 0; 0; -1]
];

;;

let check_check_proj_comp () =
  () (* FIXME: write this test. *)
;;

let check_find_component () =
  () (* FIXME: write this test. *)
;;

let check_check_proj () =
  () (* FIXME: write this test. *)
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
  chk ["+"; "-"] [("+", 1); ("-", 2)] fct;
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
  let eq n rules (list2, dp2) = try
    let (list1, dp1) = find_projection
        rules
        (compute_graph (compute_symbols rules) (compute_dps rules))
        (compute_symbols rules)
        n in
    let res = List.for_all2
        (fun (s1, int1) (s2, int2) -> eq_string s1 s2 && int1 == int2)
        list1 list2 in
    res && eq_dp dp1 dp2
  with Invalid_argument _ -> false
  and print (l, dp) =
    printf "@[";
    print_list (fun (s,n) -> printf "%s -> %d@\n" s n) l;
    printf "@\n";
    print_dp dp;
    printf "@]" in
  let chk n = metachk false print_system print (eq n) in

  chk 5 system_7_3 ([("+", 0)], (plus vX (succ vY), plus vX vY));
  chk 5 system_7_11 ([("/", 0); ("-", 0)], (minus (succ vX) (succ vY), minus vX vY));
  chk 5 system_7_19 ([("int", 0); ("intlist", -1)], (int (succ vX) (succ vY), int vX vY));
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
   (check_proj_fun_to_list, "projection function to projection list");
   (check_find_projection, "find projection")
 ] in
printf "Projection functions test suite.@\n";
exit (run_tests tests)
;;
