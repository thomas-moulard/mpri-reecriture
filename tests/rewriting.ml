(* Rewriting functions *)

open Format;;

open Reecriture;;
open Examples;;
open Test;;

(****************************************************************************
 * Check rewriting.                                                         *
 ****************************************************************************)

(*
let rec test_apply_rule rule terms =
  match terms with
  | [] -> ()
  | term::l ->
      begin
        print_string "\t\t Term: ";
        print_term term;
        print_newline ();

        print_string "\t\t Result: ";
        print_option print_term (apply_rule term rule);
        print_newline ();
        print_newline ();
        test_apply_rule rule l
      end
;;

let rec test_apply_rule_rec rules terms =
  match rules with
  | [] -> ()
  | (left, right)::l ->
      begin
        print_string "\t Rule: ";
        print_term left;
        print_string " -> ";
        print_term right;
        print_newline ();
        test_apply_rule (left, right) terms;
        test_apply_rule_rec l terms;
      end
;;

let rec test_apply_rules_rec rules terms =
  match terms with
  | [] -> ()
  | term::l ->
      begin
        print_string "\t Term: ";
        print_term term;
        print_newline ();

        print_string "\t Result(s):\n\t\t";
        print_list (fun e -> print_term e; print_string "\n\t\t") (apply_rules term rules);
        print_newline ();
        print_newline ();
        test_apply_rules_rec rules l
      end
;;

let rec test_compute_step rules terms =
  match terms with
  | [] -> ()
  | term::l ->
      begin
        print_string "\t Term: ";
        print_term term;
        print_newline ();

        print_string "\t Result(s):\n\t\t";
        print_list (fun e -> print_term e; print_string "\n\t\t") (compute_step rules term);
        print_newline ();
        print_newline ();
        test_compute_step rules l
      end
;;

let rec test_compute_n_step n rules terms =
  match terms with
  | [] -> ()
  | term::l ->
      begin
        print_string "\t Term: ";
        print_term term;
        print_newline ();

        print_string "\t Result(s):\n\t\t";
        print_list (fun e -> print_term e; print_string "\n\t\t") (compute_n_step_reds rules n term);
        print_newline ();
        print_newline ();
        test_compute_n_step n rules l
      end
;;


let test_graph n sys t=
  print_term t;
  print_newline ();

  let rwlist = compute_n_step_reds sys n t in
  print_list
    (fun e -> print_string "\t- "; print_term e; print_newline ())
    rwlist;
  print_newline ();
;;

print_string "* System 7.3\n";
print_string "** Test apply_rule (rules one by one).\n";
test_apply_rule_rec Examples.system_7_3 Examples.terms_7_3;
print_newline ();

print_string "** Test apply_rules (rules alltogether).\n";
test_apply_rules_rec Examples.system_7_3 Examples.terms_7_3;
print_newline ();

print_string "** Test compute_step (rules alltogether + subterms).\n";
test_compute_step Examples.system_7_3 Examples.terms_7_3;
print_newline ();

for n = 0 to 3 do
  print_string "** Test compute_steps (compute_step ";
  print_int n;
  print_string " times).\n";
  test_compute_n_step n Examples.system_7_3 Examples.terms_7_3;
  print_newline ();
done;
*)

let check_apply_rule () =
  () (* FIXME: *)
;;

let check_apply_rules () =
  () (* FIXME: *)
;;

let check_compute_step () =
  () (* FIXME: *)
;;

let check_compute_n_step_reds () =
  () (* FIXME: *)
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
