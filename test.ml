open Format;;

(****************************************************************************
 * Types.                                                                   *
 ****************************************************************************)

type test = (unit -> unit) * string;;

(****************************************************************************
 * Exceptions.                                                              *
 ****************************************************************************)
exception Test_failed;;

(****************************************************************************
 * Functions.                                                               *
 ****************************************************************************)

let test_begin (test_fct, name) =
  printf "Test %s:@\n" name;
  printf " @[<v>"
;;

let test_end _ =
  printf "@]@."
;;

let run_tests tests =
  let run n test =
    let (test_fct, _) = test in
    test_begin test;
    begin
      try
        test_fct ();
        printf "Result: SUCCESS@\n";
      with Test_failed ->
        begin
          n := !n + 1;
          printf "Result: FAILURE@\n"
        end;
    end;
    test_end test;
  in
  printf "---------------------------------------------------@\n";
  printf "- The output of the test suite is:                -@\n";
  printf "- INPUT = OUTPUT                                  -@\n";
  printf "- where INPUT is the INPUT of the tested function -@\n";
  printf "-       OUTPUT the expected result                -@\n";
  printf "---------------------------------------------------@\n";
  let n = ref 0 in
  List.iter (run n) tests;
  !n
;;

let check = function
  | true -> ()
  | false -> raise Test_failed
;;

let fail_check = function
  | false -> ()
  | true -> raise Test_failed
;;
