open OUnit2 (*OUnit2*)
open Sum (*[open Sum] brings into scope the definition from sum.ml*)


(* let tests = "test suite for sum" >::: [
  "empty" >:: (fun _ -> assert_equal 0 (sum []));
  "singleton" >:: (fun _ -> assert_equal 1 (sum [1]));
  "two elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]));
] (*a list of test cases
    >:: a custom operator
    Every test case function receives as input a parameter that OUnit calls a test context.
    
    *) *)

let make_sum_test name expected_output input =
  name >:: (fun _ -> assert_equal expected_output (sum input))

let tests = "test suite for sum" >::: [
  make_sum_test "empty" 0 [];
  make_sum_test "singleton" 1 [1];
  make_sum_test "two elements" 3 [1; 2];
]

let _ = run_test_tt_main tests

