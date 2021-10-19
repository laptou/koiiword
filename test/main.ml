open OUnit2

let super_suite =
  test_list
    [
      Generate_letters_test.suite;
      Util_test.suite;
      Layout_test.suite;
      State_test.suite;
    ]

let _ = run_test_tt_main super_suite
