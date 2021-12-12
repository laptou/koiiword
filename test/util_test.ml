open OUnit2
open Koiiword.Util
open Util

let slice_test (lst : int list) n (expected_left, expected_right) : test
    =
  let test_name =
    Printf.sprintf "slice %s %d gives (%s, %s)"
      (pp_list string_of_int lst)
      n
      (pp_list string_of_int expected_left)
      (pp_list string_of_int expected_right)
  in
  test_name >:: fun _ ->
  assert_equal (expected_left, expected_right) (slice lst n)
    ~printer:(fun (left, right) ->
      Printf.sprintf "(%s, %s)"
        (pp_list string_of_int left)
        (pp_list string_of_int right))

let set_test (lst : int list) n item expected : test =
  let test_name =
    Printf.sprintf "set %s %d %d gives %s"
      (pp_list string_of_int lst)
      n item
      (pp_list string_of_int expected)
  in
  test_name >:: fun _ ->
  assert_equal expected (set lst n item)
    ~printer:(pp_list string_of_int)

let wrap_test (x : int) (lim : int) expected : test =
  let test_name = Printf.sprintf "wrap %d %d gives %d" x lim expected in
  test_name >:: fun _ ->
  assert_equal expected
    (Koiiword.Util.wrap x lim)
    ~printer:string_of_int

let slice_tests =
  "test suite for Util.slice"
  >::: [
         slice_test [ 0; 1; 2; 3 ] 2 ([ 0; 1 ], [ 2; 3 ]);
         slice_test [ 0; 1; 2; 3 ] 4 ([ 0; 1; 2; 3 ], []);
         slice_test [ 0; 1; 2; 3 ] 6 ([ 0; 1; 2; 3 ], []);
         slice_test [ 0; 1; 2; 3 ] 1 ([ 0 ], [ 1; 2; 3 ]);
       ]

let set_tests =
  "test suite for Util.set"
  >::: [
         set_test [ 0; 1; 2; 3 ] 2 100 [ 0; 1; 100; 3 ];
         set_test [ 0; 1; 2; 3 ] 4 100 [ 0; 1; 2; 3; 100 ];
         set_test [ 0; 1; 2; 3 ] 6 100 [ 0; 1; 2; 3; 100 ];
         set_test [ 0; 1; 2; 3 ] 1 100 [ 0; 100; 2; 3 ];
       ]

let wrap_tests =
  "test suite for Util.wrap"
  >::: [
         wrap_test 16 17 16;
         wrap_test 17 17 17;
         wrap_test 18 17 (-17);
         wrap_test 19 17 (-16);
         wrap_test 34 17 (-1);
         wrap_test 35 17 0;
         wrap_test (-16) 17 (-16);
         wrap_test (-17) 17 (-17);
         wrap_test (-18) 17 17;
         wrap_test (-19) 17 16;
       ]

let suite =
  "test suite for Koiiword.Util functions"
  >::: [ slice_tests; set_tests; wrap_tests ]
