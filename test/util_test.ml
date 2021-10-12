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

let slice_tests =
  "test suite for Util.slice"
  >::: [
         slice_test [ 0; 1; 2; 3 ] 2 ([ 0; 1 ], [ 2; 3 ]);
         slice_test [ 0; 1; 2; 3 ] 4 ([ 0; 1; 2; 3 ], []);
         slice_test [ 0; 1; 2; 3 ] 6 ([ 0; 1; 2; 3 ], []);
         slice_test [ 0; 1; 2; 3 ] 1 ([ 0 ], [ 1; 2; 3 ]);
       ]

let suite =
  "test suite for Koiiword.Util functions" >::: [ slice_tests ]
