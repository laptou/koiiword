open OUnit2
open Koiiword.Generate_letters
open Util

let is_letter ch = match ch with 'A' .. 'Z' -> true | _ -> false

let new_deck_length_test () : test =
  "new_deck creates deck w/ size 7" >:: fun _ ->
  let deck = new_deck () in
  assert_equal true (List.length deck = 7)

let new_deck_content_test () : test =
  "new_deck creates deck w/ only letters" >:: fun _ ->
  let deck = new_deck () in
  assert_equal true
    (List.for_all is_letter deck)
    ~msg:(pp_list pp_char deck)

let test_cases =
  [
    new_deck_length_test ();
    new_deck_content_test ();
    (* new_deck_testcontent "start game creates list of letters A-Z"
       temp_deck; new_deck_length_test "start game creates list of
       length 7" (Generate_letters.remove_let 0 2 temp_deck);
       new_deck_testcontent "start game creates list of letters A-Z"
       (Generate_letters.remove_let 0 2 temp_deck); *)
  ]

let suite = "test suite for Generate_letters" >::: test_cases
