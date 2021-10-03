open OUnit2
open Koiiword

let rec all_letters = function
  | [] -> true
  | h :: t -> (
      try Char.code h > 64 && Char.code h < 91 && all_letters t
      with Invalid_argument _ -> false)

let start_game_testlength (name : string) (deck : char list) : test =
  name >:: fun _ -> assert_equal true (List.length deck = 7)

let start_game_testcontent (name : string) (deck : char list) : test =
  name >:: fun _ -> assert_equal true (all_letters deck)

let temp_deck = Generate_letters.start_game []

let test_cases =
  [
    (* test start_game*)
    start_game_testlength "start game creates list of length 7"
      temp_deck;
    start_game_testcontent "start game creates list of letters A-Z"
      temp_deck;
    (* test remove_let*)
    start_game_testlength "start game creates list of length 7"
      (Generate_letters.remove_let 0 2 temp_deck);
    start_game_testcontent "start game creates list of letters A-Z"
      (Generate_letters.remove_let 0 2 temp_deck);
  ]

let suite =
  "test suite for generateLetters" >::: List.flatten [ test_cases ]

let _ = run_test_tt_main suite
