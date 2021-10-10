open OUnit2
open Koiiword.Player
open Koiiword.Generate_letters

let update_player_list_test
    (name : string)
    (player_lst : player list)
    (expected_player_list : player list) : test =
  name >:: fun _ ->
  assert_equal (update_player_list player_lst) expected_player_list

let player1 = { letters = start_game [] }

let player2 = { letters = start_game [] }

let player3 = { letters = start_game [] }

let player_lst = [ player1; player2; player3 ]

let test_cases =
  [
    update_player_list_test
      "[player1; player2; player3] becomes [player2; player3; player1]"
      [ player1; player2; player3 ]
      [ player2; player3; player1 ];
  ]

let suite = "test suite for Players" >::: test_cases
