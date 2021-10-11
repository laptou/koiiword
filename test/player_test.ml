open OUnit2
open Koiiword.Player
open Koiiword.Generate_letters

let advance_players_test
    (name : string)
    (player_lst : player list)
    (expected_player_list : player list) : test =
  name >:: fun _ ->
  assert_equal (advance_players player_lst) expected_player_list

let player1 = { letters = start_game [] }

let player2 = { letters = start_game [] }

let player3 = { letters = start_game [] }

let player_lst = [ player1; player2; player3 ]

let test_cases =
  [
    advance_players_test
      "[player1; player2; player3] becomes [player2; player3; player1]"
      [ player1; player2; player3 ]
      [ player2; player3; player1 ];
    advance_players_test "[] becomes []" [] [];
  ]

let suite = "test suite for Players" >::: test_cases
