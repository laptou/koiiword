open OUnit2
open Koiiword.Player
open Koiiword.Generate_letters

let rec all_letters = function
  | [] -> true
  | h :: t -> (
      try Char.code h > 64 && Char.code h < 91 && all_letters t
      with Invalid_argument _ -> false)

let next_player_test
    (name : string)
    (cur_player : player)
    (player_lst : player list)
    (expected_player : player) : test =
  name >:: fun _ ->
  assert_equal (next_player cur_player player_lst) expected_player

let player1 = { letters = start_game [] }

let player2 = { letters = start_game [] }

let player3 = { letters = start_game [] }

let player_lst = [ player1; player2; player3 ]

let test_cases =
  [
    next_player_test
      "[player1; player2; player3] player 1 next is player 2" player1
      player_lst player2;
    next_player_test
      "[player1; player2; player3] player 1 next is player 2" player2
      player_lst player3;
    next_player_test
      "[player1; player2; player3] player 1 next is player 2" player3
      player_lst player1;
  ]

let suite = "test suite for Players" >::: test_cases
