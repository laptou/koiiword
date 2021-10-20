open OUnit2
open Koiiword.Player
open Koiiword.Generate_letters
open Koiiword.State

let test_next_player
    (name : string)
    (curr_player : player)
    (players : player list)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal (next_player curr_player players) expected_output

let player1 = { name = "P1"; points = 50; letters = start_game () }

let player2 = { name = "P2"; points = 30; letters = start_game () }

let player3 = { name = "P3"; points = 70; letters = start_game () }

let player_lst = [ player1; player2; player3 ]

let test_cases =
  [
    test_next_player "P1 next player is P2" player1 player_lst player2;
    test_next_player "P2 next player is P3" player2 player_lst player3;
    test_next_player "P3 next player is P1" player3 player_lst player1;
  ]

let suite = "test suite for Players" >::: test_cases
