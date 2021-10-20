open OUnit2
open Koiiword.Player
open Koiiword.Generate_letters

let player1 = { name = "P1"; points = 50; letters = new_deck () }

let player2 = { name = "P2"; points = 30; letters = new_deck () }

let player3 = { name = "P3"; points = 70; letters = new_deck () }

let player_lst = [ player1; player2; player3 ]

let test_cases =
  [
  ]

let suite = "test suite for Players" >::: test_cases
