open OUnit2
open Koiiword.Generate_letters
open Util

let is_letter ch = match ch with 'A' .. 'Z' -> true | _ -> false

let pp_deck deck = pp_list pp_char (deck_to_letters deck)

let test_deck_length (deck : letter_deck) : test =
  "letter deck contains 7 chars" >:: fun _ ->
  assert_equal 7 (List.length (deck_to_letters deck))

let test_deck_content (deck : letter_deck) : test =
  "letter deck contains only letters" >:: fun _ ->
  assert_bool
    (Printf.sprintf "letter deck %s contained a non-letter"
       (pp_deck deck))
    (List.for_all is_letter (deck_to_letters deck))

let test_deck_consume_missing (deck : letter_deck) (letter : char) :
    test =
  Printf.sprintf "consuming letter %s in deck %s raises"
    (pp_char letter) (pp_deck deck)
  >:: fun _ ->
  assert_raises Not_found (fun () -> consume_letter letter deck)

let test_deck_consume
    (deck : letter_deck)
    (letter : char)
    (expected : letter_deck) : test =
  Printf.sprintf "replacing letter %s in deck %s returns %s"
    (pp_char letter) (pp_deck deck) (pp_deck expected)
  >:: fun _ ->
  assert_equal expected
    (consume_letter letter deck)
    ~printer:pp_deck

let _ = Random.init 1289301209

(* letter deck w/ this seed is ['F', 'G', 'X', 'L', 'O', 'W', 'A'] *)
let test_deck = new_deck ()

let test_cases =
  [
    test_deck_length test_deck;
    test_deck_content test_deck;
    test_deck_consume test_deck 'W'
      (deck_from_letters [ 'F'; 'G'; 'X'; 'L'; 'O'; 'A' ]);
    test_deck_consume_missing test_deck 'D';
  ]

let suite = "test suite for Generate_letters" >::: test_cases
