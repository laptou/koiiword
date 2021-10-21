open OUnit2
open Koiiword.Board
open Util

let rec add_word board word direction position =
  match word with
  | [] -> ()
  | letter :: remainder -> (
      (match Hashtbl.find_opt board.tiles position with
      | None -> Hashtbl.add board.tiles position letter
      | Some letter ->
          if letter != letter then failwith "mismatched letter" else ());
      let row, col = position in
      match direction with
      | Vertical -> add_word board remainder direction (row + 1, col)
      | Horizontal -> add_word board remainder direction (row, col + 1))

let get_words_test (board : board) (expected : string list) : test =
  let test_name =
    Printf.sprintf "board has words %s" (pp_list pp_string expected)
  in
  test_name >:: fun _ ->
  assert_equal expected (get_words board) ~printer:(pp_list pp_string)

let test_board_1 = new_board ()

let get_words_tests =
  "test suite for Board.get_words"
  >::: [ get_words_test test_board_1 [] ]

let suite =
  "test suite for Koiiword.Board functions" >::: [ get_words_tests ]
