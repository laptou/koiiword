open OUnit2
open Koiiword.Board
open Koiiword.Util
open Koiiword.Layout
open Util

let rec add_word word direction position board =
  match word with
  | [] -> board
  | letter :: remainder -> (
      (match Hashtbl.find_opt board.tiles position with
      | None -> Hashtbl.add board.tiles position letter
      | Some existing_letter ->
          if existing_letter != letter then failwith "mismatched letter"
          else ());
      let row, col = position in
      match direction with
      | Vertical -> add_word remainder direction (row + 1, col) board
      | Horizontal -> add_word remainder direction (row, col + 1) board)

let rec add_words words_with_positions board =
  match words_with_positions with
  | [] -> board
  | (word, direction, position) :: t ->
      add_words t (add_word (explode word) direction position board)

let pp_board (board : board) =
  let cols =
    Seq.map (fun (_, col) -> col) (Hashtbl.to_seq_keys board.tiles)
  in
  let rows =
    Seq.map (fun (row, _) -> row) (Hashtbl.to_seq_keys board.tiles)
  in
  let min_col = Seq.fold_left min 0 cols in
  let max_col = Seq.fold_left max 0 cols in
  let min_row = Seq.fold_left min 0 rows in
  let max_row = Seq.fold_left max 0 rows in
  let rec pp_cols (current_row : int) (current_col : int) =
    if current_col > max_col then ""
    else
      let current_tile =
        match
          Hashtbl.find_opt board.tiles (current_row, current_col)
        with
        | None -> " "
        | Some ch -> String.make 1 ch
      in
      current_tile ^ " " ^ pp_cols current_row (current_col + 1)
  in
  let rec pp_rows (current_row : int) =
    if current_row > max_row then ""
    else pp_cols current_row min_col ^ "\n" ^ pp_rows (current_row + 1)
  in
  pp_rows min_row

let get_words_deep_test (board : board) (expected : string list) : test
    =
  let test_name =
    Printf.sprintf "board has words %s" (pp_list pp_string expected)
  in
  test_name >:: fun _ ->
  assert_equal expected (get_words_deep board)
    ~printer:(pp_list pp_string) ~cmp:(cmp_set compare)

let get_words_deep_disconnected_test (board : board) : test =
  let test_name = Printf.sprintf "board raises Disconnected" in
  test_name >:: fun _ ->
  assert_raises Disconnected (fun _ -> get_words_deep board)

let get_words_at_test
    (board : board)
    (position : position)
    (expected : (string * axis) list) : test =
  let test_name =
    Printf.sprintf "board has words %s at position %s"
      (pp_list pp_string (List.map (fun x -> fst x) expected))
      (pp_position position)
  in
  test_name >:: fun _ ->
  assert_equal expected
    (get_words_at board position)
    ~cmp:(cmp_set compare)

let _ =
  () 
  (*
            I               
            N   H           
            F   A           
        A B L A T E         
        B   A         I N T 
        R   T H O U G H     
        A   E       A       
        S           T       
  I N C I N E R A T E
        V                   
        E    
  *)
  [@ocamlformat "disable"]

let test_board_1 =
  add_words
    [
      ("ABLATE", Horizontal, (0, 0));
      ("ABRASIVE", Vertical, (0, 0));
      ("HAT", Vertical, (-2, 4));
      ("INFLATE", Vertical, (-3, 2));
      ("THOUGH", Horizontal, (2, 2));
      ("INCINERATE", Horizontal, (5, -3));
      ("GATE", Vertical, (2, 6));
      ("INT", Horizontal, (1, 7));
    ]
    (new_board ())

let test_board_2 =
  add_words
    [
      ("ABLATE", Horizontal, (0, 0));
      ("ABRASIVE", Vertical, (0, 0));
      ("HAT", Vertical, (-2, 4));
      ("INFLATE", Vertical, (-3, 2));
      ("THOUGH", Horizontal, (2, 2));
      ("INCINERATE", Horizontal, (5, -3));
      ("GATE", Vertical, (2, 6));
      ("INT", Horizontal, (1, 7));
      ("DISCONNECTED", Vertical, (1, 100));
    ]
    (new_board ())

let test_board_3 =
  add_words [ ("ABLATE", Horizontal, (0, 0)) ] (new_board ())

let get_words_tests =
  "test suite for Board.get_words"
  >::: [
         get_words_deep_test test_board_1
           [
             "ABLATE";
             "ABRASIVE";
             "HAT";
             "INFLATE";
             "THOUGH";
             "INCINERATE";
             "GATE";
             "INT";
             "IH";
           ];
         get_words_deep_test test_board_3 [ "ABLATE" ];
         get_words_deep_disconnected_test test_board_2;
         get_words_at_test test_board_1 (0, 0)
           [ ("ABLATE", Horizontal); ("ABRASIVE", Vertical) ];
         get_words_at_test test_board_1 (0, 1)
           [ ("ABLATE", Horizontal) ];
       ]

let suite =
  "test suite for Koiiword.Board functions" >::: [ get_words_tests ]
