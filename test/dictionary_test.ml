open OUnit2
open Koiiword.Dictionary

let test_length
    (name : string)
    (file_name : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal
    (Hashtbl.length (dict_from_file file_name))
    expected_output

let test_word_valid
    (name : string)
    (file_name : string)
    (input_words : string list)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal
    (List.for_all
       (is_word_valid (dict_from_file file_name))
       input_words)
    expected_output

let test_cases =
  [
    test_length "test_dict has length 3" "test_dict.txt" 3;
    test_length "test_dict_2 has length 279496" "test_dict_2.txt" 279496;
    test_word_valid "CLARKSON is valid" "test_dict.txt" [ "CLARKSON" ]
      true;
    test_word_valid "CLARKSON, LowerCase, koiiword is valid"
      "test_dict.txt"
      [ "CLARKSON"; "LowerCase"; "koiiword" ]
      true;
    test_word_valid "LowerCas is invalid" "test_dict.txt" [ "LowerCas" ]
      false;
    test_word_valid "CLARKSON, koiiword, Failing is invalid"
      "test_dict.txt"
      [ "CLARKSON"; "koiiword"; "Failing" ]
      false;
  ]

let suite = "test suite for Dictionary" >::: test_cases
