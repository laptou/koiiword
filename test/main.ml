(** testing*)
open OUnit2
open GenerateLetters

(** [char_to_string_list] is a string list representation of a char list*)
let rec char_to_string_list = function 
  | [] -> []
  | h :: t -> (Char.escaped h) :: (char_to_string_list t)

let pp_string s = "\"" ^ s ^ "\""  

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. TAKEN FROM A2*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_h2 :: _t as t') ->
          if n = 100 then acc ^ "..."
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"
  
let starting_letters = start_game []
let print_start = print_endline(pp_list pp_string (char_to_string_list starting_letters))

let start_game_test (name : string) : test = 
  name >:: fun _ ->
    assert_equal starting_letters starting_letters
