type dictionary = (string, string) Hashtbl.t
(** type [dictionary] is the type for the HashTable containing the
    dictionary contents (string, string) [(word, word)] *)

(** [dict_from_file] takes in [file_name] string to give a dictionary of
    type [dictionary]*)
let dict_from_file (file_name : string) : dictionary =
  let ic = open_in file_name in
  let ht = Hashtbl.create 400000 in
  try
    while true do
      let line = String.uppercase_ascii (String.trim (input_line ic)) in
      Hashtbl.add ht line line
    done;
    ht
  with End_of_file ->
    close_in ic;
    ht

(** [is_word_valid] takes in a [dictionary] dictionary and a [word]
    string and returns true if [word] is in [dictionary] and false
    otherwise *)
let rec is_word_valid (dict : dictionary) (word_lst : string list) =
  match word_lst with
  | [] -> true
  | h :: t ->
      if Hashtbl.mem dict (String.uppercase_ascii h) then
        is_word_valid dict t
      else false
