type dictionary = (string, unit) Hashtbl.t
(** type [dictionary] is the type for the HashTable containing the
    dictionary contents (string, string) [(word, word)] *)

(* Solution derived from -
   https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034
   and modified for Hashtables*)
(** [dict_from_file] takes in [file_name] string to give a dictionary of
    type [dictionary]*)
let dict_from_file (file_name : string) : dictionary =
  let ic = open_in file_name in
  let ht = Hashtbl.create 400000 in
  try
    while true do
      let line =
        input_line ic |> String.trim |> String.uppercase_ascii
      in
      Hashtbl.add ht line ()
    done;
    ht
  with End_of_file ->
    close_in ic;
    ht

(** [is_word_valid] takes in a [dictionary] dictionary and a [word]
    string and returns true if [word] is in [dictionary] and false
    otherwise *)
let is_word_valid (dict : dictionary) (word : string) =
  Hashtbl.mem dict (String.uppercase_ascii word)
