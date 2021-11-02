type dictionary = (string, string) Hashtbl.t
(* type [dictionary] is the type for the HashTable containing the
   dictionary contents *)

val dict_from_file : string -> dictionary
(* [dict_from_file] takes in [file_name] string to give a dictionary of
   type [dictionary]*)

val is_word_valid : dictionary -> string -> bool
(* [is_word_valid] takes in a [dictionary] dictionary and a [word]
   string and returns true if [word] is in [dictionary] and false
   otherwise *)
