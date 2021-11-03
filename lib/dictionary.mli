type dictionary = (string, unit) Hashtbl.t
(** type [dictionary] is the type for the HashTable containing the
    dictionary contents *)

val dict_from_file : string -> dictionary
(** [dict_from_file] takes in [file_name] string to give a dictionary of
    type [dictionary]*)

val is_word_valid : dictionary -> string list -> bool
(** [is_word_valid] takes in a [dictionary] dictionary and a string list
    [word_lst] and returns true if all words in [word_lst] are in
    [dictionary] and false otherwise *)
