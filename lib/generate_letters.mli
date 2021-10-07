type letter_deck = char list

val random_let : unit -> char

val start_game : char list -> char list

val remove_let : int -> int -> letter_deck -> letter_deck

val index_of_letter : int -> char -> letter_deck -> int

(*USE THIS!*)
val optimize_start : unit -> char list

(*USE THIS!*)
val optimize_let : int -> int -> letter_deck -> letter_deck
