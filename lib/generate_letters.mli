type letter_deck = char list

exception Missing_letter

val new_deck : unit -> char list

val replace_letter : char -> letter_deck -> letter_deck
