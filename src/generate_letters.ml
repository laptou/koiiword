(** letter_gen is a list containing 7 random chars at any time.
    To be used in a game of scrabble. If the char has been used in the game,
    there will be an '\000' in its place.
    Requires each char in the list to be an alphabetical letter or '\000'. *)
type letter_gen = char list

(** [random_let] is a randomly generated char from a-z.
    random_let : char*)
let random_let : char = 
  let rand_int = (Random.int 26) + 65 in 
    Char.chr rand_int

(** [start_game] is a letter_gen of 7 chars.  
    Requires lst to be empty since it is the start of the game.
    start_game : 'a list -> letter_gen *)
let rec start_game = function
  | [] -> start_game [random_let]
  | h :: t -> if (1 + List.length t) > 7 then h :: t else 
                h :: random_let :: t

(** [replenish_let] refills the letter generation data structure 
    when players use a letter. 
    replenish_lst : letter_gen -> letter_gen *)
let rec replenish_let  = function
  | [] ->  start_game []
  | h :: t -> if h = '\000' then (random_let :: replenish_let t) else 
                replenish_let t

(** [remove_let] removes the appropriate letter when players use a letter *)