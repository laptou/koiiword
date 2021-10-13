type player = {
  name : string;
  points : int;
  letters : Generate_letters.letter_deck;
}
(** type [player] defines the player.

    - Has field [name] used to store the player's name
    - Has field [points] used to store how many points they have
    - Has field [letters] used to store the letters they have in their
      hand. *)
