type player = {
  name : string;
  points : int;
  letters : Generate_letters.letter_deck;
}
(** type [player] defines the player.

    - Has field [letters] used to store the letters they have in their
      hand. *)
