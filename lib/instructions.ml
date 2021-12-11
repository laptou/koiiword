type instructions_state =
  | StartGame
  | EntrySelectStart
  | EntrySelectLetters
  | EntrySelectDirection

let text (state : instructions_state) =
  match state with
  | StartGame ->
      "press [enter] to start entering a word, or [ctrl-c] to exit the \
       game"
  | EntrySelectStart ->
      "press [enter] to start entering a word, or [tab] to return to \
       the origin"
  | EntrySelectDirection ->
      "press [➡/⬆/⬅/⬇] to choose the direction of the word"
  | EntrySelectLetters ->
      "enter letters and press [enter] to complete the word"
