type instructions_state =
  | StartGame
  | EntrySelectStart
  | EntrySelectLetters
  | EntrySelectDirection
  | GameWon

let text (state : instructions_state) =
  match state with
  | StartGame ->
      "press [enter] to start entering a word, or [ctrl-c] to exit the \
       game \n\
      \ 1. use arrow keys to move around the board \n\
      \ 2. input letters onto the board by using letter keys on keyboard \n\
      \ 3. To pan around the board, move cursor around or use \
       your mouse to scroll side to side \n\
      \ 4. press [tab] to navigate back to center of the board \n\
      \ 5. press [esc] to delete the word you are currently building \n\
      \ 6. first player to reach 100 points wins!!\n"
  | EntrySelectStart ->
      "press [enter] to start entering a word, or [tab] to return to \
       the origin"
  | EntrySelectDirection ->
      "press [➡/⬆/⬅/⬇] to choose the direction of the word"
  | EntrySelectLetters ->
      "enter letters and press [enter] to complete the word"
  | GameWon -> "press enter to restart the game"
