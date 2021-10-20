open Layout

type tile = char * position

let make_tile letter pos =
  match letter with 'A' .. 'Z' -> Some (letter, pos) | _ -> None
