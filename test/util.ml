let pp_list pp_item lst =
  "[" ^ String.concat ", " (List.map pp_item lst) ^ "]"

let pp_char ch = "'" ^ String.make 1 ch ^ "'"
