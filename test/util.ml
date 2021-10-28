let pp_list pp_item lst =
  "[" ^ String.concat ", " (List.map pp_item lst) ^ "]"

let pp_char ch = "'" ^ String.make 1 ch ^ "'"
let pp_string str = "\"" ^ str ^ "\""

let cmp_set cmp a b = List.sort_uniq cmp a = List.sort_uniq cmp b
