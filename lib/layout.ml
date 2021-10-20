open LTerm_geom

(* position is (row, col) *)
type position = int * int

(* A grid layout spec is a list of all of the dividing lines in a
   grid-based layout. each dividing line is represented as a float,
   which is the fraction of the width / height that this dividing line
   is placed from the left / top. *)
type grid_layout_spec = {
  rows : float list;
  cols : float list;
}

(* [get_grid_rect] will return the rectangle that a given grid cell or
   group of grid cells (as specified by [row_start row_span col_start
   col_span]) would occupy given that the entire grid occupies the
   rectangle specified by [bounds]. *)
let get_grid_rect
    (bounds : rect)
    (layout_spec : grid_layout_spec)
    (row_start : int)
    (row_span : int)
    (col_start : int)
    (col_span : int) : LTerm_geom.rect =
  let col_start = col_start - 1 in
  let row_start = row_start - 1 in
  let start_col_breakpoint =
    if col_start < 0 then 0. else List.nth layout_spec.cols col_start
  in
  let end_col_breakpoint =
    if col_start + col_span >= List.length layout_spec.cols then 1.
    else List.nth layout_spec.cols (col_start + col_span)
  in
  let start_row_breakpoint =
    if row_start < 0 then 0. else List.nth layout_spec.rows row_start
  in
  let end_row_breakpoint =
    if row_start + row_span >= List.length layout_spec.rows then 1.
    else List.nth layout_spec.rows (row_start + row_span)
  in
  let bounds_width = float_of_int (bounds.col2 - bounds.col1) in
  let bounds_height = float_of_int (bounds.row2 - bounds.row1) in
  {
    row1 =
      bounds.row1 + int_of_float (start_row_breakpoint *. bounds_height);
    row2 =
      bounds.row1 + int_of_float (end_row_breakpoint *. bounds_height);
    col1 =
      bounds.col1 + int_of_float (start_col_breakpoint *. bounds_width);
    col2 =
      bounds.col1 + int_of_float (end_col_breakpoint *. bounds_width);
  }

(* [inset rect x] returns a new rectangle where each edge has been moved
   inwards by [x] units. *)
let inset rect x : rect =
  {
    row1 = rect.row1 + x;
    row2 = rect.row2 - x;
    col1 = rect.col1 + x;
    col2 = rect.col2 - x;
  }
