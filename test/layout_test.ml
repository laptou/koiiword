open OUnit2
open Koiiword.Layout
open Util

let pp_rect (rect : LTerm_geom.rect) : string =
  Printf.sprintf "<rect pos = (%d, %d), size = (%d, %d)>" rect.col1
    rect.row1 (rect.col2 - rect.col1) (rect.row2 - rect.row1)

let get_grid_rect_test
    bounds
    layout_spec
    row_start
    row_span
    col_start
    col_span
    expected : test =
  let test_name =
    Printf.sprintf
      "get_grid_rect %s { cols = %s, rows = %s } %d %d %d %d gives %s"
      (pp_rect bounds)
      (pp_list string_of_float layout_spec.cols)
      (pp_list string_of_float layout_spec.rows)
      row_start row_span col_start col_span (pp_rect expected)
  in
  test_name >:: fun _ ->
  assert_equal expected
    (get_grid_rect bounds layout_spec row_start row_span col_start
       col_span)
    ~printer:pp_rect

let bounds : LTerm_geom.rect =
  { row1 = 0; row2 = 100; col1 = 0; col2 = 100 }

let bounds2 : LTerm_geom.rect =
  { row1 = 10; row2 = 110; col1 = 10; col2 = 110 }

let layout_spec = { rows = [ 0.3; 0.7 ]; cols = [ 0.2; 0.8 ] }

let get_grid_rect_tests =
  "test suite for Layout.get_grid_rect"
  >::: [
         get_grid_rect_test bounds layout_spec 0 1 0 1
           { row1 = 0; row2 = 30; col1 = 0; col2 = 20 };
         get_grid_rect_test bounds layout_spec 1 1 0 1
           { row1 = 30; row2 = 70; col1 = 0; col2 = 20 };
         get_grid_rect_test bounds layout_spec 0 2 0 1
           { row1 = 0; row2 = 70; col1 = 0; col2 = 20 };
         get_grid_rect_test bounds layout_spec 0 3 0 1
           { row1 = 0; row2 = 100; col1 = 0; col2 = 20 };
         get_grid_rect_test bounds layout_spec 0 4 0 1
           { row1 = 0; row2 = 100; col1 = 0; col2 = 20 };
         get_grid_rect_test bounds layout_spec 0 1 0 3
           { row1 = 0; row2 = 30; col1 = 0; col2 = 100 };
         get_grid_rect_test bounds layout_spec 0 3 0 3
           { row1 = 0; row2 = 100; col1 = 0; col2 = 100 };
         get_grid_rect_test bounds layout_spec 1 1 1 1
           { row1 = 30; row2 = 70; col1 = 20; col2 = 80 };
         get_grid_rect_test bounds2 layout_spec 1 1 1 1
           { row1 = 40; row2 = 80; col1 = 30; col2 = 90 };
       ]

let suite =
  "test suite for Koiiword.Layout functions"
  >::: [ get_grid_rect_tests ]
