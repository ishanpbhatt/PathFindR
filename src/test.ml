(*In order to ensure the functions we wrote work to our specification,
  we combined play, black box, and white box testing throughout the
  progress of the project.  In the test.ml file, there are tests for the
  node.ml, grid.ml, and store.ml files.  These are the three main files
  that allow our project to run. Many functions in these files are type
  unit, so they either print out something or modify a data structure.  
  These are the ones that have been play tested using “make play”, to
  ensure it produces the desired output.  For the rest of the functions
  that are not type unit, they have been tested by either instantiating
  an object that it creates, or by calling functions on these objects to
  ensure it is the same as the desired output.  By nature of our
  project, much testing was done by play testing, this was sufficient as
  the front-end of our project runs flawlessly and exactly how we
  intended.  *)

open OUnit2
open Grid
open Node
open Store
open Yojson.Basic.Util

let node00 = create_node 0 0 "START/EXIT" "N/A" [] max_int max_int None

let node01 =
  create_node 0 1 "SHELF" "OREOS"
    [ ("NORMAL", 1.00); ("DOUBLE STUFFED", 2.00) ]
    max_int max_int None

let node02 = create_node 0 2 "OPEN" "N/A" [] max_int max_int None

let node03 = create_node 0 3 "OPEN" "N/A" [] max_int max_int None

let node04 = create_node 0 4 "OPEN" "N/A" [] max_int max_int None

let node05 = create_node 0 5 "OPEN" "N/A" [] max_int max_int None

let node06 = create_node 0 6 "OPEN" "N/A" [] max_int max_int None

let node07 = create_node 0 7 "OPEN" "N/A" [] max_int max_int None

let node08 = create_node 0 8 "OPEN" "N/A" [] max_int max_int None

let node09 = create_node 0 9 "OPEN" "N/A" [] max_int max_int None

let node19 = create_node 1 9 "OPEN" "N/A" [] max_int max_int None

let node29 = create_node 2 9 "OPEN" "N/A" [] max_int max_int None

let node39 = create_node 3 9 "OPEN" "N/A" [] max_int max_int None

let node49 = create_node 4 9 "OPEN" "N/A" [] max_int max_int None

let node59 = create_node 5 9 "OPEN" "N/A" [] max_int max_int None

let node69 = create_node 6 9 "OPEN" "N/A" [] max_int max_int None

let node79 = create_node 7 9 "OPEN" "N/A" [] max_int max_int None

let node89 = create_node 8 9 "OPEN" "N/A" [] max_int max_int None

let node99 = create_node 9 9 "OPEN" "N/A" [] max_int max_int None

let node10 = create_node 1 0 "WALL" "N/A" [] max_int max_int None

let node11 = create_node 1 1 "START/EXIT" "N/A" [] max_int max_int None

let node21 = create_node 2 1 "OPEN" "N/A" [] max_int max_int None

let node12 = create_node 1 2 "OPEN" "N/A" [] max_int max_int None

let node89 = create_node 8 9 "OPEN" "N/A" [] max_int max_int None

let node98 = create_node 9 8 "OPEN" "N/A" [] max_int max_int None

let node99 = create_node 9 9 "OPEN" "N/A" [] max_int max_int None

let grid1 = init_grid 10 10

let grid2 = init_grid 2 2

let grid3 = init_grid 3 3

let test_json = Yojson.Basic.from_file "stores/test.json"

let store_obj = from_json test_json

let store_nodes = get_store_nodes store_obj

let store_grid = create_store store_obj

let get_node_from_grid_test (name : string) row col grd expected_output
    : test =
  name >:: fun _ ->
  assert_equal expected_output (get_node_from_grid row col grd)

let get_neighbors_test (name : string) node grd expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (get_neighbors node grd)

let get_node_coords nodes =
  let rec help lst acc =
    match lst with
    | [] -> acc
    | h :: t -> help t (acc @ [ (get_row h, get_col h) ])
  in
  help nodes []

let astar_test
    (name : string)
    s
    e
    grd
    (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (astar s e grd |> fst |> get_node_coords)

let get_node_field_test (name : string) node f expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f node)

let get_brands_prices_json_test (name : string) json expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Store.get_brands_prices json)

let find_store_name_test (name : string) st_obj expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (find_store_name st_obj)

let is_valid_brand_test name st_obj item input expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (is_valid_brand_string st_obj item input)

let find_enterance_test (name : string) store_nodes expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (find_entrance store_nodes)

let find_metadata_test (name : string) store_nodes expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (find_meta_data store_nodes)

let get_price_test (name : string) tup st_obj expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_price tup st_obj)

let get_item_brand_test (name : string) str tup_list expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Store.get_item_brand str tup_list)

let is_valid_remove_query_test name str tup_list expected_output : test
    =
  name >:: fun _ ->
  assert_equal expected_output (is_valid_remove_query str tup_list)

let remove_from_cart_test name st_obj str grd expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (remove_from_cart st_obj str grd)

let get_add_node_tests =
  [
    get_node_from_grid_test "Top left" 0 0
      (add_node_to_grid (ref node00) grid1;
       grid1)
      (ref node00);
    get_node_from_grid_test "Bottom right" 9 9
      (add_node_to_grid (ref node99) grid1;
       grid1)
      (ref node99);
  ]

let get_neighbors_tests =
  [
    get_neighbors_test "Top left neighbors" (ref node00)
      (add_node_to_grid (ref node00) grid1;
       add_node_to_grid (ref node01) grid1;
       add_node_to_grid (ref node10) grid1;
       grid1)
      [ ref node10; ref node01 ];
    get_neighbors_test "Bottom right neighbors" (ref node99)
      (add_node_to_grid (ref node99) grid1;
       add_node_to_grid (ref node89) grid1;
       add_node_to_grid (ref node98) grid1;
       grid1)
      [ ref node89; ref node98 ];
    get_neighbors_test "middle neighbors" (ref node11)
      (add_node_to_grid (ref node11) grid3;
       add_node_to_grid (ref node12) grid3;
       add_node_to_grid (ref node21) grid3;
       add_node_to_grid (ref node01) grid3;
       add_node_to_grid (ref node10) grid3;
       grid1)
      [ ref node21; ref node12; ref node01; ref node10 ];
  ]

let grid = init_grid 2 2

let lgrid = init_grid 10 10

let lgrid2 = init_grid 10 10

let astar_tests =
  [
    astar_test "simple 2by2"
      (get_node_from_grid 0 0 grid)
      (get_node_from_grid 1 1 grid)
      grid [ (0, 0); (1, 0) ];
    astar_test "simple 2by2 adjacent"
      (get_node_from_grid 0 0 grid)
      (get_node_from_grid 0 1 grid)
      grid [ (0, 0) ];
    astar_test "large 10by10 adjacent"
      (get_node_from_grid 0 0 lgrid)
      (get_node_from_grid 8 8 lgrid)
      lgrid
      [
        (0, 0);
        (1, 0);
        (2, 0);
        (3, 0);
        (4, 0);
        (5, 0);
        (6, 0);
        (7, 0);
        (8, 0);
        (8, 1);
        (8, 2);
        (8, 3);
        (8, 4);
        (8, 5);
        (8, 6);
        (8, 7);
      ];
    astar_test "large 10by10 walls"
      (get_node_from_grid 0 0 lgrid2)
      (get_node_from_grid 5 5 lgrid2)
      (add_node_to_grid (ref node10) lgrid2;
       lgrid2)
      [
        (0, 0);
        (0, 1);
        (1, 1);
        (2, 1);
        (3, 1);
        (4, 1);
        (5, 1);
        (5, 2);
        (5, 3);
        (5, 4);
      ];
  ]

let get_node_field_tests =
  [
    get_node_field_test "0, 0" node00 get_row 0;
    get_node_field_test "0, 0" node00 get_col 0;
    get_node_field_test "0, 1" node01 get_row 0;
    get_node_field_test "0, 1" node01 get_col 1;
    get_node_field_test "1, 0" node10 get_row 1;
    get_node_field_test "1, 0" node10 get_col 0;
    get_node_field_test "1, 1" node11 get_row 1;
    get_node_field_test "1, 1" node11 get_col 1;
    get_node_field_test "typ 0,0" node00 get_typ "START/EXIT";
    get_node_field_test "typ 0, 1" node01 get_typ "SHELF";
    get_node_field_test "typ 1, 0" node10 get_typ "WALL";
    get_node_field_test "typ 1, 1" node11 get_typ "START/EXIT";
    get_node_field_test "name 0, 0" node00 get_name "N/A";
    get_node_field_test "name 0, 1" node01 get_name "OREOS";
    get_node_field_test "name 1, 0" node10 get_name "N/A";
    get_node_field_test "name 1, 1" node11 get_name "N/A";
    get_node_field_test "gscore 0,0" node00 get_g_score max_int;
    get_node_field_test "gscore 0,1" node01 get_g_score max_int;
    get_node_field_test "gscore 1,0" node10 get_g_score max_int;
    get_node_field_test "gscore 1, 1" node11 get_g_score max_int;
    get_node_field_test "fscore 0, 0" node00 get_f_score max_int;
    get_node_field_test "fscore 0, 1" node01 get_f_score max_int;
    get_node_field_test "fscore 1, 0" node10 get_f_score max_int;
    get_node_field_test "fscore 1, 1" node11 get_f_score max_int;
    get_node_field_test "came 0, 0" node00 get_came_from None;
    get_node_field_test "came 0, 1" node01 get_came_from None;
    get_node_field_test "came 1, 0" node10 get_came_from None;
    get_node_field_test "came 1, 1" node11 get_came_from None;
    get_node_field_test "brands prices 0, 0" node00
      Node.get_brands_prices [];
    get_node_field_test "brands prices 0, 1" node01
      Node.get_brands_prices
      [ ("NORMAL", 1.00); ("DOUBLE STUFFED", 2.00) ];
    get_node_field_test "brands prices 1, 0" node10
      Node.get_brands_prices [];
    get_node_field_test "brands prices 1, 1" node11
      Node.get_brands_prices [];
    get_node_field_test "hscore 0, 0" node00 (h_score node11) 2;
    get_node_field_test "hscore 0, 1" node01 (h_score node11) 1;
    get_node_field_test "hscore 1, 0" node10 (h_score node11) 1;
    get_node_field_test "hscore a1, 1" node11 (h_score node11) 0;
  ]

let grid_tests =
  [ get_add_node_tests; get_neighbors_tests; astar_tests ]

let node_tests = [ get_node_field_tests ]

let store_tests =
  [
    [
      find_store_name_test "Test store name" store_obj "TEST";
      is_valid_brand_test "True oreos brand" store_obj "OREOS"
        "DOUBLE STUFFED" true;
      is_valid_brand_test "False oreos brand" store_obj "OREOS" "DOUBLE"
        false;
      find_enterance_test "Test json" store_nodes (0, 0);
      find_metadata_test "Test json" store_nodes (2, 2);
      get_price_test "Tup 1" ("OREOS", "DOUBLE STUFFED") store_obj 2.00;
      is_valid_remove_query_test "Correct remove query"
        "DOUBLE STUFFED OREOS"
        [ ("OREOS", "DOUBLE STUFFED") ]
        true;
      is_valid_remove_query_test "Incorrect remove query" "OREOS"
        [ ("OREOS", "DOUBLE STUFFED") ]
        false;
      remove_from_cart_test "Simple cart"
        (add_to_cart store_obj "DOUBLE STUFFED OREO" store_grid)
        "DOUBLE STUFFED OREOs" store_grid store_obj;
      get_item_brand_test "Oreo brand" "DOUBLE STUFFED OREO"
        [ ("OREO", "DOUBLE STUFFED"); ("BREAD", "WONDER") ]
        ("OREO", "DOUBLE STUFFED");
    ];
  ]

let tests =
  "test suite for final project"
  >::: List.flatten (grid_tests @ node_tests @ store_tests)

let _ = run_test_tt_main tests
