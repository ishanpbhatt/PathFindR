open Yojson.Basic.Util
open Grid
open Node

type cart = node ref list

type store_node = {
  row : int;
  col : int;
  node_type : string;
  name : string;
  brands_prices : (string * float) list;
}

type store_objects = {
  store_nodes : store_node list;
  node_cart : node ref list;
  cart : (string * string) list;
}

let get_cart st_obj = st_obj.cart

let get_brands_of_item st_obj item_name =
  let store_nodes = st_obj.store_nodes in
  let rec find_brands nodes =
    match nodes with
    | [] -> []
    | node :: t when node.name = item_name ->
        let rec extract_brand_names brand_prices_list acc =
          match brand_prices_list with
          | [] -> acc
          | (brand, price) :: t -> extract_brand_names t (brand :: acc)
        in
        extract_brand_names node.brands_prices []
    | node :: t -> find_brands t
  in
  find_brands store_nodes

let get_specific_price
    (st_obj : store_objects)
    (item_name : string)
    (item_brand : string) : float =
  let store_nodes = st_obj.store_nodes in
  let rec find_item nodes =
    match nodes with
    | [] -> failwith "invalid json"
    | node :: t when node.name = item_name ->
        let node_brand_prices = node.brands_prices in
        let rec find_item (brand_prices : (string * float) list) =
          match brand_prices with
          | [] -> failwith "invalid json inside"
          | (brand, price) :: t when brand = item_brand -> price
          | (brand, price) :: t -> find_item t
        in
        find_item node_brand_prices
    | node :: t -> find_item t
  in
  find_item store_nodes

(* JSON HANDLING *)
let get_brands_prices json =
  let prices = json |> member "price" |> to_list |> List.map to_float in
  let brands =
    json |> member "brands" |> to_list |> List.map to_string
  in
  List.combine brands prices

let get_node json : store_node =
  {
    row = json |> member "row" |> to_int;
    col = json |> member "col" |> to_int;
    node_type = json |> member "type" |> to_string;
    name = json |> member "name" |> to_string;
    brands_prices = get_brands_prices json;
  }

let from_json json : store_objects =
  {
    store_nodes = json |> member "nodes" |> to_list |> List.map get_node;
    node_cart = [];
    cart = [];
  }

(* STORE CREATION *)

let rec find_entrance store_nodes =
  match store_nodes with
  | [] -> failwith "bad json"
  | h :: t when h.node_type = "START/EXIT" -> (h.row, h.col)
  | h :: t -> find_entrance t

let rec find_meta_data store_nodes =
  match store_nodes with
  | [] -> failwith "bad json"
  | h :: t when h.node_type = "METADATA" -> (h.row, h.col)
  | h :: t -> find_meta_data t

let find_store_name st_obj =
  let rec store_name_helper store_nodes =
    match store_nodes with
    | [] -> failwith "bad json"
    | h :: t when h.node_type = "METADATA" -> h.name
    | h :: t -> store_name_helper t
  in
  store_name_helper st_obj.store_nodes

let rec create_nodes (store_nodes : store_node list) acc =
  match store_nodes with
  | [] -> acc
  | h :: t when h.node_type = "METADATA" -> create_nodes t acc
  | h :: t ->
      let node_ref =
        ref
          (create_node h.row h.col h.node_type h.name [] max_int max_int
             None)
      in
      create_nodes t (node_ref :: acc)

let rec update_grid grid nodes =
  match nodes with
  | h :: t ->
      add_node_to_grid h grid;
      update_grid grid t
  | [] -> ()

let create_store st_obj =
  let grid_size = find_meta_data st_obj.store_nodes in
  let grid = init_grid (fst grid_size) (snd grid_size) in
  let s_nodes = create_nodes st_obj.store_nodes [] in
  update_grid grid s_nodes;
  grid

(* CART HANDLING *)
let get_store_nodes st_obj = st_obj.store_nodes

let get_node_cart st_obj = st_obj.node_cart

let rec is_in_cart item brand cart =
  match cart with
  | (a, b) :: t when a = item && b = brand -> true
  | h :: t -> is_in_cart item brand t
  | [] -> false

let rec print_brands_prices brands_prices item st_obj =
  match brands_prices with
  | (a, b) :: t ->
      if is_in_cart item a st_obj.cart then
        ANSITerminal.print_string [ ANSITerminal.red ]
          ("  " ^ String.uppercase_ascii a);
      if is_in_cart item a st_obj.cart = false then
        print_string ("  " ^ String.uppercase_ascii a);
      ANSITerminal.print_string [ ANSITerminal.red ] ": ";
      print_float b;
      print_endline ",";
      print_brands_prices t item st_obj
  | [] -> ()

let print_items st_obj =
  let rec print_items_helper store_nodes =
    match store_nodes with
    | [] -> ()
    | h :: t when h.node_type = "SHELF" ->
        ANSITerminal.print_string [ ANSITerminal.green ] h.name;
        print_string "\n Brands";
        ANSITerminal.print_string [ ANSITerminal.red ] ":\n";
        print_brands_prices h.brands_prices h.name st_obj;
        print_items_helper t
    | h :: t -> print_items_helper t
  in
  print_items_helper st_obj.store_nodes

let print_cart st_obj =
  if List.length st_obj.cart = 0 then
    ANSITerminal.print_string [ ANSITerminal.red ] "EMPTY CART\n";
  let rec cart_helper cart =
    match cart with
    | [] -> ()
    | h :: t ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          (snd h ^ " " ^ fst h);
        print_endline "";
        cart_helper t
  in
  cart_helper st_obj.cart

let get_coords st_obj input =
  let rec coords_helper store_nodes =
    match store_nodes with
    | h :: t when h.name = input && input != "N/A" -> (h.row, h.col)
    | h :: t -> coords_helper t
    | [] -> failwith "bad json"
  in
  coords_helper st_obj.store_nodes

let is_item st_obj input =
  let rec is_item_helper store_nodes =
    match store_nodes with
    | h :: t when h.name = input && input != "N/A" -> true
    | h :: t -> is_item_helper t
    | [] -> false
  in
  is_item_helper st_obj.store_nodes

let is_valid_brand_string st_obj item input =
  let rec is_valid_brand_helper brands_prices =
    match brands_prices with
    | (a, b) :: t when a = input -> true
    | (a, b) :: t -> is_valid_brand_helper t
    | [] -> false
  in
  let rec find_item_helper store_nodes =
    match store_nodes with
    | h :: t when h.name = item -> is_valid_brand_helper h.brands_prices
    | h :: t -> find_item_helper t
    | [] -> failwith "not possible"
  in
  find_item_helper st_obj.store_nodes

let update_cart st_obj item brand grid =
  let coord_tup = get_coords st_obj item in
  let nd = get_node_from_grid (fst coord_tup) (snd coord_tup) grid in
  {
    store_nodes = st_obj.store_nodes;
    node_cart = nd :: st_obj.node_cart;
    cart = (item, brand) :: st_obj.cart;
  }

let get_brands_string st_obj item =
  let rec help lst acc =
    match lst with [] -> acc | (a, b) :: t -> help t acc ^ ", " ^ a
  in
  let rec find_item_helper store_nodes =
    match store_nodes with
    | h :: t when h.name = item -> help h.brands_prices ""
    | h :: t -> find_item_helper t
    | [] -> failwith "not possible"
  in
  find_item_helper st_obj.store_nodes

let rec brand_handler (st_obj : store_objects) item grid =
  print_endline "";
  print_string "Enter [";
  ANSITerminal.print_string [ ANSITerminal.green ] "BRAND NAME";
  print_string "] to finish adding an item to your cart.\n";
  print_string "Enter [";
  ANSITerminal.print_string [ ANSITerminal.red ] "BACK";
  print_string "] to not add this item.";
  print_string
    ("\nAvailable brands: "
    ^ (String.sub (get_brands_string st_obj item))
        1
        (String.length (get_brands_string st_obj item) - 1));
  ANSITerminal.print_string [ ANSITerminal.green ] "\n> ";
  match String.uppercase_ascii (read_line ()) with
  | "BACK" ->
      print_endline "Exiting brand selector";
      st_obj
  | brand
    when is_valid_brand_string st_obj item brand
         && is_in_cart item brand st_obj.cart = false ->
      update_cart st_obj item brand grid
  | brand when is_valid_brand_string st_obj item brand ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nAlready in your cart!\n";
      brand_handler st_obj item grid
  | input ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nNot a valid brand!\n";
      brand_handler st_obj item grid

let add_to_cart (st_obj : store_objects) (input : string) (grid : grid)
    : store_objects =
  match input with
  | input when is_item st_obj input -> brand_handler st_obj input grid
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nThat's not a valid item.\n";
      st_obj

let rec is_valid_remove_query input cart =
  match cart with
  | h :: t when snd h ^ " " ^ fst h = input -> true
  | h :: t -> is_valid_remove_query input t
  | [] -> false

let rec get_item_brand input cart =
  match cart with
  | h :: t when snd h ^ " " ^ fst h = input -> h
  | h :: t -> get_item_brand input t
  | [] -> failwith "not possible"

let rec rem_cart item_brand cart acc =
  match cart with
  | h :: t when h = item_brand -> acc @ t
  | h :: t -> rem_cart item_brand cart (h :: acc)
  | [] -> acc

let rec rem_node_cart input node_cart acc =
  match node_cart with
  | h :: t when get_name !h = input -> t @ acc
  | h :: t -> rem_node_cart input t (h :: acc)
  | [] -> failwith "broken code"

let remove_from_cart
    (st_obj : store_objects)
    (input : string)
    (grid : grid) : store_objects =
  print_endline input;
  match is_valid_remove_query input st_obj.cart with
  | true ->
      let item_brand = get_item_brand input st_obj.cart in
      let new_cart = rem_cart item_brand st_obj.cart [] in
      let new_node_cart =
        rem_node_cart (fst item_brand) st_obj.node_cart []
      in
      {
        store_nodes = st_obj.store_nodes;
        node_cart = new_node_cart;
        cart = new_cart;
      }
  | false ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nThat's not a valid remove query.\n";
      st_obj

let get_price item_brand st_obj =
  let rec get_price_of_brand brands_prices brand =
    match brands_prices with
    | (a, b) :: t when a = brand -> b
    | h :: t -> get_price_of_brand t brand
    | [] -> failwith "not possible"
  in
  let rec get_price_helper store_nodes =
    match store_nodes with
    | h :: t when h.name = fst item_brand ->
        get_price_of_brand h.brands_prices (snd item_brand)
    | h :: t -> get_price_helper t
    | [] -> failwith "not possible"
  in
  get_price_helper st_obj.store_nodes

let get_total (cart : (string * string) list) st_obj =
  List.fold_left (fun acc item -> acc +. get_price item st_obj) 0. cart

let rec receipt_helper cart acc st_obj =
  match cart with
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        (snd h ^ " " ^ fst h ^ "\n");
      ANSITerminal.print_string [ ANSITerminal.green ]
        "                     $";
      let price = get_price h st_obj in
      ANSITerminal.print_string [ ANSITerminal.green ]
        (string_of_float price);
      print_endline "";
      receipt_helper t (price +. acc) st_obj
  | [] -> acc

let receipt_print_helper st_obj =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "###########################\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "        ##RECEIPT##        \n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "###########################\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "THANKS FOR SHOPPING AT \n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    (String.uppercase_ascii (find_store_name st_obj));
  print_endline "";
  ()

let print_distance_total total path_dist =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "ORDER TOTAL:         $";
  ANSITerminal.print_string [ ANSITerminal.green ]
    (string_of_float total);
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n***************************\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "TOTAL DISTANCE:         ";
  ANSITerminal.print_string [ ANSITerminal.green ]
    (string_of_int path_dist);
  ()

let print_receipt st_obj path_dist =
  receipt_print_helper st_obj;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "###########################\n";
  let total = receipt_helper st_obj.cart 0. st_obj in
  ANSITerminal.print_string [ ANSITerminal.green ]
    "***************************\n";
  print_distance_total total path_dist;
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "###########################\n";
  print_endline "\n";
  ()

(* PATHFINDING *)
let rec permutations lst =
  let rec perm_helper acc lst =
    match lst with
    | h :: t ->
        (acc :: lst) :: List.map (fun y -> h :: y) (perm_helper acc t)
    | [] -> [ [ acc ] ]
  in
  match lst with
  | h :: t -> List.concat (List.map (perm_helper h) (permutations t))
  | [] -> [ lst ]

let get_a_star_tup h1 h2 new_grid =
  let a_node =
    get_node_from_grid (get_row !h1) (get_col !h1) new_grid
  in
  let b_node =
    get_node_from_grid (get_row !h2) (get_col !h2) new_grid
  in
  astar a_node b_node new_grid

let rec find_path path acc grid st_obj =
  let new_grid = create_store st_obj in
  match path with
  | [ h1; h2 ] ->
      let astar_tup = get_a_star_tup h1 h2 new_grid in
      fst astar_tup @ acc
  | h1 :: h2 :: t ->
      let astar_tup = get_a_star_tup h1 h2 new_grid in
      find_path (snd astar_tup :: t)
        (fst astar_tup @ acc)
        new_grid st_obj
  | [] -> acc
  | _ -> failwith "shouldn't happen"

let rec add_s_nodes options s_node acc =
  match options with
  | h :: t -> add_s_nodes t s_node (((s_node :: h) @ [ s_node ]) :: acc)
  | [] -> acc

let rec find_min_path combinations path_len curr_path grid st_obj =
  match combinations with
  | [] -> curr_path
  | h :: t ->
      let path = find_path h [] grid st_obj in
      if List.length path < path_len then
        find_min_path t path_len path grid st_obj
      else find_min_path t (List.length path) curr_path grid st_obj

let min_path st_obj grid =
  let s_node_tup = find_entrance st_obj.store_nodes in
  let s_node =
    get_node_from_grid (fst s_node_tup) (snd s_node_tup) grid
  in
  let perm = permutations st_obj.node_cart in
  let combinations = add_s_nodes perm s_node [] in
  find_min_path combinations max_int [] grid st_obj

let search_test (st_obj : store_objects) grid =
  let a_node = get_node_from_grid 0 0 grid in
  let b_node = get_node_from_grid 3 3 grid in
  let ab = astar a_node b_node grid in
  let poin_b = snd ab in
  let path_one = fst ab in
  let new_grid = create_store st_obj in
  let bn_node =
    get_node_from_grid (get_row !poin_b) (get_col !poin_b) new_grid
  in
  let c_node = get_node_from_grid 0 4 new_grid in
  let cd = fst (astar bn_node c_node new_grid) in
  path_one @ cd

let rec is_valid_brand_tuple brand brand_prices =
  match brand_prices with
  | h :: t when fst h = brand -> true
  | h :: t -> is_valid_brand_tuple brand t
  | [] -> false

let rec is_item_in_store item store_nodes =
  match store_nodes with
  | h :: t
    when h.name = fst item
         && is_valid_brand_tuple (snd item) h.brands_prices ->
      true
  | h :: t -> is_item_in_store item t
  | [] -> false

let set_cart
    (st_obj : store_objects)
    (cart : (string * string) list)
    (grid : Grid.grid) : store_objects =
  let rec cart_updater stobj cart =
    match cart with
    | [] -> stobj
    | h :: t when is_item_in_store h st_obj.store_nodes ->
        let new_st_obj = update_cart stobj (fst h) (snd h) grid in
        cart_updater new_st_obj t
    | h :: t -> cart_updater stobj t
  in
  cart_updater st_obj cart
