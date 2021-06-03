open Node
open Grid
open Yojson
open Store
open Build
open Uploader

(** [dir_contents] returns the paths of all regular files that are *
    contained in [dir]. Each file is a path starting with [dir]. *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

let string_of_list (lst : string list) =
  let rec help tail acc =
    match tail with
    | [] -> acc
    | [ h ] -> acc ^ String.sub h 7 (String.length h - 7)
    | h :: t ->
        help t (acc ^ String.sub h 7 (String.length h - 7) ^ "\n")
  in
  help lst ""

let manage_cart_printer () =
  ANSITerminal.print_string [ ANSITerminal.green ] "#############\n\n";
  print_string "Enter '";
  ANSITerminal.print_string [ ANSITerminal.red ] "ADD";
  print_string " + [";
  ANSITerminal.print_string [ ANSITerminal.green ] "ITEM NAME";
  print_string "]' to add an item to your cart.\n";
  print_string "Enter '";
  ANSITerminal.print_string [ ANSITerminal.red ] "REMOVE";
  print_string " + [";
  ANSITerminal.print_string [ ANSITerminal.green ] "BRAND + ITEM NAME";
  print_string "]' to remove an item from your cart.\n";
  print_string "Enter '";
  ANSITerminal.print_string [ ANSITerminal.red ] "DONE";
  print_string "' when you are satisfied.\n";
  print_string "Enter '";
  ANSITerminal.print_string [ ANSITerminal.red ] "TERMINATE";
  print_string "' to exit the program.\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> ";
  ()

let manage_cart_valid_input input game grid =
  match input with
  | "INIT" -> game
  | input
    when String.length input > 4
         && String.uppercase_ascii (String.sub input 0 4) = "ADD " ->
      let item = String.sub input 4 (String.length input - 4) in
      add_to_cart game item grid
  | input
    when String.length input > 7
         && String.uppercase_ascii (String.sub input 0 7) = "REMOVE " ->
      let item_brand = String.sub input 7 (String.length input - 7) in
      remove_from_cart game item_brand grid
  | input ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nYour command was not valid. Try again.\n";
      game

let rec manage_cart (grid : Grid.grid) game input =
  match input with
  | "TERMINATE" ->
      print_endline "Thanks for checking out PathFindR!";
      exit 0
  | "DONE" ->
      print_endline "";
      game
  | input ->
      let new_game = manage_cart_valid_input input game grid in
      ANSITerminal.print_string [ ANSITerminal.green ] "#############\n";
      print_endline "Here are the options for your cart:";
      print_items new_game;
      ANSITerminal.print_string [ ANSITerminal.green ] "#############\n";
      print_endline "Here is your cart so far: ";
      print_cart new_game;
      manage_cart_printer ();
      manage_cart grid new_game (String.uppercase_ascii (read_line ()))

let rec display_receipt_helper input cart_game path =
  match input with
  | "NO" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nThanks for being sustainble!\n"
  | "YES" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nHere is your receipt;\n";
      print_receipt cart_game (List.length path)
  | input ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nThat wasn't a valid option...\n Try again.\n";
      ANSITerminal.print_string [ ANSITerminal.green ] "> ";
      display_receipt_helper
        (String.uppercase_ascii (read_line ()))
        cart_game path

let rec display_grid_helper input grid node_list path =
  match input with
  | "NO" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nThe store will not be displayed\n"
  | "YES" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nHere is the store with the shortest path:\n";
      print_grid grid node_list path
  | input ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nThat wasn't a valid option...\n Try again.\n";
      ANSITerminal.print_string [ ANSITerminal.green ] "> ";
      display_grid_helper
        (String.uppercase_ascii (read_line ()))
        grid node_list path

let display_grid grid node_list path =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Would you like to see the store?\n";
  print_string "Enter the prompt ";
  ANSITerminal.print_string [ ANSITerminal.green ] "'YES'";
  print_string "or the prompt";
  ANSITerminal.print_string [ ANSITerminal.red ] " 'NO'\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> ";
  display_grid_helper
    (String.uppercase_ascii (read_line ()))
    grid node_list path;
  ()

let display_receipt cart_game path =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Would you like a receipt?\n";
  print_string "Enter the prompt ";
  ANSITerminal.print_string [ ANSITerminal.green ] "'YES'";
  print_string "or the prompt";
  ANSITerminal.print_string [ ANSITerminal.red ] " 'NO'\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> ";
  display_receipt_helper
    (String.uppercase_ascii (read_line ()))
    cart_game path;
  ()

let play_store_helper game : unit =
  let grid = create_store game in
  print_endline ("\nWelcome to " ^ find_store_name game ^ "!");
  let cart_game = manage_cart grid game "INIT" in
  let path = min_path cart_game grid in
  display_grid grid (get_node_cart cart_game) path;
  print_endline "\n";
  display_receipt cart_game path;
  print_endline "\n";
  print_endline "Would you like to shop again?";
  print_string "Enter the prompt ";
  ANSITerminal.print_string [ ANSITerminal.green ] "'CONTINUE'";
  print_string " to keep going!\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> ";
  ()

let rec play_store_game f =
  match f with
  | "DONE" ->
      print_endline "Goodbye!!";
      exit 0
  | _ -> (
      match Yojson.Basic.from_file f with
      | x -> play_store_helper (from_json x)
      | exception _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "That file wasn't valid. Try again:\n";
          print_string "Current options: ";
          print_endline (dir_contents "stores" |> string_of_list);
          print_string "\n";
          ANSITerminal.print_string [ ANSITerminal.green ] "> ";
          play_store_game (String.uppercase_ascii (read_line ())))

let equivalent_lists l1 l2 =
  List.length l1 = List.length l2
  && List.for_all
       (fun elt1 -> List.exists (fun elt2 -> elt2 = elt1) l2)
       l1

let create_stores (cart : (string * string) list) stores =
  let rec set_cart_helper jsons acc =
    match jsons with
    | [] -> acc
    | file :: t ->
        let json = file |> Yojson.Basic.from_file |> from_json in
        let grid = create_store json in
        let st_obj = set_cart json cart grid in
        let has_items = equivalent_lists cart (get_cart st_obj) in
        let total = get_total (get_cart st_obj) st_obj in
        let data =
          ( find_store_name st_obj,
            st_obj,
            grid,
            min_path st_obj grid,
            has_items,
            total )
        in
        set_cart_helper t (data :: acc)
  in
  set_cart_helper stores []

let rec print_available_paths stores =
  match stores with
  | [] -> ()
  | (name, st_obj, grid, min_path, has_items, total) :: t when has_items
    ->
      print_grid grid (get_node_cart st_obj) min_path;
      print_receipt st_obj (List.length min_path);
      print_available_paths t
  | (name, st_obj, grid, min_path, has_items, total) :: t ->
      print_available_paths t

let get_cart_diff
    (cart1 : (string * string) list)
    (cart2 : (string * string) list) =
  let cart1_removed_overlap =
    List.filter
      (fun item -> not (List.exists (fun elt -> elt = item) cart2))
      cart1
  in
  let cart2_removed_overlap =
    List.filter
      (fun item -> not (List.exists (fun elt -> elt = item) cart1))
      cart2
  in
  cart1_removed_overlap @ cart2_removed_overlap

let rec print_cart_diff (cart : (string * string) list) : unit =
  match cart with
  | [] -> ()
  | (item, brand) :: t ->
      print_string (item ^ " " ^ brand ^ "\n");
      print_cart_diff t

let print_price_differential
    (item_name : string)
    (item_brand : string)
    (new_brand : string)
    (st_obj : store_objects) =
  print_string " ";
  let price = get_specific_price st_obj item_name new_brand in
  ANSITerminal.print_string [ ANSITerminal.white ] "for a price of ";
  ANSITerminal.print_string [ ANSITerminal.white ]
    (string_of_float price)

let print_message_with_suggestions
    (item_name : string)
    (item_brand : string)
    (other_brands : string list)
    (st_obj : store_objects) : unit =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Replacements for: " ^ item_name ^ " " ^ item_brand);
  print_endline "";
  let rec print_other_brands brands =
    match brands with
    | [] -> ()
    | new_brand :: t ->
        print_string (item_name ^ " " ^ new_brand);
        print_price_differential item_name item_brand new_brand st_obj;
        print_endline ""
  in
  print_other_brands other_brands;
  print_endline "";
  ()

let print_message_no_suggestions
    (name : string)
    (item_name : string)
    (item_brand : string) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    (name ^ " has no replacements for " ^ item_name ^ " " ^ item_brand);
  print_endline "";
  ()

let print_suggestions
    (cart_diff : (string * string) list)
    (st_obj : store_objects)
    (name : string) : unit =
  let rec suggestions_help cart_diff =
    match cart_diff with
    | [] -> ()
    | (item_name, item_brand) :: t ->
        let other_brands = get_brands_of_item st_obj item_name in
        if List.length other_brands = 0 then
          print_message_no_suggestions name item_name item_brand
        else
          print_message_with_suggestions item_name item_brand
            other_brands st_obj
  in
  suggestions_help cart_diff

let rec show_unavailable_analysis
    (stores :
      (string
      * Store.store_objects
      * Grid.grid
      * Node.node list
      * bool
      * float)
      list)
    (cart : (string * string) list) : unit =
  print_endline "";
  match stores with
  | [] -> ()
  | (name, st_obj, grid, min_path, has_items, total) :: t
    when not has_items ->
      print_endline
        (String.uppercase_ascii name
        ^ " is missing the following items: ");
      let cart_diff = get_cart_diff (get_cart st_obj) cart in
      print_cart_diff cart_diff;
      print_suggestions cart_diff st_obj name;
      show_unavailable_analysis t cart
  | (name, st_obj, grid, min_path, has_items, total) :: t ->
      show_unavailable_analysis t cart

let get_store_total store =
  match store with
  | name, st_obj, grid, min_path, has_items, total -> total

let get_store_name store =
  match store with
  | name, st_obj, grid, min_path, has_items, total -> name

let get_contents a =
  match a with None -> failwith "failed precondition" | Some v -> v

let rec find_cheapest
    (stores :
      (string
      * Store.store_objects
      * Grid.grid
      * Node.node list
      * bool
      * float)
      list)
    (curr_cheapest_price : float option)
    (curr_cheapest_name : string option) =
  match stores with
  | [] -> (curr_cheapest_name, curr_cheapest_price)
  | (name, st_obj, grid, min_path, has_items, total) :: t ->
      if has_items && curr_cheapest_price = None then
        find_cheapest t (Some total) (Some name)
      else if has_items && total <= get_contents curr_cheapest_price
      then find_cheapest t (Some total) (Some name)
      else find_cheapest t curr_cheapest_price curr_cheapest_name

let show_price_analysis
    (stores :
      (string
      * Store.store_objects
      * Grid.grid
      * Node.node list
      * bool
      * float)
      list) =
  let cheapest_name, cheapest_price = find_cheapest stores None None in
  match cheapest_name with
  | None ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, none of the stores have all of the items, so \
         we cannot conduct price analysis.\n"
  | Some name ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Here is the cheapest store: ";
      print_endline (String.uppercase_ascii name);
      ANSITerminal.print_string [ ANSITerminal.green ] "Price: ";
      print_endline (cheapest_price |> get_contents |> string_of_float);
      ()

let rec find_shortest
    (stores :
      (string
      * Store.store_objects
      * Grid.grid
      * Node.node list
      * bool
      * float)
      list)
    (curr_shortest_dist : int option)
    (curr_shortest_name : string option) =
  match stores with
  | [] -> (curr_shortest_name, curr_shortest_dist)
  | (name, st_obj, grid, min_path, has_items, total) :: t ->
      if has_items && curr_shortest_dist = None then
        find_shortest t (Some (List.length min_path)) (Some name)
      else if
        has_items
        && List.length min_path <= get_contents curr_shortest_dist
      then find_shortest t (Some (List.length min_path)) (Some name)
      else find_shortest t curr_shortest_dist curr_shortest_name

let show_time_analysis
    (stores :
      (string
      * Store.store_objects
      * Grid.grid
      * Node.node list
      * bool
      * float)
      list) =
  let shortest_name, shortest_dist = find_shortest stores None None in
  match shortest_name with
  | None ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, none of the stores have all of the items, so \
         we cannot conduct time analysis.\n"
  | Some name ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Here is the store with the least walking distance: ";
      print_endline (String.uppercase_ascii name);
      ANSITerminal.print_string [ ANSITerminal.green ] "Distance: ";
      print_endline (shortest_dist |> get_contents |> string_of_int);
      ()

let analyze_stores (cart : (string * string) list) =
  print_endline "";
  let store_files = dir_contents "medium_stores" in
  let stores = create_stores cart store_files in
  print_available_paths stores;
  show_unavailable_analysis stores cart;
  show_price_analysis stores;
  show_time_analysis stores;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "We hope you found the analysis insightful.\n\
     Thanks for checking out our project!\n";
  exit 0

let rec print_path_from_name store_name stores =
  match stores with
  | [] -> ()
  | (name, st_obj, grid, min_path, has_items, total) :: t
    when store_name = name ->
      print_grid grid (get_node_cart st_obj) min_path;
      print_receipt st_obj (List.length min_path)
  | (name, st_obj, grid, min_path, has_items, total) :: t ->
      print_path_from_name store_name t

let print_path_option (name : string option) stores =
  match name with
  | None -> ()
  | Some name -> print_path_from_name name stores

let analyze_times (cart : (string * string) list) =
  let store_files = dir_contents "medium_stores" in
  let stores = create_stores cart store_files in
  show_unavailable_analysis stores cart;
  let shortest_name, _ = find_shortest stores None None in
  print_path_option shortest_name stores;
  show_time_analysis stores;
  ()

let analyze_prices (cart : (string * string) list) =
  let store_files = dir_contents "medium_stores" in
  let stores = create_stores cart store_files in
  show_unavailable_analysis stores cart;
  let cheapest_name, _ = find_cheapest stores None None in
  print_path_option cheapest_name stores;
  show_price_analysis stores;
  ()

let rec choose_optimizer_matcher (cart : (string * string) list) : unit
    =
  match String.uppercase_ascii (read_line ()) with
  | "DONE" ->
      print_endline "Thanks for checking out our project!";
      exit 0
  | "PRICE" ->
      print_endline "";
      analyze_prices cart;
      ANSITerminal.print_string [ ANSITerminal.green ]
        "We hope you found the analysis insightful.\n\
         Thanks for checking out our project!\n";
      exit 0
  | "TIME" ->
      print_endline "";
      analyze_times cart;
      ANSITerminal.print_string [ ANSITerminal.green ]
        "We hope you found the analysis insightful.\n\
         Thanks for checking out our project!\n";
      exit 0
  | "ANALYZE" -> analyze_stores cart
  | input ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nThat is not a valid input. Try again: ";
      ANSITerminal.print_string [ ANSITerminal.green ] "\n> ";
      choose_optimizer_matcher cart

let choose_optimizer cart =
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.red ] "DONE";
  print_string "' to exit the program.\n";
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.green ] "PRICE";
  print_string "' to optimize for order total.\n";
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.green ] "TIME";
  print_string "' to optimize for total distance travelled.\n";
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.green ] "ANALYZE";
  print_string "' to consider all possiblities.\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "\n> ";
  choose_optimizer_matcher cart;
  ()

let build_game_printer () =
  ANSITerminal.print_string [ ANSITerminal.green ] "#############\n";
  print_string "\nEnter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.red ] "ADD";
  print_string " + [";
  ANSITerminal.print_string [ ANSITerminal.green ] "ITEM NAME";
  print_string "]' to add an item to your cart.\n";
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.red ] "REMOVE";
  print_string " + [";
  ANSITerminal.print_string [ ANSITerminal.green ] "BRAND + ITEM NAME";
  print_string "]' to remove an item from your cart.\n";
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.red ] "DONE";
  print_string "' when you are satisfied with your cart.\n";
  print_string "Enter the prompt '";
  ANSITerminal.print_string [ ANSITerminal.red ] "TERMINATE";
  print_string "' to exit the program.\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "\n> ";
  ()

let rec play_build_game (game : Build.cart_built) =
  display_options game;
  ANSITerminal.print_string [ ANSITerminal.green ] "#############\n";
  cart_printer game;
  build_game_printer ();
  let input = String.uppercase_ascii (read_line ()) in
  if input = "DONE" then (
    print_endline "";
    print_endline
      "Now that you have build your cart, would you like to optimize \
       for total cost or for time.\n\
      \ You can also do an overall analysis.";
    choose_optimizer (get_cart_built game))
  else if input = "TERMINATE" then (
    print_endline "Thanks for checking out PathFindr!";
    exit 0)
  else
    let new_game = input_parser game input in
    play_build_game new_game

let rec play_upload_game f =
  match f with
  | "DONE" ->
      print_endline "Goodbye!!";
      exit 0
  | _ -> (
      match Yojson.Basic.from_file f with
      | x ->
          let json = from_json_upload x in
          let cart = get_uploaded_cart json in
          choose_optimizer cart
      | exception _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "That file wasn't valid. Try again:\n";
          print_string "Current options: ";
          print_endline (dir_contents "clists" |> string_of_list);
          print_string "\n";
          ANSITerminal.print_string [ ANSITerminal.green ] "> ";
          play_upload_game
            ("clists/" ^ String.uppercase_ascii (read_line ())))

let top_half_printer () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "########                               \
     ########                         ** ########\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "##    ##                               \
     ##                               ** ##    ##\n";

  ANSITerminal.print_string [ ANSITerminal.green ]
    "##    ##                               \
     ##                               ** ##    ##\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "##    ##                               \
     ##                               ** ##    ##\n";
  ()

let bottom_printer_helper_one () =
  ANSITerminal.print_string [ ANSITerminal.green ] "######## ";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "******** ******** **    **    ";
  ANSITerminal.print_string [ ANSITerminal.green ] "######## ";
  ANSITerminal.print_string [ ANSITerminal.red ] "******** ******** ";
  ANSITerminal.print_string [ ANSITerminal.green ] "******** ";
  ANSITerminal.print_string [ ANSITerminal.red ] "++++++";
  ANSITerminal.print_string [ ANSITerminal.green ] "##\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "**    ** ******** **    **    ";
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ] "   **    **    ** ";
  ANSITerminal.print_string [ ANSITerminal.green ] "**    ** ";
  ANSITerminal.print_string [ ANSITerminal.red ] "++ ++    \n";
  ()

let bottom_printer_helper_two () =
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "********    **    ********    ";
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ] "   **    **    ** ";
  ANSITerminal.print_string [ ANSITerminal.green ] "**    ** ";
  ANSITerminal.print_string [ ANSITerminal.red ] "++  ++   \n";

  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "**    **    **    **    **    ";
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ] "   **    **    ** ";
  ANSITerminal.print_string [ ANSITerminal.green ] "**    ** ##   ";
  ANSITerminal.print_string [ ANSITerminal.red ] "++\n";
  ()

let bottom_printer_helper_three () =
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "**    **    **    **    **    ";
  ANSITerminal.print_string [ ANSITerminal.green ] "##       ";
  ANSITerminal.print_string [ ANSITerminal.red ] "******** **    ** ";
  ANSITerminal.print_string [ ANSITerminal.green ] "******** ##    ";
  ANSITerminal.print_string [ ANSITerminal.red ] "++\n";
  print_endline "\n\n";
  print_string "Welcome to ";
  ANSITerminal.print_string [ ANSITerminal.green ] "PathFind";
  ANSITerminal.print_string [ ANSITerminal.red ] "R";
  print_string "!\n\n";
  ()

let selector_printer_helper () =
  print_endline
    "Would you like to shop at a specific store, upload a grocery \
     list, or build a grocery cart?";
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.red ] "'DONE'";
  print_string " to exit at any point from the program.\n";
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.green ] "'STORE'";
  print_string " to shop at a specific store\n";
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.green ] "'UPLOAD'";
  print_string " to select a customized grocery list\n";
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.green ] "'BUILD'";
  print_string " to build a grocery cart\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "\n> ";
  ()

let build_from_main_helper () =
  print_string "Welcome to ";
  ANSITerminal.print_string [ ANSITerminal.green ] "CartBuild";
  ANSITerminal.print_string [ ANSITerminal.red ] "R";
  print_endline "!\n";
  play_build_game
    (from_json_built (Yojson.Basic.from_file "cart_builder.json"));
  ()

let upload_from_main_helper () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Please enter the grocery list you would like to select.\n";
  print_endline (dir_contents "clists" |> string_of_list);
  ANSITerminal.print_string [ ANSITerminal.red ] "*  *  *  *\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> ";
  match String.uppercase_ascii (read_line ()) with
  | exception End_of_file -> ()
  | "DONE" ->
      print_endline "Goodbye!!";
      exit 0
  | file_name ->
      play_upload_game ("colists/" ^ file_name);
      ()

let store_from_main_helper () =
  print_string "\n\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Please enter the name of the store you would like to buy from.\n";
  print_endline (dir_contents "stores" |> string_of_list);
  ANSITerminal.print_string [ ANSITerminal.red ] "*  *  *  *\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> ";
  match String.uppercase_ascii (read_line ()) with
  | exception End_of_file -> ()
  | "DONE" ->
      print_endline "Goodbye!!";
      exit 0
  | file_name ->
      play_store_game ("stores/" ^ file_name);
      ()

let rec selector () =
  selector_printer_helper ();
  match String.uppercase_ascii (read_line ()) with
  | "DONE" ->
      print_endline "Goodbye!! Thanks for checking out our project.";
      exit 0
  | "BUILD" -> build_from_main_helper ()
  | "UPLOAD" -> upload_from_main_helper ()
  | "STORE" -> store_from_main_helper ()
  | _ ->
      print_endline "That was a bad input. Try again.";
      selector ()

let main () =
  top_half_printer ();
  bottom_printer_helper_one ();
  bottom_printer_helper_two ();
  bottom_printer_helper_three ();
  selector ();
  match
    String.uppercase_ascii (String.uppercase_ascii (read_line ()))
  with
  | "CONTINUE" ->
      print_string "\n\n";
      selector ()
  | _ ->
      print_endline "Thanks for using our project!";
      ()

(* Execute the game engine. *)

let () = main ()
