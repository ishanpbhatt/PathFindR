open Yojson.Basic.Util
open Grid
open Node

type cart = string list

type cart_node = {
  name : string;
  brands : string list;
}

type cart_built = {
  cart_nodes : (string * string list) list;
  preference : string;
  cart : (string * string) list;
}

(* JSON HANDLING *)

let get_node json =
  let name = json |> member "name" |> to_string in
  let brands =
    json |> member "brands" |> to_list |> List.map to_string
  in
  (name, brands)

let from_json_built json : cart_built =
  {
    cart_nodes = json |> member "nodes" |> to_list |> List.map get_node;
    preference = "NONE";
    cart = [];
  }

(* CART BUILDING *)

let rec is_in_cart cart item brand =
  match cart with
  | (a, b) :: t when a = brand && b = item -> true
  | (a, b) :: t -> is_in_cart t item brand
  | [] -> false

let cart_printer c_built =
  print_endline "Here is your cart:";
  let rec cart_printer_helper cart =
    match cart with
    | h :: t ->
        print_endline (snd h ^ " " ^ fst h);
        cart_printer_helper t
    | [] -> ()
  in
  if List.length c_built.cart = 0 then
    ANSITerminal.print_string [ ANSITerminal.red ] "EMPTY CART\n";
  if List.length c_built.cart != 0 then cart_printer_helper c_built.cart

let rec string_list_printer str_lst item cart =
  match str_lst with
  | [ h ] when is_in_cart cart item h ->
      ANSITerminal.print_string [ ANSITerminal.red ] (h ^ "\n")
  | [ h ] -> print_endline h
  | h :: t when is_in_cart cart item h ->
      ANSITerminal.print_string [ ANSITerminal.red ] h;
      print_string ", ";
      string_list_printer t item cart
  | h :: t ->
      print_string (h ^ ", ");
      string_list_printer t item cart
  | [] -> ()

let display_options c_built =
  print_endline "Here are the options for your cart: ";
  let rec item_printer cart_nodes =
    match cart_nodes with
    | h :: t ->
        ANSITerminal.print_string [ ANSITerminal.green ] (fst h);
        print_string "\n  Brands: ";
        string_list_printer (snd h) (fst h) c_built.cart;
        item_printer t
    | [] -> ()
  in
  item_printer c_built.cart_nodes

let rec input_matcher cart_nodes input =
  match cart_nodes with
  | h :: t when fst h = input -> snd h
  | h :: t -> input_matcher t input
  | [] -> failwith "not possible"

let add_to_cart c_built (item : string) (brand : string) =
  if is_in_cart c_built.cart item brand then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nThis item and brand combination is already in the cart.\n";
    c_built)
  else (
    print_endline
      ("\n" ^ brand ^ " " ^ item ^ " has been added to your cart.");
    {
      cart_nodes = c_built.cart_nodes;
      preference = c_built.preference;
      cart = (item, brand) :: c_built.cart;
    })

let rec brand_handler c_built input =
  let brands = input_matcher c_built.cart_nodes input in
  print_string "Enter [";
  ANSITerminal.print_string [ ANSITerminal.green ] "BRAND NAME";
  print_string "] to finish adding an item to your cart.\n";
  print_string "Enter [";
  ANSITerminal.print_string [ ANSITerminal.red ] "BACK";
  print_string "] to not add this item.";
  ANSITerminal.print_string [ ANSITerminal.green ] "\n> ";
  match String.uppercase_ascii (read_line ()) with
  | "BACK" ->
      print_endline "Exiting brand selector";
      c_built
  | brand when List.mem brand brands -> add_to_cart c_built input brand
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nNot a valid brand!";
      brand_handler c_built input

let rec is_item cart_nodes input =
  match cart_nodes with
  | h :: t when fst h = input -> true
  | h :: t -> is_item t input
  | [] -> false

let rec remove_item cart item acc =
  match cart with
  | h :: t when fst h ^ " " ^ snd h = item -> t @ acc
  | h :: t -> remove_item t item (h :: acc)
  | [] -> acc

let remove_check (c_built : cart_built) item =
  let rec item_brand_finder cart item =
    match cart with
    | h :: t when snd h ^ " " ^ fst h = item ->
        ANSITerminal.print_string [ ANSITerminal.green ] item;
        print_endline "\n was removed!";
        let new_cart = remove_item c_built.cart item [] in
        {
          cart_nodes = c_built.cart_nodes;
          preference = c_built.preference;
          cart = new_cart;
        }
    | h :: t -> item_brand_finder t item
    | [] ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nYour remove command was not valid.\n";
        c_built
  in
  item_brand_finder c_built.cart item

let input_parser c_built input =
  match input with
  | input when String.sub input 0 4 = "ADD " ->
      let item_name = String.sub input 4 (String.length input - 4) in
      if is_item c_built.cart_nodes item_name = false then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nYour add command is not valid\n";
        c_built)
      else brand_handler c_built item_name
  | input when String.sub input 0 7 = "REMOVE " ->
      let item_tup = String.sub input 7 (String.length input - 7) in
      remove_check c_built item_tup
  | input ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Your input is not valid\n";
      c_built

let get_cart_built c_built = c_built.cart
