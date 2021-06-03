open Yojson.Basic.Util

type uploaded_cart = { cart : (string * string) list }

let create_cart json =
  let item = json |> member "item" |> to_string in
  let brand = json |> member "brand" |> to_string in
  (item, brand)

let from_json_upload json : uploaded_cart =
  { cart = json |> member "nodes" |> to_list |> List.map create_cart }

let rec cart_printer cart =
  match cart with
  | (a, b) :: t ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        (b ^ " " ^ a ^ "\n");
      cart_printer t
  | [] -> ()

let print_uploaded_cart (cart : (string * string) list) : unit =
  print_endline "Here is your selected grocery list: ";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "##################\n";
  ANSITerminal.print_string [ ANSITerminal.green ] "Groceries:\n";
  cart_printer cart;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "##################\n";
  ()

let get_uploaded_cart (up_cart : uploaded_cart) =
  print_uploaded_cart up_cart.cart;
  up_cart.cart
