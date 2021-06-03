open Yojson.Basic.Util
open Grid
open Node

(**[cart] is a structure that holds nodes representing certain points in
   a store. Each node contains store items to which [cart] holds in a
   list *)
type cart

(**[store_node] is a structure that holds store-related fields. This
   includes the [row, col] coordinate to where in the store the node is
   located, the type of node [node_type], and a [name] and a
   [brands_prices] atrribute which corrospedes to items located in that
   point of the store *)
type store_node

(**[store_objects] is a structure that contains a list of type
   [store_node], [node_cart] refering to a list of mutable nodes in a
   store, and type [cart] which holds items and their corresponding
   brand name entries *)
type store_objects

(**[init_grid row col] is a 2d array of nodes with [col] nodes in a row
   and [row] rows in the array *)
val get_node_cart : store_objects -> Node.node ref list

(** [get_cart st_obj] is the cart field of the store_objects [st_obj] *)
val get_cart : store_objects -> (string * string) list

(** [get_brands_of_item st_obj item_name] is a string list of brand
    names for the item named [item_name] in the store_node list of the
    store_objects [st_obj] *)
val get_brands_of_item : store_objects -> string -> string list

(**[remove_from_cart st_obj input grid] is a store_objects with the item
   [input] removed from the cart field of [st_obj] if [input] is a valid
   item to be removed, otherwise [st_obj] *)
val remove_from_cart : store_objects -> string -> grid -> store_objects

(** [from_json json] is a store_objects with the assocaited field data
    taken from the JSON file [json] *)
val from_json : Yojson.Basic.t -> store_objects

(** [create_store st_obj] is a grid that represents the store created
    from the data of store_objects [st_obj] *)
val create_store : store_objects -> Grid.grid

(** [print itmes st_obj] prints the current possible from the data of
    store_objects [st_obj]*)
val print_items : store_objects -> unit

(** [print reciept st_obj] prints the current cart from the data of
    store_objects [st_obj]*)
val print_cart : store_objects -> unit

(** [print reciept st_obj] prints the final receipt from the data of
    store_objects [st_obj]*)
val print_receipt : store_objects -> int -> unit

(** [get_specific_price st_obj item_name item_brand] is a float
    representing the price of an item with the name [item_name] and
    brand [item_brand] *)
val get_specific_price : store_objects -> string -> string -> float

(** [get_brands_prices json] is a (string * float) list representing
    different brands and their assocaited prices from a specific store
    created from the JSON file [json] *)
val get_brands_prices : Yojson.Basic.t -> (string * float) list

(** [get_node json] is a single store_node from the JSON file [json] *)
val get_node : Yojson.Basic.t -> store_node

(** [find_entrance store_nodes] is an (int * int) tuple representing the
    row and column coordinates of a specific node in [store_node list]
    that serves as the store's extrance and exit *)
val find_entrance : store_node list -> int * int

(** [find_meta_data store_nodes] is an (int * int) tuple representing
    the row and column coordinates of a specific node in
    [store_node list] that has type ["METADATA"] *)
val find_meta_data : store_node list -> int * int

(** [find_store_name st_obj] is a string that represents a given store's
    name from store_objects [st_obj] *)
val find_store_name : store_objects -> string

(**[init_grid row col] is a node ref list with [col] nodes in a row and
   [row] rows in the array *)
val get_node_cart : store_objects -> Node.node ref list

(** [is_in_cart item brand cart] is true if a given item with name
    [item] and brand [brand] is currently in the cart [cart], otherwise
    false *)
val is_in_cart : string -> string -> (string * string) list -> bool

(** [get_coords st_obj input] is a (int * int) tuple representing the
    row and column corrdinates of a given node with name [input] *)
val get_coords : store_objects -> string -> int * int

(** [is_item st_obj input] is true if a given node with name [input] is
    an item, otherwise false *)
val is_item : store_objects -> string -> bool

(** [update_cart st_obj item brand grid] is a store_objects with the
    tuple ([item], [brand]) added to the cart field in [st_obj] *)
val update_cart :
  store_objects -> string -> string -> grid -> store_objects

(** [get_brands_string st_obj item] is a string representing all the
    possible brands of the item [item] *)
val get_brands_string : store_objects -> string -> string

(** [brand_handler st_obj item grid] is a store_objects with item [item]
    added to the cart field only if the user selectes a valid item and
    corresponding brand to that item, otherwise [st_obj] *)
val brand_handler : store_objects -> string -> grid -> store_objects

(** [add_to_cart st_obj input grid] is a store_objects with the item
    [item] added to the cart field in [str_obj] *)
val add_to_cart : store_objects -> string -> grid -> store_objects

(** [is_valid_brand_string st_obj item input] is true if the brand name
    [input] is a valid brand name for the given item [item], otherwise
    false *)
val is_valid_brand_string : store_objects -> string -> string -> bool

(** [get_price item_brand st_obj] is a float representing the price of
    the given item with its specified brand [item_brand] *)
val get_price : string * string -> store_objects -> float

(** [get_total cart st_obj] is a float representing the total price of a
    cart [cart] with all its (item, brand) pairs inputted *)
val get_total : (string * string) list -> store_objects -> float

(** [find_path path acc grid st_obj] is a node list representing a
    possible path to collect all the specified items in a given store
    [grid] *)
val find_path :
  Node.node ref list ->
  Node.node list ->
  Grid.grid ->
  store_objects ->
  Node.node list

(** [min_path st_obj grid] is a node list representing the shortest-path
    to collect all specified items in the cart field of store_objects
    [st_obj] in the store [grid] *)
val min_path : store_objects -> Grid.grid -> Node.node list

(** [is_item_in_store item store_nodes] is true if the given (item,
    brand) tuple [item] is in the store_node list [store_nodes],
    otherwise false *)
val is_item_in_store : string * string -> store_node list -> bool

(** [set_cart st_obj cart grid] is a store_objects with the given cart
    [cart] added to the corresponding field in store_objects [st_obj] *)
val set_cart :
  store_objects -> (string * string) list -> Grid.grid -> store_objects

val get_store_nodes : store_objects -> store_node list

val get_item_brand : string -> (string * string) list -> string * string

val is_valid_remove_query : string -> (string * string) list -> bool
