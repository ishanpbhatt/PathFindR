open Yojson.Basic.Util
open Grid
open Node

(** [cart] is a string list type representing a list of items chosen by
    a user *)
type cart

(** [cart_node] is a stucture to hold the name of an item and the brands
    available for that given item *)
type cart_node

(** [cart_built] is a strcutre that has field [cart_nodes] representing
    a list of tuples holding an item and a list of the possible brands
    for that item, and another field [cart] that represents a final list
    of (item * brand) tuples a user selects *)
type cart_built

(** [get_node json] is a (item * brand list) from a json file that
    represents a store *)
val get_node : Yojson.Basic.t -> string * string list

(** [from_json_built json] is a cart_built created fron the json file
    [json] that represents a given store *)
val from_json_built : Yojson.Basic.t -> cart_built

(** [cart_printer c_built] is a unit that serves as a printer function
    to output the user's cart to terminal *)
val cart_printer : cart_built -> unit

(** [display_options c_built] is a unit that serves as a printer
    function to interact with the user about the options of items that
    can be added to their cart *)
val display_options : cart_built -> unit

(** [brand_handler c_built input] is a cart_built that requires a brand
    input for a given item being added to the cart *)
val brand_handler : cart_built -> string -> cart_built

(** [remove_check (c_built : cart_built) item] is a cart_built that
    ensures the item [item] was succesffuly removed from the user's cart *)
val remove_check : cart_built -> string -> cart_built

(** [input_parser c_built input] is a cart_built that ensures a user's
    input [input] is a valid add or remove action to be done on a given
    cart *)
val input_parser : cart_built -> string -> cart_built

(** [get_cart_built c_built] is a [cart] field from the type cart_build *)
val get_cart_built : cart_built -> (string * string) list
