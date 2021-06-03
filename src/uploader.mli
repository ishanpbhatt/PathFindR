open Yojson.Basic.Util

(** type [uploaded_cart] is a data structure that holds one field
    component, a (string * string) list [cart] where each element in the
    list is a (item, brand) pair *)
type uploaded_cart

(** [create_cart json] is an (item, brand) tuple representing a single
    element in the user's cart *)
val create_cart : Yojson.Basic.t -> string * string

(** [from_json_upload json] is an uploaded_cart with the field data
    [[cart] set as a list of (item, brand) tuples taken from the JSON
    file [json] *)
val from_json_upload : Yojson.Basic.t -> uploaded_cart

(** [print_uploaded_cart cart] is a unit that prints the cart [cart] to
    terminal for the user *)
val print_uploaded_cart : (string * string) list -> unit

(** [get_uploaded_cart up_cart] is a (string * string) list that
    represents the field cart from the uploaded_cart [up_cart] *)
val get_uploaded_cart : uploaded_cart -> (string * string) list
