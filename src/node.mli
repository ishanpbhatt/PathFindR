(** [node] is a node that represents a point in a grid with a given
    (row, column) coordinate and contains information regarnding what in
    the store is located at that point and how to construct a path to
    that point *)
type node

(** [get_row n] is the record field row of node [n] *)
val get_row : node -> int

(** [get_col n] is the record field col of node [n] *)
val get_col : node -> int

(** [get_typ n] is the record field typ of node [n] *)
val get_typ : node -> string

(**[get_name n] is the record field name of node [n]*)
val get_name : node -> string

(** [get_g_score n] is the record field g_score of node n *)
val get_g_score : node -> int

(** [get_f_score n] is the record field fty_score of node [n] *)
val get_f_score : node -> int

(** [get_came_from n] is the record field came_from of node n *)
val get_came_from : node -> node option

(**[get_brands_prices n] is an association list of the brands and
   corresponding prices of node [n]*)
val get_brands_prices : node -> (string * float) list

(** [create_node] is a type node with record field values as inputed *)
val create_node :
  int ->
  int ->
  string ->
  string ->
  (string * float) list ->
  int ->
  int ->
  node option ->
  node

(** [get_min_node open_set f_score] is the node in [open_set] that has
    the smallest score in [f_score]*)
val get_min_node : node ref list -> node ref array array -> node ref

(** [h_score s e] is the Manhattan distance from start node [s] to end
    node [e]*)
val h_score : node -> node -> int
