open Node

(**[grid] is the structure to hold an array of nodes*)
type grid

(**[PathFailure] is an exception when no path is found using [astar] *)
exception PathFailure of string

(**[init_grid row col] is a 2d array of nodes with [col] nodes in a row
   and [row] rows in the array *)
val init_grid : int -> int -> grid

(**[get_node_from_grid row col grd] is node at row [row] and column
   [col] in the grid [grd] *)
val get_node_from_grid : int -> int -> grid -> Node.node ref

(**[add_node_to_grid node grd] mutably modifies the grid [grd] by adding
   [node] to the grid at its corresponding (row,col) coordinates *)
val add_node_to_grid : Node.node ref -> grid -> unit

(**[print_node_list index set] prints the list of nodes in [set] *)
val print_node_list : int -> Node.node ref list -> unit

(**[get_neighbors n grid] is a list of neighboring nodes around [n] in
   [grid] *)
val get_neighbors : Node.node ref -> grid -> Node.node ref list

(** [contains elem lst] is a boolean indicating whether [elem] is in
    [lst] *)
val contains : node ref -> node ref list -> bool

(**[remove elem lst] is [lst] without the element [elem] *)
val remove : node -> node list -> node list

(**[reconstruct_path s e] is an ordered list of nodes from end [e] to
   the start [s] *)
val reconstruct_path : node -> node option -> node list

(**[astar s e] is an ordered list of nodes that represents the shortest
   path from start node [s] to end node [e] *)
val astar : node ref -> node ref -> grid -> node list * node ref

(** [get_path_string location i j path] is a string that represents a
    corner of a path in a store *)
val get_path_string : string -> int -> int -> node list -> string

(**[point_in_path path row col] is a boolean representing whether
   [row],[col] is a point in [path] *)
val point_in_path : Node.node list -> int -> int -> bool

(**[print_grid grid cart] prints the grid representing a store with the
   items in cart highlighted *)
val print_grid : grid -> node ref list -> node list -> unit
