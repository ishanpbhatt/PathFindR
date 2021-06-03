type node = {
  row : int;
  col : int;
  typ : string;
  (*typ will be like "wall" "item" "open"*)
  symbol : string;
  g_score : int;
  f_score : int;
  came_from : node option;
}

exception PathFailure of string

(**[init_grid row col] is a 2d array of nodes with [col] nodes in a row
   and [row] rows in the array *)
let init_grid row col =
  let rec init_row r c acc : node ref list =
    match c with
    | -1 -> acc
    | _ ->
        init_row r (c - 1) acc
        @ [
            ref
              {
                row = r;
                col = c;
                typ = "none";
                symbol = "#";
                g_score = 1000000;
                f_score = 1000000;
                came_from = None;
              };
          ]
  in
  let rec init_cols r c acc : node ref list list =
    match r with
    | -1 -> acc
    | _ -> init_cols (r - 1) c acc @ [ init_row r c [] ]
  in
  init_cols (row - 1) (col - 1) []

(** [h_score s e] is the manhatten distance from start node [s] to end
    node [e]*)
let h_score (s : node) (e : node) : int =
  abs (s.row - e.row) + abs (s.col - e.col)

(** [contains elem lst] is a boolean indicating whether [elem] is in
    [lst]*)
let rec contains elem = function
  | [] -> false
  | h :: _ when h = elem -> true
  | _ :: t -> contains elem t

(**[remove elem lst] is [lst] without the element [elem]*)
let rec remove elem = function
  | [] -> []
  | h :: t when h = elem -> t
  | h :: t -> remove elem t @ [ h ]

(**[get_neighbors n grid] is a list of viable nodes in [grid] that the
   astar algo is able to traverse to from node [n]*)
let get_neighbors (n : node ref) (grid : node ref list list) :
    node ref list =
  let neighbors = ref [] in
  if !n.row < List.length grid - 1 then
    neighbors :=
      !neighbors @ [ List.nth (List.nth grid (!n.row + 1)) !n.col ];
  if !n.col < List.length (List.nth grid 0) - 1 then
    neighbors :=
      !neighbors @ [ List.nth (List.nth grid !n.row) (!n.col + 1) ];
  if !n.row > 0 then
    neighbors :=
      !neighbors @ [ List.nth (List.nth grid (!n.row - 1)) !n.col ];
  if !n.col > 0 then
    neighbors :=
      !neighbors @ [ List.nth (List.nth grid !n.row) (!n.col - 1) ];
  !neighbors

(** [get_min_node open_set f_score] is the node in [open_set] that has
    the smallest score in [f_score]*)
let get_min_node (open_set : node ref list) (grid : node ref list list)
    : node ref =
  let rec get_min_node_help
      (os : node ref list)
      (cur_min : node ref)
      cur_min_score : node ref =
    match os with
    | [] -> cur_min
    | v :: t when !v.f_score < cur_min_score ->
        get_min_node_help t v !v.f_score
    | v :: t ->
        let n = List.nth (List.nth grid !v.col) !v.row in
        if !n.f_score < cur_min_score then
          get_min_node_help t n !n.f_score
        else get_min_node_help t cur_min cur_min_score
  in
  get_min_node_help open_set (List.nth open_set 0) 100000000

let reconstruct_path s e =
  let rec reconstruct_help cur_node acc =
    match cur_node with
    | None -> acc
    | Some node -> reconstruct_help node.came_from acc @ [ node ]
  in
  reconstruct_help e []

(**[astar s e] is an ordered list of nodes that represents the shortest
   path from start node [s] to end node [e]*)
let astar (s : node ref) (e : node ref) (g : node ref list list) :
    node list =
  s :=
    {
      row = !s.row;
      col = !s.col;
      typ = !s.typ;
      symbol = !s.symbol;
      g_score = 0;
      f_score = h_score !s !e;
      came_from = None;
    };
  let open_set = ref [ s ] in
  let rec loop cur =
    if List.length !open_set < 1 then
      raise (PathFailure "No path found");
    match cur with
    | cur when cur = e -> reconstruct_path !s (Some !e)
    | _ ->
        open_set := remove cur !open_set;
        let neighbors = get_neighbors cur g in
        for i = 0 to List.length neighbors - 1 do
          let n = List.nth neighbors i in
          let temp_g_score = !cur.g_score + 1 in
          if temp_g_score < !n.g_score then (
            let new_node =
              {
                row = !n.row;
                col = !n.col;
                typ = !n.typ;
                symbol = !n.symbol;
                g_score = temp_g_score;
                f_score = temp_g_score + h_score !n !e;
                came_from = Some !cur;
              }
            in
            n := new_node;
            if not (contains n !open_set) then
              open_set := !open_set @ [ n ])
        done;
        loop (get_min_node !open_set g)
  in
  loop (get_min_node !open_set g)

let grid = init_grid 50 50

let start = List.nth (List.nth grid 0) 0

let en = List.nth (List.nth grid 49) 49

let path = astar start en grid

let print_path (path : node list) (grid : node ref list list) rows cols
    : unit =
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let node = List.nth (List.nth grid j) i in
      if contains !node path then print_string "#" else print_string "+"
    done;
    print_string "\n"
  done
