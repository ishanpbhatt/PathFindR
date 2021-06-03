type node = {
  row : int;
  col : int;
  typ : string;
  name : string;
  brands_prices : (string * float) list;
  g_score : int;
  f_score : int;
  came_from : node option;
}

let get_row (n : node) : int = n.row

let get_col (n : node) : int = n.col

let get_typ (n : node) : string = n.typ

let get_name (n : node) : string = n.name

let get_g_score (n : node) : int = n.g_score

let get_f_score (n : node) : int = n.f_score

let get_came_from (n : node) : node option = n.came_from

let get_brands_prices (n : node) : (string * float) list =
  n.brands_prices

let create_node
    (r : int)
    (c : int)
    (t : string)
    (n : string)
    (b_p : (string * float) list)
    (g : int)
    (f : int)
    (cf : node option) : node =
  {
    row = r;
    col = c;
    typ = t;
    name = n;
    brands_prices = b_p;
    g_score = g;
    f_score = f;
    came_from = cf;
  }

let get_min_node
    (open_set : node ref list)
    (grid : node ref array array) : node ref =
  let rec get_min_node_help
      (os : node ref list)
      (cur_min : node ref)
      cur_min_score : node ref =
    match os with
    | [] -> cur_min
    | v :: t when !v.f_score < cur_min_score ->
        get_min_node_help t v !v.f_score
    | v :: t ->
        let n = grid.(!v.row).(!v.col) in
        if !n.f_score < cur_min_score then
          get_min_node_help t n !n.f_score
        else get_min_node_help t cur_min cur_min_score
  in
  get_min_node_help open_set (List.nth open_set 0) max_int

let h_score (s : node) (e : node) : int =
  abs (s.row - e.row) + abs (s.col - e.col)
