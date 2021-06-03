open Node

type grid = node ref array array

exception PathFailure of string

let init_grid r c =
  let grid =
    Array.make_matrix r c
      (ref (create_node 0 0 "OPEN" "N/A" [] max_int max_int None))
  in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      grid.(i).(j) <-
        ref (create_node i j "OPEN" "N/A" [] max_int max_int None)
    done
  done;
  grid

let get_node_from_grid row col (grd : grid) =
  let r = grd.(row) in
  r.(col)

let add_node_to_grid (node : Node.node ref) (grd : grid) =
  grd.(get_row !node).(get_col !node) <- node

let rec print_node_list index (set : node ref list) : unit =
  match set with
  | [] -> print_endline "End of neighbors"
  | h :: t ->
      if index = 0 then print_endline "Start of neighbors";
      print_string "Index: ";
      print_int index;
      print_endline "";
      print_string "row: ";
      print_int (get_row !h);
      print_endline "";
      print_string "col: ";
      print_int (get_col !h);
      print_endline "";
      print_node_list (index + 1) t

let get_neighbors n (grd : grid) : node ref list =
  let neighbors = ref [] in
  if get_row !n < Array.length grd - 1 then
    neighbors := !neighbors @ [ grd.(get_row !n + 1).(get_col !n) ];
  if get_col !n < Array.length grd.(0) - 1 then
    neighbors := !neighbors @ [ grd.(get_row !n).(get_col !n + 1) ];
  if get_row !n > 0 then
    neighbors := !neighbors @ [ grd.(get_row !n - 1).(get_col !n) ];
  if get_col !n > 0 then
    neighbors := !neighbors @ [ grd.(get_row !n).(get_col !n - 1) ];
  !neighbors

let rec contains elem = function
  | [] -> false
  | h :: _ when h = elem -> true
  | _ :: t -> contains elem t

(**[remove elem lst] is [lst] without the element [elem]*)
let rec remove elem = function
  | [] -> []
  | h :: t when h = elem -> t
  | h :: t -> remove elem t @ [ h ]

let reconstruct_path s e =
  let rec reconstruct_help cur_node acc =
    match cur_node with
    | None -> acc
    | Some node -> reconstruct_help (get_came_from node) acc @ [ node ]
  in
  reconstruct_help e []

let print_nodes nodes =
  print_string "\nLength:";
  print_int (List.length nodes);
  let rec help lst =
    match lst with
    | [] -> print_string "\nend\n"
    | h :: t ->
        print_string "\nRow:";
        print_int (get_row h);
        print_string "\nCol:";
        print_int (get_col h);
        help t
  in
  help nodes

let rec check_neighbors neighbors s e cur open_set =
  match neighbors with
  | n :: t when get_row !n = get_row !e && get_col !n = get_col !e ->
      let path = reconstruct_path !s (Some !cur) in
      (path, cur)
  | n :: t ->
      let temp_g_score = get_g_score !cur + 1 in
      if temp_g_score < get_g_score !n then (
        let new_node =
          create_node (get_row !n) (get_col !n) (get_typ !n)
            (get_name !n) (get_brands_prices !s) temp_g_score
            (temp_g_score + h_score !n !e)
            (Some !cur)
        in
        n := new_node;
        if not (contains n !open_set) then open_set := !open_set @ [ n ]);
      check_neighbors t s e cur open_set
  | [] -> ([], cur)

(**[astar s e] is an ordered list of nodes that represents the shortest
   path from start node [s] to end node [e]*)
let astar (s : Node.node ref) (e : Node.node ref) g =
  s :=
    create_node (get_row !s) (get_col !s) (get_typ !s) (get_name !s)
      (get_brands_prices !s) 0 (h_score !s !e) None;
  let open_set = ref [ s ] in
  let rec loop cur =
    if List.length !open_set < 1 then
      raise (PathFailure "No path found");
    match cur with
    | cur when get_typ !cur = "SHELF" || get_typ !cur = "WALL" ->
        open_set := remove cur !open_set;
        loop (get_min_node !open_set g)
    | _ ->
        open_set := remove cur !open_set;
        let neighbors = get_neighbors cur g in
        let n_check = check_neighbors neighbors s e cur open_set in
        if List.length (fst n_check) = 0 then
          loop (get_min_node !open_set g)
        else n_check
  in
  loop (get_min_node !open_set g)

let variation_helper1 i j adj_nodes =
  if contains (i, j - 1) adj_nodes then
    if contains (i + 1, j) adj_nodes then
      if contains (i - 1, j) adj_nodes then 9 else 11
    else if contains (i - 1, j) adj_nodes then 10
      (*Contains right,left,top*)
    else 4 (*Contains right and left*)
  else if contains (i + 1, j) adj_nodes then
    if contains (i - 1, j) adj_nodes then 7 (*Contains right,bot,top*)
    else 6 (*Contains right,bot*)
  else if contains (i - 1, j) adj_nodes then 1 (*Contains right,top*)
  else 0

(*Contains only right*)

let variation_helper2 i j adj_nodes =
  if contains (i, j - 1) adj_nodes then
    if contains (i + 1, j) adj_nodes then
      if contains (i - 1, j) adj_nodes then 8 (*Contains left,bot,top*)
      else 5 (*Contains left,bot*)
    else if contains (i - 1, j) adj_nodes then 3
    else 0
  else if contains (i + 1, j) adj_nodes then
    if contains (i - 1, j) adj_nodes then 2 (*Contains top,bot*) else 0
  else 0

let get_variation adj_nodes i j =
  if contains (i, j + 1) adj_nodes then variation_helper1 i j adj_nodes
  else variation_helper2 i j adj_nodes

let rec get_adj_nodes
    (path : Node.node list)
    (acc : (int * int) list)
    i
    j =
  match path with
  | [] -> acc
  | h :: t when get_row h = i && get_col h = j + 1 ->
      get_adj_nodes t acc i j @ [ (i, j + 1) ]
  | h :: t when get_row h = i && get_col h = j - 1 ->
      get_adj_nodes t acc i j @ [ (i, j - 1) ]
  | h :: t when get_col h = j && get_row h = i + 1 ->
      get_adj_nodes t acc i j @ [ (i + 1, j) ]
  | h :: t when get_col h = j && get_row h = i - 1 ->
      get_adj_nodes t acc i j @ [ (i - 1, j) ]
  | _ :: t -> get_adj_nodes t acc i j

let get_top_string var =
  match var with
  | 1 -> "  #  "
  | 2 -> "  #  "
  | 3 -> "  #  "
  | 4 -> "     "
  | 5 -> "     "
  | 6 -> "     "
  | 7 -> "  #  "
  | 8 -> "  #  "
  | 9 -> "  #  "
  | 10 -> "  #  "
  | 11 -> "     "
  | _ -> "     "

let get_mid_string var =
  match var with
  | 1 -> "  ###"
  | 2 -> "  #  "
  | 3 -> "###  "
  | 4 -> "#####"
  | 5 -> "###  "
  | 6 -> "  ###"
  | 7 -> "  ###"
  | 8 -> "###  "
  | 9 -> "#####"
  | 10 -> "#####"
  | 11 -> "#####"
  | _ -> "  #  "

let get_bot_string var =
  match var with
  | 1 -> "     "
  | 2 -> "  #  "
  | 3 -> "     "
  | 4 -> "     "
  | 5 -> "  #  "
  | 6 -> "  #  "
  | 7 -> "  #  "
  | 8 -> "  #  "
  | 9 -> "  #  "
  | 10 -> "     "
  | 11 -> "  #  "
  | _ -> "     "

let get_path_string location i j path =
  let adj_nodes = get_adj_nodes path [] i j in
  let var = get_variation adj_nodes i j in
  match location with
  | s when s = "TOP" -> get_top_string var
  | s when s = "CENTER" -> get_mid_string var
  | s when s = "BOT" -> get_bot_string var
  | _ -> failwith ""

let rec point_in_path path row col =
  match path with
  | h :: t when get_row h = row && get_col h = col -> true
  | h :: t -> point_in_path t row col
  | [] -> false

let print_node_top node cart path i j =
  match get_typ !node with
  | "SHELF" when List.mem node cart ->
      ANSITerminal.print_string [ ANSITerminal.green ] "#####"
  | "SHELF" -> ANSITerminal.print_string [ ANSITerminal.black ] "#####"
  | "START/EXIT" ->
      ANSITerminal.print_string [ ANSITerminal.red ] "#####"
  | "OPEN" when point_in_path path i j && List.length path != 1 ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        (get_path_string "TOP" i j path)
  | "WALL" -> ANSITerminal.print_string [ ANSITerminal.black ] "#####"
  | _ -> print_string "     "

let print_node_mid node cart path i j =
  match get_typ !node with
  | "OPEN" when point_in_path path i j && List.length path != 1 ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        (get_path_string "CENTER" i j path)
  | "SHELF" when List.mem node cart ->
      ANSITerminal.print_string [ ANSITerminal.green ] "ITEM "
  | "SHELF" -> ANSITerminal.print_string [ ANSITerminal.red ] "ITEM "
  | "WALL" -> ANSITerminal.print_string [ ANSITerminal.black ] "#####"
  | "START/EXIT" ->
      ANSITerminal.print_string [ ANSITerminal.red ] "EXIT "
  | _ -> print_string "     "

let print_node_bot node cart path i j =
  match get_typ !node with
  | "SHELF" when List.mem node cart ->
      ANSITerminal.print_string [ ANSITerminal.green ] "#####"
  | "SHELF" -> ANSITerminal.print_string [ ANSITerminal.black ] "#####"
  | "START/EXIT" ->
      ANSITerminal.print_string [ ANSITerminal.red ] "#####"
  | "OPEN" when point_in_path path i j && List.length path != 1 ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        (get_path_string "BOT" i j path)
  | "WALL" -> ANSITerminal.print_string [ ANSITerminal.black ] "#####"
  | _ -> print_string "     "

let print_row_top rows cols cart path grid i : unit =
  ANSITerminal.print_string [ ANSITerminal.black ] "#";
  for j = 0 to cols - 1 do
    let node = get_node_from_grid i j grid in
    print_node_top node cart path i j
  done;
  ANSITerminal.print_string [ ANSITerminal.black ] "#";
  print_string "\n";
  ANSITerminal.print_string [ ANSITerminal.black ] "#"

let print_row_mid rows cols cart path grid i : unit =
  for j = 0 to Array.length grid.(0) - 1 do
    let node = get_node_from_grid i j grid in
    print_node_mid node cart path i j
  done;
  ANSITerminal.print_string [ ANSITerminal.black ] "#";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.black ] "#"

let print_row_bot rows cols cart path grid i : unit =
  for j = 0 to Array.length grid.(0) - 1 do
    let node = get_node_from_grid i j grid in
    print_node_bot node cart path i j
  done;
  ANSITerminal.print_string [ ANSITerminal.black ] "#";
  print_string "\n"

let print_grid grid (cart : node ref list) path : unit =
  print_string "Items in your cart are in ";
  ANSITerminal.print_string [ ANSITerminal.green ] "green.\n\n";
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  ANSITerminal.print_string [ ANSITerminal.black ]
    (String.make (cols * 5) '#');
  print_string "\n";
  for i = 0 to rows - 1 do
    print_row_top rows cols cart path grid i;
    print_row_mid rows cols cart path grid i;
    print_row_bot rows cols cart path grid i
  done;
  ANSITerminal.print_string [ ANSITerminal.black ]
    (String.make (cols * 5) '#');
  print_string "\n";
  ()
