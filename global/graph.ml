
type 'a graph =
    { g_top: 'a node list }

and 'a node =
    { g_content: 'a;
      g_tag: int;
      mutable g_visited: bool;
      mutable g_mark: int;
      mutable g_depends_on: 'a node list;
      (*mutable g_depends_by: 'a node list;*)
    }

exception Cycle of int (* returns the index of the node *)

let tag = ref 0
let new_tag () = incr tag; !tag
let content g = g.g_content
let linked g1 g2 =
  (List.memq g2 g1.g_depends_on) or (List.memq g1 g2.g_depends_on)

let mk_node c =
  { g_content = c; g_tag = new_tag (); g_visited = false;
    g_mark = -1; g_depends_on = [] }

let mk_graph top_list = { g_top = top_list }

let add_depends node1 node2 =
  if not (node1.g_tag = node2.g_tag) then
    node1.g_depends_on <- node2 :: node1.g_depends_on

let topological g_list =
  let rec sortrec g_list seq =
    match g_list with
      | [] -> seq
      | g :: g_list ->
          if g.g_visited then sortrec g_list seq
          else
            begin
              g.g_visited <- true;
              let seq = sortrec g.g_depends_on seq in
              sortrec g_list (g :: seq)
            end in
  let seq = sortrec g_list [] in
  List.iter
    (fun ({ g_visited = _ } as node) -> node.g_visited <- false) g_list;
  List.rev seq

(** Detection of cycles *)
(* Mark nodes with:
   - -1 initially, for unvisited nodes
   - 0 for "opened" nodes, currently visited, while visiting its descendents
   - 1 for "closed" nodes, visited once, no circuits found from it.
   A circuit is found when a node marked with 0 is visited again.
*)

let cycle g_list =
  (* store nodes in a stack *)
  let s = Stack.create () in
  (* flush the connected component *)
  let rec flush () =
    if Stack.is_empty s then []
    else let v = Stack.pop s in
    v.g_content :: flush () in

  let rec visit g =
    match g.g_mark with
      | -1 ->
          (* Unvisited yet *)
          (* Open node *)
          Stack.push g s;
          g.g_mark <- 0;
          (* Visit descendents *)
          List.iter visit g.g_depends_on;
          (* Close node *)
          ignore (Stack.pop s);
          g.g_mark <- 1
      | 0 ->
          (* Visit an opened node (visited and not close) : circuit *)
          raise (Cycle g.g_tag)
      | 1 | _ ->
          (* Visit a closed node (no existing circuit) : pass *)
          () in
  try
    List.iter visit g_list; None
  with
    | Cycle _ -> Some(flush ())
