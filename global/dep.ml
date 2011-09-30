open Ast
open Graph

module Graph = struct
  module G =
    Imperative.Digraph.Abstract(struct
      type t = equation
    end)

  module DepTopological = Topological.Make(G)
  module DepTraverse = Traverse.Mark(G)

  type graph =
      { g_graph : G.t;
        mutable g_env : G.V.t IdentEnv.t }

  let mk_graph () =
   { g_graph = G.create ();
     g_env = IdentEnv.empty }

  let add_node g v ids =
    let n = G.V.create v in
    G.add_vertex g.g_graph n;
    g.g_env <- List.fold_left (fun env id -> IdentEnv.add id n env) g.g_env ids

  let add_edge g v1 v2 =
    G.add_edge g.g_graph v1 v2

  let node_for_ident g x =
    IdentEnv.find x g.g_env

  let topological g =
    let l = DepTopological.fold (fun v acc -> (G.V.label v) :: acc) g.g_graph [] in
    l

  let has_cycle g =
    DepTraverse.has_cycle g.g_graph
end


(** @return the variables defined by a pattern *)
let def (pat, _) = match pat with
  | Evarpat id -> [id]
  | Etuplepat l -> l

let add id acc =
  if List.mem id acc then acc else id::acc
(** @return the variables read by an expression
    (variables to the right of a register are ignored)*)
let rec read acc e =
  match e.e_desc with
    | Econst _ -> acc
    | Evar id -> add id acc
    (* no dependency for registers *)
    | Eapp(OReg, _) -> acc
    (* for a RAM, only depend on the read_addr input *)
    | Eapp(OMem (MRam, _, _, _), args) ->
      let read_addr, _ = Misc.assert_1min args in
      read acc read_addr
    | Eapp(_, args) -> List.fold_left read acc args

let read (_, e) = read [] e

let mk_dependency_graph n =
  (** Creates the initial graph (one node for each equation)
      and an environment mapping idents to nodes. *)
  let init_graph () =
    let eqs = match n.n_body with | BEqs (eqs, _) -> eqs | _ -> assert false in
    let g = Graph.mk_graph () in
    List.iter (fun eq -> Graph.add_node g eq (def eq)) eqs;
    g
  in
  (** Add the dependences corresponding to the equation in the graph. *)
  let add_depends g eq =
    let node_for_eq g eq =
      let id, _ = Misc.assert_1min (def eq) in
      Graph.node_for_ident g id
    in
    let attach n1 n =
      try
        let n2 = Graph.node_for_ident g n in
        Graph.add_edge g n1 n2
      with
        | Not_found -> () (*n is an input, no dependency*)
    in
    let node = node_for_eq g eq in
    let ids = read eq in
    List.iter (attach node) ids
  in
  let g = init_graph () in
  let eqs = match n.n_body with | BEqs (eqs, _) -> eqs | _ -> assert false in
    List.iter (add_depends g) eqs;
    g
