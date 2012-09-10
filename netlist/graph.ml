open Graph

module G =
  Imperative.Digraph.Abstract(struct
    type t = string
  end)

module DepTopological = Topological.Make(G)
module DepTraverse = Traverse.Mark(G)

module IdentEnv = Map.Make (struct type t = string let compare = compare end)

(* on ne peut pas faire de graphe polymorphe avec ocamlgraph
   a cause des limitations du systeme de module de OCaml *)
type graph =
    { g_graph : G.t;
      mutable g_env : G.V.t IdentEnv.t }

let mk_graph () =
  { g_graph = G.create ();
    g_env = IdentEnv.empty }

let add_node g id =
  let n = G.V.create id in
  G.add_vertex g.g_graph n;
  g.g_env <- IdentEnv.add id n g.g_env

let node_for_ident g x =
  IdentEnv.find x g.g_env

let add_edge g id1 id2 =
  let n1 = node_for_ident g id1 in
  let n2 = node_for_ident g id2 in
  G.add_edge g.g_graph n1 n2

let topological g =
  let l = DepTopological.fold (fun v acc -> (G.V.label v) :: acc) g.g_graph [] in
  List.rev l

let has_cycle g =
  DepTraverse.has_cycle g.g_graph
