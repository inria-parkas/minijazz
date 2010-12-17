open Graph
open Ast

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
    | Eapp(OReg, _) -> acc
    | Eapp(_, args) -> List.fold_left read acc args

let read (_, e) = read [] e

let mk_dependency_graph n =
  (** Creates the initial graph (one node for each equation)
      and an environment mapping idents to nodes. *)
  let init_graph () =
    let add_eq (env, node_list) eq =
      let ids = def eq in
      let n = Graph.mk_node eq in
      let env = List.fold_left (fun env id -> IdentEnv.add id n env) env ids in
        env, n::node_list
    in
    let eqs = match n.n_body with | BEqs (eqs, _) -> eqs | _ -> assert false in
    let env, node_list = List.fold_left add_eq (IdentEnv.empty, []) eqs in
    let top = List.map (fun vd -> IdentEnv.find vd.v_ident env) n.n_outputs in
    let g = mk_graph (Misc.unique top) in
      g, env
  in
  (** Add the dependences corresponding to the equation in the graph. *)
  let add_depends env eq =
    let node_for_eq env eq =
      let id, _ = Misc.assert_1min (def eq) in
        IdentEnv.find id env
    in
    let attach n1 n =
      let n2 = IdentEnv.find n env in
        add_depends n1 n2
    in
    let node = node_for_eq env eq in
    let ids = read eq in
      List.iter (attach node) ids
  in
  let g, env = init_graph () in
  let eqs = match n.n_body with | BEqs (eqs, _) -> eqs | _ -> assert false in
    List.iter (add_depends env) eqs;
    g
