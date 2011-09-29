open Ast

let node n =
  let g = Dep.mk_dependency_graph n in
  if Dep.Graph.has_cycle g then (
    Format.eprintf "The node '%s' is not causal@." n.n_name;
    raise Errors.Error);
  let eqs = Dep.Graph.topological g in
  match n.n_body with
    | BEqs (_, vds) ->
      { n with n_body = BEqs(eqs, vds) }
    | _ -> assert false

let program p =
  { p with p_nodes = List.map node p.p_nodes }
