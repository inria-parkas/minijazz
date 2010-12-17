open Ast

let node n =
  let g = Dep.mk_dependency_graph n in
    try
      ignore (Graph.cycle g.Graph.g_top)
    with
        Graph.Cycle _ ->
          Format.eprintf "The node '%s' is not causal@." n.n_name;
          raise Errors.Error

let program p =
  List.iter node p.p_nodes;
  p
