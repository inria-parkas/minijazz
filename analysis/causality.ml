open Ast
open Graph

let node n =
  let g = Dep.mk_dependency_graph n in
    (try
      ignore (cycle g.g_top)
    with
        Cycle _ ->
          Format.eprintf "The node '%s' is not causal@." n.n_name;
          raise Errors.Error);
    let nodes = topological g.g_top in
    let eqs = List.map (fun g -> g.g_content) nodes in
      match n.n_body with
        | BEqs (_, vds) ->
            { n with n_body = BEqs(eqs, vds) }
        | _ -> assert false

let program p =
  { p with p_nodes = List.map node p.p_nodes }
