open Ast

let mk_eq e =
  let id = "_l" ^ (Misc.gen_symbol ()) in
  let eq = (Evarpat id, e) in
    Evar id, eq

let rec mk_simple acc e = match e.e_desc with
  (*| Eapp ((OSelect _ | OSlice _), _ ) -> e, acc*)
  | Eapp (op, args) ->
      let args, acc = Misc.mapfold mk_simple acc args in
      let desc, eq = mk_eq { e with e_desc = Eapp (op, args) } in
        { e with e_desc = desc }, eq::acc
  | _ -> e, acc

let norm_exp acc e = match e.e_desc with
  | Econst _ | Evar _ -> e, acc
  | Eapp (op, args) ->
      let args, acc = Misc.mapfold mk_simple acc args in
        { e with e_desc = Eapp(op, args) }, acc

let norm_eq acc (pat,e) =
  let e, acc = norm_exp acc e in
    (pat, e)::acc

let rec block b = match b with
  | BEqs(eqs, vds) ->
      let eqs = List.fold_left norm_eq [] eqs in
        BEqs(eqs, vds)
  | BIf(se, trueb, elseb) ->
      BIf(se, block trueb, block elseb)

let node n =
  { n with n_body = block n.n_body }

let program p =
    { p with p_nodes = List.map node p.p_nodes }
