open Ast
open Static

let rec simplify_exp e = match e.e_desc with
  | Eapp(op, ([e1] as args)) ->
    (match op, e1.e_desc with
      | OSelect i, Eapp (OSlice (i1, _), args) ->
        let new_i = SBinOp (SMinus, SBinOp (SAdd, i, i1), SInt 1) in
        let new_i = Static.simplify NameEnv.empty new_i in
        let new_e = { e with e_desc = Eapp(OSelect new_i, args) } in
        simplify_exp new_e
      | op, _ -> { e with e_desc = Eapp(op, List.map simplify_exp args) }
    )
  | Eapp(op, args) ->
    { e with e_desc = Eapp(op, List.map simplify_exp args) }
  | _ -> e

let simplify_eq (pat,e) =
  (pat, simplify_exp e)

let rec block b = match b with
  | BEqs(eqs, vds) -> BEqs(List.map simplify_eq  eqs, vds)
  | BIf(se, trueb, elseb) -> BIf(se, block trueb, block elseb)

let node n =
  { n with n_body = block n.n_body }

let program p =
    { p with p_nodes = List.map node p.p_nodes }
