open Ast
open Static

let rec simplify_exp e = match e.e_desc with
  | Ecall("select", idx::params, [{ e_desc = Ecall ("slice", min::_, args) }]) ->
      let new_idx = SBinOp (SMinus, SBinOp (SAdd, idx, min), SInt 1) in
      let new_idx = Static.simplify NameEnv.empty new_idx in
      let new_e = { e with e_desc = Ecall("select", new_idx::params, args) } in
      simplify_exp new_e
  | Ecall("slice", [min; max; n], args) when min = max ->
      let new_e = { e with e_desc = Ecall("select", [min; n], args) } in
      simplify_exp new_e
  | Ecall("slice", [min1; max1; n1], [{ e_desc = Ecall ("slice", [min2; max2; n2], args) }]) ->
      let new_min = SBinOp (SMinus, SBinOp (SAdd, min2, min1), SInt 1) in
      let new_min = Static.simplify NameEnv.empty new_min in
      let new_max = SBinOp (SMinus, SBinOp (SAdd, min2, max1), SInt 1) in
      let new_max = Static.simplify NameEnv.empty new_max in
      let new_e = { e with e_desc = Ecall("slice", [new_min; new_max; n1], args) } in
      simplify_exp new_e
  | Ecall(f, params, args) ->
      { e with e_desc = Ecall(f, params, List.map simplify_exp args) }
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
