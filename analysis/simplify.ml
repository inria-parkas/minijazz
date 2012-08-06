open Ast
open Static

let rec simplify_exp e = match e.e_desc with
  (* replace x[i..j] with [] if j < i *)
  | Ecall("slice", [SInt min; SInt max; n], _) when max < min ->
      { e with e_desc = Econst (VBitArray (Array.make 0 false)) }
  (* replace x[i..i] with x[i] *)
  | Ecall("slice", [min; max; n], args) when min = max ->
      let new_e = { e with e_desc = Ecall("select", [min; n], args) } in
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
