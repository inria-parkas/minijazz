open Ast
open Static

let expect_int se =
  match simplify NameEnv.empty se with
    | SInt v -> v
    | se ->
        Format.eprintf "Unexpected static exp: %a@." Printer.print_static_exp se;
        assert false

let expect_ident e = match e.e_desc with
  | Evar id -> id
  | _ -> assert false

let tr_value v = match v with
  | VBit b -> Netlist_ast.VBit b
  | VBitArray a -> Netlist_ast.VBitArray (Array.to_list a)

let tr_ty ty = match ty with
  | TBit -> Netlist_ast.TBit
  | TBitArray se -> Netlist_ast.TBitArray (expect_int se)
  | _ -> Format.eprintf "Unexpected type: %a@." Printer.print_type ty; assert false

let tr_var_dec { v_ident = n; v_ty = ty } =
  n, tr_ty ty

let tr_pat pat = match pat with
  | Evarpat id -> id
  | Etuplepat ids ->
      Format.eprintf "Unexpected pattern: %a@." Printer.print_pat pat;
      assert false

let expect_arg e = match e.e_desc with
  | Evar id -> Netlist_ast.Avar id
  | Econst v -> Netlist_ast.Aconst (tr_value v)
  | _ -> Format.eprintf "Unexpected arg : %a@." Printer.print_exp e; assert false

let rec tr_exp e = match e.e_desc with
  | Evar id -> Netlist_ast.Earg (Netlist_ast.Avar id)
  | Econst v ->  Netlist_ast.Earg (Netlist_ast.Aconst (tr_value v))
  | Eapp(OReg, args) ->
      let e = Misc.assert_1 args in
      Netlist_ast.Ereg (expect_ident e)
 | Eapp(OCall ("not", _), args) ->
      let e = Misc.assert_1 args in
      Netlist_ast.Enot (expect_arg e)
 | Eapp(OCall (("or" | "xor" | "and" | "nand") as op, _), args) ->
      let e1, e2 = Misc.assert_2 args in
      let op =
        match op with
          | "or" -> Netlist_ast.Or
          | "xor" -> Netlist_ast.Xor
          | "and" -> Netlist_ast.And
          | "nand" -> Netlist_ast.Nand
          | _ -> assert false
      in
      Netlist_ast.Ebinop (op, expect_arg e1, expect_arg e2)
 | Eapp(OCall ("mux", _), args) ->
      let e1, e2, e3 = Misc.assert_3 args in
      Netlist_ast.Emux (expect_arg e1, expect_arg e2, expect_arg e3)
  | Eapp(OSelect idx, args) ->
      let e = Misc.assert_1 args in
      Netlist_ast.Eselect (expect_int idx, expect_arg e)
  | Eapp(OSlice (min, max), args) ->
      let e = Misc.assert_1 args in
      Netlist_ast.Eslice (expect_int min, expect_int max, expect_arg e)
  | Eapp(OConcat, args) ->
      let e1, e2 = Misc.assert_2 args in
      Netlist_ast.Econcat (expect_arg e1, expect_arg e2)
  | Eapp(OMem(MRom, addr_size, word_size, _), args) ->
      let e = Misc.assert_1 args in
      Netlist_ast.Erom (expect_int addr_size, expect_int word_size, expect_arg e)
  | Eapp(OMem(MRam, addr_size, word_size, _), args) ->
      let ra, we, wa, data = Misc.assert_4 args in
      Netlist_ast.Eram (expect_int addr_size, expect_int word_size,
                       expect_arg ra, expect_arg we, expect_arg wa, expect_arg data)
  | _ -> assert false

let tr_eq (pat, e) =
  tr_pat pat, tr_exp e

let tr_block b = match b with
  | BEqs (eqs, vds) ->
      let env =
        List.fold_left
          (fun env vd -> Netlist_ast.Env.add vd.v_ident (tr_ty vd.v_ty) env)
          Netlist_ast.Env.empty vds
      in
      let eqs = List.map tr_eq eqs in
      env, eqs
  | _ -> assert false

let program p =
  let n = match p.p_nodes with [n] -> n | _ -> assert false in
  let vars, eqs = tr_block n.n_body in
  { Netlist_ast.p_vars = vars; Netlist_ast.p_eqs = eqs }
