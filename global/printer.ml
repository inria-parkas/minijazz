open Ast
open Format

let print_name ff n = fprintf ff "%s" n
let print_ident ff id = fprintf ff "%s" id

let rec print_list_r print lp sep rp ff = function
  | [] -> ()
  | x :: l ->
      fprintf ff "%s%a" lp print x;
      List.iter (fprintf ff "%s %a" sep print) l;
      fprintf ff "%s" rp

let rec print_list_nlr print lp sep rp ff = function
  | [] -> ()
  | x :: l ->
      fprintf ff "%s%a" lp print x;
      List.iter (fprintf ff "%s@ %a" sep print) l;
      fprintf ff "%s" rp

let print_bool ff b =
  if b then
    fprintf ff "1"
  else
    fprintf ff "0"

let rec print_const ff v = match v with
  | VBit b -> print_bool ff b
  | VBitArray l -> List.iter (print_bool ff) l

let rec print_static_exp ff se = match se with
  | SConst i -> fprintf ff "%d" i
  | SVar n -> print_name ff n
  | SBinOp(op, se1, se2) ->
    let op_str = match op with
      | SAdd -> "+" | SMinus -> "-"
      | SMult -> "*" | SDiv -> "/"
      | SPower -> "^" in
      fprintf ff "(%a %s %a)" print_static_exp se1  op_str  print_static_exp se2

let print_type ff ty = match ty with
  | TUnit -> fprintf ff "()"
  | TBit -> fprintf ff "bit"
  | TBitArray se -> fprintf ff "bit[%a]" print_static_exp se

let print_op ff op = match op with
  | OReg -> fprintf ff "reg"
  | OMem(true, addr_size, word_size) -> (*ROM*)
    fprintf ff "rom<%d,%d>" addr_size word_size
  | OMem(false, addr_size, word_size) -> (*ROM*)
    fprintf ff "ram<%d,%d>" addr_size word_size
  | OCall fn -> print_name ff fn
  | OPrim _ -> assert false

let rec print_exp ff e =
  (*if print_types then
      fprintf ff "%a : %a" print_edesc e.e_desc print_
    else*)
  fprintf ff "%a" print_edesc e.e_desc

and print_edesc ff ed = match ed with
  | Econst v -> print_const ff v
  | Evar n -> print_ident ff n
  | Eapp(OPrim op, args) -> print_prim ff op args
  | Eapp(op, args) -> fprintf ff "%a%a" print_op op  print_args args

and print_args ff args =
  print_list_r print_exp "(" "," ")" ff args

and print_prim ff prim args = match prim with
  | PAnd | POr | PXor | PNand -> (*binary op*)
    let s = match prim with
      | PAnd -> "and"   | POr -> "or"
      | PNand -> "nand" | PXor -> "xor"
      | _ -> assert false in
    let e1, e2 = Misc.assert_2 args in
      fprintf ff "%a %s %a" print_exp e1  s  print_exp e2

  | PNot | PMux -> (*unary op*)
    let s = match prim with
      | PMux -> "mux" | PNot -> "not"
      | _ -> assert false in
    let e1 = Misc.assert_1 args in
      fprintf ff "%s(%a)" s  print_exp e1

let rec print_pat ff pat = match pat with
  | Evarpat id -> print_ident ff id
  | Etuplepat l -> print_list_r print_ident "(" "," ")" ff l

let print_eq ff (pat, e) =
  fprintf ff "%a = %a" print_pat pat  print_exp e

let print_eqs ff eqs =
  print_list_nlr print_eq """;""" ff eqs

let print_var_dec ff vd = match vd.v_ty with
  | TUnit | TBit -> fprintf ff "%a" print_name vd.v_ident
  | TBitArray se ->
    fprintf ff "%a : [%a]" print_name vd.v_ident  print_static_exp se

let print_var_decs ff vds =
  print_list_r print_var_dec "("","")" ff vds

let print_node ff n =
  fprintf ff "@[<v2>@[node %a%a = %a@] where@ %a@]@\n@."
    print_name n.n_name
    print_var_decs n.n_inputs
    print_var_decs n.n_outputs
    print_eqs n.n_eqs

let print_const_dec ff cd =
  fprintf ff "const %a = %a@\n@."
    print_name cd.c_name  print_static_exp cd.c_value

let print_program oc p =
  let ff = formatter_of_out_channel oc in
    List.iter (print_const_dec ff) p.p_consts;
    List.iter (print_node ff) p.p_nodes

