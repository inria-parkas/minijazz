open Ast
open Format

let print_name ff n = fprintf ff "%s" n
let print_ident ff id = fprintf ff "%s" id

let rec print_list_r print lp sep rp ff = function
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
  | SBinOp(op, se1, se2) ->
    let op_str = match op with
      | SAdd -> "+" | SMinus -> "-"
      | SMult -> "*" | SDiv -> "/"
      | SPower -> "^" in
      fprintf ff "(%a %s %a)" print_static_exp se1  op_str  print_static_exp se2

let print_prim ff prim =
  let s = match prim with
    | PAnd -> "and"   | POr -> "or"
    | PNand -> "nand" | PMux -> "mux"
    | PXor -> "xor"   | PNot -> "not"
  in
    fprintf ff "%s" s

let print_op ff op = match op with
  | OReg -> fprintf ff "reg"
  | OPrim p -> print_prim ff p
  | OMem(true, addr_size, word_size) -> (*ROM*)
    fprintf ff "rom<%d,%d>" addr_size word_size
  | OMem(false, addr_size, word_size) -> (*ROM*)
    fprintf ff "ram<%d,%d>" addr_size word_size
  | OCall fn -> print_name ff fn

let rec print_exp ff e =
  (*if print_types then
      fprintf ff "%a : %a" print_edesc e.e_desc print_
    else*)
  fprintf ff "%a" print_edesc e.e_desc

and print_edesc ff ed = match ed with
  | Econst v -> print_const ff v
  | Evar n -> print_ident ff n
  | Eapp(op, args) -> fprintf ff "%a%a" print_op op  print_args args

and print_args ff args =
  print_list_r print_exp "(" "," ")" ff args

let rec print_pat ff pat = match pat with
  | Evarpat id -> print_ident ff id
  | Etuplepat l -> print_list_r print_ident "(" "," ")" ff l

let print_eq ff (pat, e) =
  fprintf ff "%a = %a;" print_pat pat  print_exp e

let print_eqs ff eqs =
  print_list_r print_eq """@,""" ff eqs

let print_var_dec ff vd = match vd.v_ty with
  | TBit -> fprintf ff "%a" print_name vd.v_ident
  | TBitArray i -> fprintf ff "%a : [%d]" print_name vd.v_ident  i

let print_var_decs ff vds =
  print_list_r print_var_dec "("","")" ff vds

let print_node ff n =
  fprintf ff "@[node %a%a = %a@] where@\n[<v2>%a]@\n@."
    print_name n.n_name
    print_var_decs n.n_inputs
    print_var_decs n.n_outputs
    print_eqs n.n_eqs

let print_const_dec ff cd =
  fprintf ff "const %a = %a" print_name cd.c_name  print_static_exp cd.c_value

let print_program oc p =
  let ff = formatter_of_out_channel oc in
    List.iter (print_const_dec ff) p.p_consts;
    List.iter (print_node ff) p.p_nodes

