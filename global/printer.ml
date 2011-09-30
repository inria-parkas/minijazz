open Ast
open Static
open Format
open Misc

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
  | VBitArray l -> Array.iter (print_bool ff) l

let rec print_static_exp ff se = match se with
  | SInt i -> fprintf ff "%d" i
  | SBool b -> print_bool ff b
  | SVar n -> print_name ff n
  | SBinOp(op, se1, se2) ->
    let op_str = match op with
      | SAdd -> "+" | SMinus -> "-"
      | SMult -> "*" | SDiv -> "/"
      | SPower -> "^" | SEqual -> "="
      | SLess -> "<" | SLeq -> "<="
      | SGreater -> ">" | SGeq -> ">=" in
      fprintf ff "(%a %s %a)" print_static_exp se1  op_str  print_static_exp se2

let rec print_static_type ff sty = match sty with
  | STInt -> fprintf ff "int"
  | STBool -> fprintf ff "bool"

let rec print_type ff ty = match ty with
  | TUnit -> fprintf ff "()"
  | TBit -> fprintf ff "bit"
  | TBitArray se -> fprintf ff "bit[%a]" print_static_exp se
  | TProd l ->  print_list_r print_type "" "*" "" ff l
  | TVar _ -> fprintf ff "<var>"

let print_call_params ff params = match params with
  | [] -> ()
  | _ -> print_list_r print_static_exp "<<"","">>" ff params

let print_op ff op = match op with
  | OReg -> fprintf ff "reg"
  | OMem(MRom, addr_size, word_size, _) ->
    fprintf ff "rom<%a,%a>"
      print_static_exp addr_size  print_static_exp word_size
  | OMem(MRam, addr_size, word_size, _) ->
    fprintf ff "ram<%a,%a>"
      print_static_exp addr_size  print_static_exp word_size
  | OCall (fn, params) ->
      fprintf ff "%a%a"  print_name fn  print_call_params params
  | OSelect _ | OSlice _ | OConcat -> assert false

let rec print_exp ff e =
  (*if print_types then
      fprintf ff "%a : %a" print_edesc e.e_desc print_
    else*)
  fprintf ff "%a" print_edesc e.e_desc

and print_edesc ff ed = match ed with
  | Econst v -> print_const ff v
  | Evar n -> print_ident ff n
  | Eapp(OSelect idx, args) ->
      let e1 = assert_1 args in
        fprintf ff "%a[%a]" print_exp e1  print_static_exp idx
  | Eapp(OSlice(low, high), args) ->
      let e1 = assert_1 args in
        fprintf ff "%a[%a..%a]"
          print_exp e1  print_static_exp low  print_static_exp high
  | Eapp(OConcat, args) ->
      let e1, e2 = assert_2 args in
        fprintf ff "%a . %a" print_exp e1  print_exp e2
  | Eapp(op, args) -> fprintf ff "%a%a" print_op op  print_args args

and print_args ff args =
  print_list_r print_exp "(" "," ")" ff args

(*and print_prim ff prim args = match prim with
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
      fprintf ff "%s(%a)" s  print_exp e1*)

let rec print_pat ff pat = match pat with
  | Evarpat id -> print_ident ff id
  | Etuplepat l -> print_list_r print_ident "(" "," ")" ff l

let print_eq ff (pat, e) =
  fprintf ff "%a = %a" print_pat pat  print_exp e

let print_eqs ff eqs =
  print_list_nlr print_eq """;""" ff eqs

let print_var_dec ff vd = match vd.v_ty with
  | TUnit -> fprintf ff "%a : ." print_name vd.v_ident
  | TBit -> fprintf ff "%a" print_name vd.v_ident
  | TBitArray se ->
    fprintf ff "%a : [%a]" print_name vd.v_ident  print_static_exp se
  | TProd _ -> assert false
  | TVar _ -> fprintf ff "%a : <var>" print_name vd.v_ident

let print_var_decs ff vds =
  print_list_r print_var_dec "("","")" ff vds

let rec print_block ff b = match b with
  | BEqs (eqs, []) -> print_eqs ff eqs
  | BEqs (eqs, vds) ->
    fprintf ff "var %a in@,%a" print_var_decs vds print_eqs eqs
  | BIf(se, thenb, elseb) ->
      fprintf ff "@[<v 2>if %a then@,%a@]@,@[<v 2>else@,%a@]"
        print_static_exp se
        print_block thenb
        print_block elseb

let print_param ff p =
  print_name ff p.p_name

let print_params ff params = match params with
  | [] -> ()
  | _ -> print_list_r print_param "<<"","">>" ff params

let print_node ff n =
  fprintf ff "@[<v2>@[node %a%a%a = %a@] where@ %a@]@\n@."
    print_name n.n_name
    print_params n.n_params
    print_var_decs n.n_inputs
    print_var_decs n.n_outputs
    print_block n.n_body

let print_const_dec ff cd =
  fprintf ff "const %a = %a@\n@."
    print_name cd.c_name  print_static_exp cd.c_value

let print_program oc p =
  let ff = formatter_of_out_channel oc in
    List.iter (print_const_dec ff) p.p_consts;
    List.iter (print_node ff) p.p_nodes

