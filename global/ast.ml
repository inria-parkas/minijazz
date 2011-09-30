open Location
open Static

type ident = string
type name = string

module IdentEnv = Map.Make (struct type t = ident let compare = compare end)
module IdentSet = Set.Make (struct type t = ident let compare = compare end)

module NameEnv = Map.Make (struct type t = name let compare = compare end)
module NameSet = Set.Make (struct type t = name let compare = compare end)

type ty =
  | TUnit | TBit | TBitArray of static_exp | TProd of ty list
  | TVar of link ref
and link =
  | TIndex of int
  | TLink of ty
let invalid_type = TUnit

type mem_kind = MRom | MRam

type op =
  | OReg
  | OMem of mem_kind * static_exp * static_exp * string option
      (* ro * address size * word size * input file *)
  | OCall of name * static_exp list (*function, params*)
  | OSelect of static_exp
  | OSlice of static_exp * static_exp
  | OConcat

type value =
  | VBit of bool
  | VBitArray of bool array

type edesc =
  | Econst of value
  | Evar of ident
  | Eapp of op * exp list

and exp = {
  e_desc : edesc;
  e_ty : ty;
  e_loc: location;
}

type pat =
  | Evarpat of ident
  | Etuplepat of ident list

type equation = pat * exp

type var_dec = {
  v_ident : ident;
  v_ty : ty;
}

type param = {
  p_name : name;
}

type block =
    | BEqs of equation list * var_dec list
    | BIf of static_exp * block * block

type node_dec = {
  n_name : name;
  n_loc: location;
  n_inputs : var_dec list;
  n_outputs : var_dec list;
  n_params : param list;
  n_constraints : static_exp list;
  n_body : block;
}

type const_dec = {
  c_name : name;
  c_loc : location;
  c_value : static_exp;
}

type program = {
  p_consts : const_dec list;
  p_nodes : node_dec list;
}


let mk_exp ?(loc = no_location) desc =
  { e_desc = desc; e_loc = loc; e_ty = invalid_type}

let mk_const_dec ?(loc = no_location) n se =
  { c_name = n; c_loc = loc; c_value = se }

let mk_equation pat e = (pat, e)

let mk_var_dec n ty =
  { v_ident = n; v_ty = ty }

let mk_param n =
  { p_name = n }

let mk_node n loc inputs outputs params b =
  { n_name = n; n_inputs = inputs; n_outputs = outputs;
    n_body = b; n_params = params; n_constraints = [];
    n_loc = loc }

let mk_program cds nds =
  { p_consts = cds; p_nodes = nds }


let unifty t1 t2 = (t1 = t2)
