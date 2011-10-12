open Ast
open Static
open Location
open Errors

(** Checks that names used in a node are defined.
    Adds local vars to node.n_locals.
    Also simplifies static exp when possible. *)

exception Unbound_identifier of ident

let simplify_ty env ty = match ty with
  | TBitArray se -> TBitArray (simplify env se)
  | _ -> ty

let simplify_vd env vd =
  { vd with v_ty = simplify_ty env vd.v_ty }

let rec check_exp const_env env e = match e.e_desc with
  | Evar id ->
    if not (IdentEnv.mem id env) then
      raise (Unbound_identifier id)
  | Eapp(_, args) -> List.iter (check_exp const_env env) args
  | _ -> ()

let pat_vars env l pat =
  let add_id env l id =
    if not (IdentEnv.mem id env) then
      (mk_var_dec id invalid_type)::l
    else
      l
  in
  match pat with
    | Evarpat id -> add_id env l id
    | Etuplepat ids -> List.fold_left (add_id env) l ids

let build_vd env vd = IdentEnv.add vd.v_ident vd env

let rec block const_env env b = match b with
  | BEqs (eqs, _) ->
      let locals = List.fold_left (fun l (pat, _) -> pat_vars env l pat) [] eqs in
      let env = List.fold_left build_vd env locals in
        (* check names in equations *)
        List.iter (fun (_, e) -> check_exp const_env env e) eqs;
        BEqs (eqs, locals)
  | BIf(se, trueb, elseb) ->
     BIf(simplify const_env se, block const_env env trueb,
        block const_env env elseb)

let node const_env n =
  let env = List.fold_left build_vd IdentEnv.empty n.n_inputs in
  let env =  List.fold_left build_vd env n.n_outputs in
    Modules.add_node n;
  (*simplify static exps in inputs/outputs types*)
  let body =
    try
      block const_env env n.n_body
    with
      | Unbound_identifier id ->
        Format.eprintf "%aThe identifier '%s' is unbound@." print_location n.n_loc  id;
        raise Error
  in
  { n with
    n_body = body;
    n_inputs = List.map (simplify_vd const_env) n.n_inputs;
    n_outputs = List.map (simplify_vd const_env) n.n_outputs }


let build_cd env cd =
  let v = simplify env cd.c_value in
  let env = NameEnv.add cd.c_name v env in
    { cd with c_value = v }, env

let program p =
  let consts, env = Misc.mapfold build_cd NameEnv.empty p.p_consts in
    { p with p_consts = consts; p_nodes = List.map (node env) p.p_nodes }

