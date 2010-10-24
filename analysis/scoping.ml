open Ast
open Location
open Errors

let fun_of_op op = match op with
  | SAdd -> (+) | SMinus -> (-)
  | SMult -> (fun i1 i2 -> i1 * i2)
  | _ -> assert false

let rec simplify env se = match se with
  | SConst _ -> se
  | SVar n ->
    (try
      simplify env (NameEnv.find n env)
    with
      | Not_found ->
        Format.eprintf  "The name '%s' is unbound@." n; raise Error)
  | SBinOp(op, se1, se2) ->
    (match simplify env se1, simplify env se2 with
      | SConst i1, SConst i2 ->
        let f = fun_of_op op in
          SConst (f i1 i2)
      | _, _ -> Format.eprintf "Static instanciation failed@."; raise Error)

let simplify_ty env ty = match ty with
  | TBitArray se -> TBitArray (simplify env se)
  | _ -> ty

let simplify_vd env vd =
  { vd with v_ty = simplify_ty env vd.v_ty }

let rec check_exp const_env env e = match e.e_desc with
  | Evar id ->
    if not (IdentEnv.mem id env) then (
      Format.eprintf "The identifier '%s' is unbound@." id; raise Error)
  | Eapp(_, args) -> List.iter (check_exp const_env env) args
  | _ -> ()

let node const_env n =
  let build_vd env vd = IdentEnv.add vd.v_ident vd env in
  let add_id l id = (mk_var_dec id invalid_type)::l in
  let pat_vars l pat = match pat with
    | Evarpat id -> add_id l id
    | Etuplepat ids -> List.fold_left add_id l ids
  in
  let env = List.fold_left build_vd IdentEnv.empty n.n_inputs in
  let env =  List.fold_left build_vd env n.n_outputs in
  (* add local vars to the env *)
  let locals = List.fold_left
    (fun l (pat, _) -> pat_vars l pat) [] n.n_eqs in
  let env = List.fold_left build_vd env locals in
    (* check names in equations *)
    List.iter (fun (_, e) -> check_exp const_env env e) n.n_eqs;

  (*simplify static exps in inputs/outputs types*)
  { n with n_locals = locals;
    n_inputs = List.map (simplify_vd const_env) n.n_inputs;
    n_outputs = List.map (simplify_vd const_env) n.n_outputs }


let build_cd env cd =
  let v = simplify env cd.c_value in
  let env = NameEnv.add cd.c_name v env in
    { cd with c_value = v }, env

let program p =
  let consts, env = Misc.mapfold build_cd NameEnv.empty p.p_consts in
    { p with p_consts = consts; p_nodes = List.map (node env) p.p_nodes }

