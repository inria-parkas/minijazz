open Ast
open Static
open Printer
open Errors
open Misc

exception Unify

type signature =
    { s_inputs : ty list;
      s_outputs : ty list;
      s_params : name list;
      s_constraints : static_exp list }

module Modules = struct
  let env = ref Ast.NameEnv.empty

  let tys_of_vds vds = List.map (fun vd -> vd.v_ty) vds

  let add_node n constr =
    let s = { s_inputs = tys_of_vds n.n_inputs;
              s_outputs = tys_of_vds n.n_outputs;
              s_params = List.map (fun p -> p.p_name) n.n_params;
              s_constraints = constr } in
    env := Ast.NameEnv.add n.n_name s !env

  let build_param_env param_names params =
    List.fold_left2
      (fun env pn p -> NameEnv.add pn p env)
      NameEnv.empty param_names params

  let subst_ty env ty = match ty with
    | TBitArray se -> TBitArray (subst env se)
    | _ -> ty

  let find_node n params =
    try
      let s = Ast.NameEnv.find n !env in
      let env = build_param_env s.s_params params in
      let s =
        { s with s_inputs = List.map (subst_ty env) s.s_inputs;
          s_outputs = List.map (subst_ty env) s.s_outputs;
          s_constraints = List.map (subst env) s.s_constraints }
      in
      s
    with Not_found ->
      Format.eprintf "Unbound node '%s'@." n; raise Error
end

let arity_error found expected =
 Format.eprintf "Wrong number of arguments (found '%d'; expected '%d')"
   found expected;
  raise Error

let type_error loc found_ty exp_ty =
  Format.eprintf "%aThis expression has type '%a' but '%a' was expected@."
    Location.print_location loc  print_type found_ty  print_type exp_ty; raise Error

let static_type_error loc found_ty exp_ty =
  Format.eprintf "%aThis static expression has type '%a' but '%a' was expected@."
    Location.print_location loc  print_static_type found_ty print_static_type exp_ty;
  raise Error

let constr_list = ref []
let add_constraint se =
  constr_list := se :: !constr_list
let get_constraints () =
  let v = !constr_list in
  constr_list := []; v

let fresh_static_var () =
  SVar ("s_"^(Misc.gen_symbol ()))

(* Functions on types*)

let fresh_type =
  let index = ref 0 in
  let gen_index () = (incr index; !index) in
  (** returns a new clock variable *)
  let fresh_type () = TVar (ref (TIndex (gen_index ()))) in
  fresh_type

(** returns the canonic (short) representant of [ty]
    and update it to this value. *)
let rec ty_repr ty = match ty with
  | TVar link ->
    (match !link with
      | TLink ty ->
        let ty = ty_repr ty in
        link := TLink ty;
        ty
      | _ -> ty)
  | _ -> ty

(** verifies that index is fresh in ck. *)
let rec occur_check index ty =
  let ty = ty_repr ty in
  match ty with
    | TUnit | TBit | TBitArray _  -> ()
    | TVar { contents = TIndex n } when index <> n -> ()
    | TProd ty_list -> List.iter (occur_check index) ty_list
    | _ -> raise Unify

let rec unify ty1 ty2 =
  let ty1 = ty_repr ty1 in
  let ty2 = ty_repr ty2 in
  if ty1 == ty2 then ()
  else
   match (ty1, ty2) with
     | TBitArray n1, TBitArray n2 -> add_constraint (SBinOp(SEqual, n1, n2))
     | TVar { contents = TIndex n1 }, TVar { contents = TIndex n2 } when n1 = n2 -> ()
     | TProd ty_list1, TProd ty_list2 ->
         List.iter2 unify ty_list1 ty_list2
     | TVar ({ contents = TIndex n } as link), ty
     | ty, TVar ({ contents = TIndex n } as link) ->
       occur_check n ty;
       link := TLink ty
     | _ -> raise Unify

let prod ty_list = match ty_list with
  | [ty] -> ty
  | _ -> TProd ty_list

(* Typing of static exps *)
let rec type_static_exp loc se = match se with
    | SInt _ | SVar _ -> STInt
    | SBool _ -> STBool
    | SBinOp((SAdd | SMinus | SMult | SDiv | SPower ), se1, se2) ->
      expect_static_exp loc se1 STInt;
      expect_static_exp loc se2 STInt;
      STInt
    | SBinOp((SEqual | SLess | SLeq | SGreater | SGeq), se1, se2) ->
      expect_static_exp loc se1 STInt;
      expect_static_exp loc se2 STInt;
      STBool

and expect_static_exp loc se ty =
  let found_ty = type_static_exp loc se in
  if found_ty <> ty then
    static_type_error loc found_ty ty

let solve_constr params cl =
  let params = List.map (fun p -> p.p_name) params in
  let rec find_simplification cl = match cl with
    | [] -> None, []
    | ((SBinOp(SEqual, SVar s, se))
          |(SBinOp(SEqual, se, SVar s)))::cl when not (List.mem s params) ->
      Some (s, se), cl
    | c::cl ->
      let res, cl = find_simplification cl in
      res, c::cl
  in
  let rec solve_one cl =
    let res, cl = find_simplification cl in
    match res with
      | None -> cl
      | Some (s, se) ->
        let env = NameEnv.add s se NameEnv.empty in
        let cl = List.map (subst env) cl in
        solve_one cl
  in
  solve_one cl

(* Typing of expressions *)
let rec type_exp env e =
  let desc, ty = match e.e_desc with
    | Econst (VBit _) -> e.e_desc, TBit
    | Econst (VBitArray a) -> e.e_desc, TBitArray (SInt (Array.length a))
    | Evar id -> Evar id, IdentEnv.find id env
    | Eapp (op, args) ->
      let args, ty = type_op env op args in
        Eapp(op, args), ty
  in
    { e with e_desc = desc; e_ty = ty }, ty

and expect_exp env e ty =
  let e, found_ty = type_exp env e in
    try
      unify ty found_ty;
      e
    with
        Unify -> type_error e.e_loc found_ty ty

and type_op env op args = match op with
  | OReg | OCall ("not", []) ->
    let e = assert_1 args in
    let e = expect_exp env e TBit in
    [e], TBit
  | OCall ("and", []) | OCall ("xor", []) | OCall ("or", []) ->
    let e1, e2 = assert_2 args in
    let e1 = expect_exp env e1 TBit in
    let e2 = expect_exp env e2 TBit in
    [e1; e2], TBit
  | OSelect i ->
    let e = assert_1 args in
    let n = fresh_static_var () in
    add_constraint (SBinOp(SLeq, i, n)); (* i = n *)
    let e = expect_exp env e (TBitArray n) in
    [e], TBit
  | OConcat ->
    let e1, e2 = assert_2 args in
    let n1 = fresh_static_var () in
    let n2 = fresh_static_var () in
    let e1 = expect_exp env e1 (TBitArray n1) in
    let e2 = expect_exp env e2 (TBitArray n2) in
    [e1; e2], TBitArray (SBinOp(SAdd, n1, n2))
  | OSlice (i1, i2) ->
    let e = assert_1 args in
    let n = fresh_static_var () in
    let e = expect_exp env e (TBitArray n) in
    add_constraint (SBinOp(SLess, SInt 0, i1)); (* 0 <= i1 *)
    add_constraint (SBinOp(SLess, i2, n)); (* i2 <= n *)
    let size = SBinOp(SAdd, (SBinOp(SMinus, i2, i1)), SInt 1) in (* size = i2 - i1 + 1 *)
    [e], TBitArray size
  | OMem (MRom, addr_size, word_size, _) ->
    let read_addr = assert_1 args in
    let read_addr = expect_exp env read_addr (TBitArray addr_size) in
      [read_addr], TBitArray word_size
  | OMem (MRam, addr_size, word_size, _) ->
    let read_addr, write_addr, data_in, write_en = assert_4 args in
    let read_addr = expect_exp env read_addr (TBitArray addr_size) in
    let write_addr = expect_exp env write_addr (TBitArray addr_size) in
    let data_in = expect_exp env data_in (TBitArray word_size) in
    let write_en = expect_exp env write_en TBit in
      [read_addr; write_addr; data_in; write_en], TBitArray word_size
  | OCall (f, params) ->
    (*todo: type params*)
    let s = Modules.find_node f params in

    (*check arity*)
    if List.length s.s_inputs <> List.length args then
      arity_error (List.length args) (List.length s.s_inputs);
    (*check types of all arguments*)
    let args = List.map2 (expect_exp env) args s.s_inputs in
      args, prod s.s_outputs

let type_pat env pat = match pat with
  | Evarpat x -> IdentEnv.find x env
  | Etuplepat id_list -> prod (List.map (fun x -> IdentEnv.find x env) id_list)

let type_eq env (pat, e) =
  let pat_ty = type_pat env pat in
  let e = expect_exp env e pat_ty in
    (pat, e)

let build env vds =
  let build_one env vd = IdentEnv.add vd.v_ident vd.v_ty env in
    List.fold_left build_one env vds

let rec type_block env b = match b with
  | BEqs(eqs, vds) ->
    let vds = List.map (fun vd -> { vd with v_ty = fresh_type () }) vds in
    let env = build env vds in
    let eqs = List.map (type_eq env) eqs in
    BEqs(eqs,vds)
  | BIf(e, trueb, falseb) ->
    (*todo: type static exps *)
    let trueb = type_block env trueb in
    let falseb = type_block env falseb in
    BIf(e, trueb, falseb)

let rec repr_ty_block b = match b with
  | BEqs(eqs, vds) ->
    let vds = List.map (fun vd -> { vd with v_ty = ty_repr vd.v_ty }) vds in
    BEqs(eqs,vds)
  | BIf(e, trueb, falseb) ->
    expect_static_exp Location.no_location e STBool;
    let trueb = repr_ty_block trueb in
    let falseb = repr_ty_block falseb in
    BIf(e, trueb, falseb)

let node n =
  Modules.add_node n [];
  let env = build IdentEnv.empty n.n_inputs in
  let env = build env n.n_outputs in
  let body = type_block env n.n_body in
  let body = repr_ty_block body in
  let constr = get_constraints () in
  let constr = solve_constr n.n_params constr in
  Modules.add_node n constr;
    { n with n_body = body; n_constraints = constr }

let program p =
  let p_nodes = List.map node p.p_nodes in
    { p with p_nodes = p_nodes }
