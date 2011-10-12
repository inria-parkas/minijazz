open Ast
open Static
open Location
open Errors

(** Inlines all nodes with static paramaters. *)

let expect_bool env se =
  match simplify env se with
    | SBool v -> v
    | _ ->
        let ff = Format.formatter_of_out_channel stdout in
          Printer.print_static_exp ff se; Format.fprintf ff "@.";
        Format.eprintf "Static instanciation to bool failed@.";
        NameEnv.iter (fun k _ -> Format.eprintf "%s " k) env;
        Format.eprintf "is_empty : %b @." (NameEnv.is_empty env);
        assert false

let expect_int env se =
  match simplify env se with
    | SInt v -> v
    | _ ->
        let ff = Format.formatter_of_out_channel stdout in
          Printer.print_static_exp ff se; Format.fprintf ff "@.";
          Format.eprintf "Static instanciation to int failed@."; raise Error


(** Function to create unique names *)
let used_names = ref IdentSet.empty
let reset_names () =
  used_names := IdentSet.empty
let add_names vds =
  List.iter (fun vd -> used_names := IdentSet.add vd.v_ident !used_names) vds

(** Find a node by name*)
let nodes_list = ref []
let find_node f =
  List.find (fun n -> f = n.n_name) !nodes_list

let vars_of_pat pat =
  let exp_of_ident id = mk_exp (Evar id) in
  let rec _vars_of_pat acc pat = match pat with
    | Evarpat id -> (exp_of_ident id)::acc
    | Etuplepat l -> List.fold_left (fun acc id -> (exp_of_ident id)::acc) acc l
  in
    _vars_of_pat [] pat

let ident_of_exp e = match e.e_desc with
  | Evar x -> x
  | _ -> assert false

let rename env vd =
  IdentEnv.add vd.v_ident (mk_exp (Evar (vd.v_ident ^(Misc.gen_symbol ())))) env

let build_params m names values =
  List.fold_left2 (fun m { p_name = n } v -> NameEnv.add n v m) m names values

let build_exp m vds values =
  List.fold_left2 (fun m { v_ident = n } e -> IdentEnv.add n e m) m vds values

let rec find_local_vars b = match b with
  | BEqs (_, vds) -> vds
  | BIf (_, trueb, falseb) -> (find_local_vars trueb) @ (find_local_vars falseb)

(** Substitutes idents with new names, static params with their values *)
module Subst =
struct
  let do_subst_op m op = match op with
    | OSelect se -> OSelect (simplify m se)
    | OSlice (idx1, idx2) ->
        OSlice (simplify m idx1, simplify m idx2)
    | OMem(b, se1, se2, file) ->
        OMem (b, simplify m se1, simplify m se2, file)
    | OCall(f, params) -> OCall(f, List.map (simplify m) params)
    | _ -> op

  let do_subst_pat subst pat =
    let translate_ident id =
      try ident_of_exp (IdentEnv.find id subst)
      with | Not_found -> id
    in
      match pat with
        | Evarpat id -> Evarpat (translate_ident id)
        | Etuplepat ids -> Etuplepat (List.map translate_ident ids)

  let rec do_subst_exp m subst e = match e.e_desc with
    | Evar x ->
        if IdentEnv.mem x subst then
          IdentEnv.find x subst
        else
          e
    | Eapp(op, args) ->
        { e with e_desc = Eapp(do_subst_op m op,
                              List.map (do_subst_exp m subst) args) }
    | Econst _ -> e

  let do_subst_eq m subst (pat, e) =
    (do_subst_pat subst pat, do_subst_exp m subst e)

  let rec do_subst_block m subst b = match b with
    | BEqs (eqs, vds) ->
        let eqs = List.map (do_subst_eq m subst) eqs in
          BEqs (eqs, vds)
    | BIf (se, trueb, falseb) ->
        let se = simplify m se in
        let trueb = do_subst_block m subst trueb in
        let falseb = do_subst_block m subst falseb in
          BIf(se, trueb, falseb)
end

let check_params loc param_names params cl =
  let env = build_params NameEnv.empty param_names params in
  try
    check_true env cl
  with Unsatisfiable(c) ->
    Format.eprintf "%aThe following constraint is not satisfied: %a@."
      print_location loc  Printer.print_static_exp c;
    raise Error

let rec inline_node loc m call_stack f params args pat =
  (* Check that the definition is sound *)
  if List.mem (f, params) call_stack then (
    Format.eprintf "The definition of %s is circular.@." f;
    raise Error
  );
  let call_stack = (f, params)::call_stack in

  (* do the actual work *)
  let n = find_node f in
  check_params loc n.n_params params n.n_constraints;
  let m = build_params m n.n_params params in
  let subst = build_exp IdentEnv.empty n.n_inputs args in
  let subst = build_exp subst n.n_outputs (vars_of_pat pat) in
  let locals = find_local_vars n.n_body in
  let subst = List.fold_left rename subst locals in
  let b = Subst.do_subst_block m subst n.n_body in
    Normalize.block b, call_stack

and translate_eq m subst call_stack acc ((pat, e) as eq) =
  let (pat, e) = Subst.do_subst_eq m subst eq in
  match e.e_desc with
    | Eapp(OCall(f, params), args) when not (Misc.is_empty params) ->
      let params = List.map (simplify m) params in
      let b, call_stack = inline_node e.e_loc m call_stack f params args pat in
      (translate_block m subst call_stack b)@acc
    (* Inline nodes that were declared inlined *)
    | Eapp(OCall(f, _), args) ->
        (try
            let n = find_node f in
              if n.n_inlined = Inlined then
                let b, call_stack = inline_node e.e_loc m call_stack f [] args pat in
                  (translate_block m subst call_stack b)@acc
              else
                eq::acc
          with
              Not_found -> (* Predefined function*)
                eq::acc
        )
    | Eapp(OMem(mem_kind, addr_size, word_size, f), args) ->
      let addr_size = simplify m addr_size in
      let word_size = simplify m word_size in
      let e = { e with e_desc = Eapp(OMem(mem_kind, addr_size, word_size, f), args) } in
      (pat, e)::acc
    | _ -> eq::acc

and translate_eqs m subst call_stack acc eqs =
  List.fold_left (translate_eq m subst call_stack) acc eqs

and translate_block m subst call_stack b =
  match b with
    | BEqs (eqs, vds) -> translate_eqs m subst call_stack [] eqs
    | BIf(se, trueb, elseb) ->
        if expect_bool m se then
          translate_block m subst call_stack trueb
        else
          translate_block m subst call_stack elseb

let node m n =
  (*Init state*)
  reset_names ();
  add_names n.n_inputs;
  add_names n.n_outputs;
  let call_stack = [(n.n_name, [])] in
  (*Do the translation*)
  let eqs = translate_block m IdentEnv.empty call_stack n.n_body in
    { n with n_body = BEqs (eqs, []) } (** TODO : var_decs*)

let build_cd env cd =
  NameEnv.add cd.c_name cd.c_value env

let program p =
  nodes_list := p.p_nodes;
  let m = List.fold_left build_cd NameEnv.empty p.p_consts in
  (* Find the nodes without static parameters *)
  let nodes = List.filter (fun n -> Misc.is_empty n.n_params) p.p_nodes in
  let nodes = List.map (fun n -> node m n) nodes in
    { p with p_nodes = nodes }
