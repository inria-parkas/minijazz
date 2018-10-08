(***********************************************************************)
(*                                                                     *)
(*                             MiniJazz                                *)
(*                                                                     *)
(* Cedric Pasteur, Parkas, ENS                                         *)
(*                                                                     *)
(* Copyright 2013 ENS                                                  *)
(*                                                                     *)
(* This file is part of the MiniJazz compiler.                         *)
(*                                                                     *)
(* MiniJazz is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published by   *)
(* the Free Software Foundation, either version 3 of the License, or   *)
(* (at your option) any later version.                                 *)
(*                                                                     *)
(* MiniJazz is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(* GNU General Public License for more details.                        *)
(*                                                                     *)
(* You should have received a copy of the GNU General Public License   *)
(* along with MiniJazz.  If not, see <http://www.gnu.org/licenses/>    *)
(*                                                                     *)
(***********************************************************************)

open Ast
open Static

let is_not_zero ty = match ty with
  | TBitArray { se_desc = SInt 0 } -> false
  | _ -> true

let rec simplify_exp e = match e.e_desc with
  (* replace x[i..j] with [] if j < i *)
  | Ecall("slice",
         [{ se_desc = SInt min };
          { se_desc = SInt max }; n], _) when max < min ->
      { e with e_desc = Econst (VBitArray (Array.make 0 false)) }
  (* replace x[i..i] with x[i] *)
  | Ecall("slice", [min; max; n], args) when min = max ->
      let new_e = { e with e_desc = Ecall("select", [min; n], args) } in
      simplify_exp new_e
  (* replace x.[] or [].x with x *)
  | Ecall("concat", _, [{ e_ty = TBitArray { se_desc = SInt 0 } }; e1])
  | Ecall("concat", _, [e1; { e_ty = TBitArray { se_desc = SInt 0 } }]) ->
      e1
  | Ecall(f, params, args) ->
      { e with e_desc = Ecall(f, params, List.map simplify_exp args) }
  | _ -> e

let simplify_eq (pat,e) =
  (pat, simplify_exp e)

let rec block b = match b with
  | BEqs(eqs, vds) ->
      let eqs = List.map simplify_eq eqs in
      (* remove variables with size 0 *)
      let vds = List.filter (fun vd -> is_not_zero vd.v_ty) vds in
      let eqs = List.filter (fun (_, e) -> is_not_zero e.e_ty) eqs in
      BEqs(eqs, vds)
  | BIf(se, trueb, elseb) -> BIf(se, block trueb, block elseb)

let node n =
  { n with n_body = block n.n_body }

let program p =
    { p with p_nodes = List.map node p.p_nodes }
