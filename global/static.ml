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

open Errors
open Location

type name = string
module NameEnv = Map.Make (struct type t = name let compare = compare end)

type sop =
  | SAdd | SMinus | SMult | SDiv | SPower (*int*)
  | SEqual | SLess | SLeq | SGreater | SGeq (*bool*)

type static_exp_desc =
  | SInt of int
  | SBool of bool
  | SVar of name
  | SBinOp of sop * static_exp * static_exp
  | SIf of static_exp * static_exp * static_exp (* se1 ? se2 : se3 *)

and static_exp =
    { se_desc : static_exp_desc;
      se_loc : location }

type static_ty = STInt | STBool

let mk_static_exp ?(loc = no_location) desc =
  { se_desc = desc; se_loc = loc }
let mk_static_var s =
  mk_static_exp (SVar s)
let mk_static_int i =
  mk_static_exp (SInt i)
let mk_static_bool b =
  mk_static_exp (SBool b)
