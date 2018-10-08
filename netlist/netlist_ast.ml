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

type ident = string

(* Environment using ident as key *)
module Env = struct
  include Map.Make(struct
    type t = ident
    let compare = compare
  end)

  let of_list l =
    List.fold_left (fun env (x, ty) -> add x ty env) empty l
end

type ty = TBit | TBitArray of int
type value = VBit of bool | VBitArray of bool array

type binop = Or | Xor | And | Nand

(* argument of operators (variable or constant) *)
type arg =
    | Avar of ident (* x *)
    | Aconst of value (* constant *)

(* Expressions (see MiniJazz documentation for more info on the operators) *)
type exp =
    | Earg of arg (* a: argument *)
    | Ereg of ident (* REG x : register *)
    | Enot of arg (* NOT a *)
    | Ebinop of binop * arg * arg (* OP a1 a2 : boolean operator *)
    | Emux of arg * arg * arg (* MUX a1 a2 : multiplexer *)
    | Erom of int (*addr size*) * int (*word size*) * arg (*read_addr*)
        (* ROM addr_size word_size read_addr *)
    | Eram of int (*addr size*) * int (*word size*)
        * arg (*read_addr*) * arg (*write_enable*)
        * arg (*write_addr*) * arg (*data*)
        (* RAM addr_size word_size read_addr write_enable write_addr data *)
    | Econcat of arg * arg (* CONCAT a1 a2 : concatenation of arrays *)
    | Eslice of int * int * arg
      (* SLICE i1 i2 a : extract the slice of a between indices i1 and i2 *)
    | Eselect of int * arg
      (* SELECT i a : ith element of a *)

(* equations: x = exp *)
type equation = ident * exp

type program =
    { p_eqs : equation list; (* equations *)
      p_inputs : ident list; (* inputs *)
      p_outputs : ident list; (* outputs *)
      p_vars : ty Env.t; } (* maps variables to their types*)
