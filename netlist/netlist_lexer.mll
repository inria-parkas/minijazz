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

{
open Lexing
open Netlist_parser
exception Eof

let keyword_list =
[
  "AND", AND;
  "CONCAT", CONCAT;
  "IN", IN;
  "INPUT", INPUT;
  "MUX", MUX;
  "NAND", NAND;
  "NOT", NOT;
  "OR", OR;
  "OUTPUT", OUTPUT;
  "RAM", RAM;
  "REG", REG;
  "ROM", ROM;
  "SELECT", SELECT;
  "SLICE", SLICE;
  "VAR", VAR;
  "XOR", XOR;
]

}

rule token = parse
    '\n'
      { new_line lexbuf; token lexbuf }     (* skip blanks *)
  | [' ' '\t']
      { token lexbuf }     (* skip blanks *)
  | "="            { EQUAL }
  | ":"            { COLON }
  | ","            { COMMA }
  | ['0'-'9']+ as lxm { CONST lxm }
  | ('_' ? ['A'-'Z' 'a'-'z']('_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9']) * as id)
      { let s = Lexing.lexeme lexbuf in
        try List.assoc s keyword_list
        with Not_found -> NAME id }
  | eof            { EOF }
