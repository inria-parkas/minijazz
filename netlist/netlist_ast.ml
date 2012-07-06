type ident = string

module Env = Map.Make(struct
  type t = ident
  let compare = compare
end)

type ty = TBit | TBitArray of int
type value = VBit of bool | VBitArray of bool list

type binop = Or | Xor | And | Nand

type arg =
    | Avar of ident
    | Aconst of value

type exp =
    | Econst of value
    | Ereg of ident
    | Enot of arg
    | Ebinop of binop * arg * arg
    | Emux of arg * arg * arg
    | Erom of int (*addr size*) * int (*word size*) * arg (*read_addr*)
    | Eram of int (*addr size*) * int (*word size*)
        * arg (*read_addr*) * arg (*write_enable*)
        * arg (*write_addr*) * arg (*data*)
    | Econcat of arg * arg
    | Eslice of int * int * arg
    | Eselect of int * arg

type equation = ident * exp

type program =
    { p_eqs : equation list;
      p_vars : ty Env.t; }
