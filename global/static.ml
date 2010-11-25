open Errors

type name = string
module NameEnv = Map.Make (struct type t = name let compare = compare end)

type sop =
  | SAdd | SMinus | SMult | SDiv | SPower (*int*)
  | SEqual | SLess | SLeq | SGreater | SGeq (*bool*)

type static_exp =
  | SInt of int
  | SBool of bool
  | SVar of name
  | SBinOp of sop * static_exp * static_exp

let fun_of_op op = match op with
  | SAdd -> (+) | SMinus -> (-)
  | SMult -> (fun i1 i2 -> i1 * i2)
  | _ -> assert false

let fun_of_comp_op op = match op with
  | SEqual -> (=) | SLeq -> (<=)
  | _ -> assert false

let rec simplify env se = match se with
  | SInt _ | SBool _ -> se
  | SVar n ->
    (try
       simplify env (NameEnv.find n env)
     with
       | Not_found ->
         Format.eprintf "The name '%s' is unbound@." n; raise Error)
  | SBinOp(op, se1, se2) ->
    (match op, simplify env se1, simplify env se2 with
      | (SAdd | SMinus | SDiv  | SMult), SInt i1, SInt i2 ->
          let f = fun_of_op op in
            SInt (f i1 i2)
      | (SEqual | SLess | SLeq | SGreater | SGeq), SInt i1, SInt i2 ->
          let f = fun_of_comp_op op in
            SBool (f i1 i2)
      | _, _, _ -> SBinOp(op, se1, se2))

let expect_const env se =
  match simplify env se with
    | SInt v -> SInt v
    | _ -> Format.eprintf "Static instanciation failed@."; raise Error

