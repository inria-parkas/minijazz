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

type static_ty = STInt | STBool

let pow a n =
  let rec g p x = function
  | 0 -> x
  | i ->
      g (p * p) (if i mod 2 = 1 then p * x else x) (i/2)
  in
  g a 1 n
;;


let fun_of_op op = match op with
  | SAdd -> (+) | SMinus -> (-)
  | SMult -> (fun i1 i2 -> i1 * i2)
  | SDiv -> (/)
  | SPower -> pow
  | _ -> assert false

let fun_of_comp_op op = match op with
  | SEqual -> (=) | SLeq -> (<=)  | SLess -> (<)
  | _ -> assert false

let rec _simplify is_rec env se = match se with
  | SInt _ | SBool _ -> se
  | SVar n ->
      (try
         let se = NameEnv.find n env in
         if is_rec then
          _simplify is_rec env se
         else
           se
        with
          | Not_found -> se)
  | SBinOp(op, se1, se2) ->
    (match op, _simplify is_rec env se1, _simplify is_rec env se2 with
      | (SAdd | SMinus | SDiv  | SMult | SPower), SInt i1, SInt i2 ->
          let f = fun_of_op op in
            SInt (f i1 i2)
      | (SEqual | SLess | SLeq | SGreater | SGeq), SInt i1, SInt i2 ->
          let f = fun_of_comp_op op in
            SBool (f i1 i2)
      | _, se1, se2 -> SBinOp(op, se1, se2))

let simplify = _simplify true
let subst = _simplify false

exception Unsatisfiable of static_exp
let check_true env cl =
  let check_one c =
    let res = simplify env c in
    match res with
      | SBool true -> ()
      | _ -> raise (Unsatisfiable c)
  in
  List.iter check_one cl
