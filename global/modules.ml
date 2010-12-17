open Ast

type node_sig = {
  node_inputs : var_dec list;
  node_outputs : var_dec list;
  node_params : param list }

type global_env = {
  mutable nodes : node_sig NameEnv.t ;
}

let global_env = { nodes = NameEnv.empty }

exception Already_defined

let add_node_from_sig name s =
    if NameEnv.mem name global_env.nodes then
      raise Already_defined;
    global_env.nodes <- NameEnv.add name s global_env.nodes

let add_node n =
  let s = { node_inputs = n.n_inputs; node_outputs = n.n_outputs;
            node_params = n.n_params } in
    add_node_from_sig n.n_name s

let find_node n =
  NameEnv.mem n global_env.nodes

let add_pervasives () =
  let add_sig ?(params = []) n inp outp =
    let s = { node_inputs = inp; node_outputs = outp; node_params = params } in
      add_node_from_sig n s
  in
    add_sig "and" [mk_var_dec "a" TBit; mk_var_dec "b" TBit]
      [mk_var_dec "o" TBit];
    add_sig "xor" [mk_var_dec "a" TBit; mk_var_dec "b" TBit]
      [mk_var_dec "o" TBit];
    add_sig "or" [mk_var_dec "a" TBit; mk_var_dec "b" TBit]
      [mk_var_dec "o" TBit];
    add_sig "not" [mk_var_dec "a" TBit] [mk_var_dec "o" TBit];
    add_sig "mux" [mk_var_dec "a" TBit; mk_var_dec "b" TBit;
                   mk_var_dec "c" TBit] [mk_var_dec "o" TBit]
(*  add_sig "select" [mk_var_dec "a" (TBitArray (SVar "n"))]
    [mk_var_dec "o" TBit] ~params:["n"];
  let size = SBinOp (SMinus, SBinOp(SAdd, SVar "j", SInt 1), SVar "i") in
    add_sig "slice" [mk_var_dec "a" (TBitArray (SVar "n"))]
      [mk_var_dec "o" (TBitArray size)] ~params:["n", "i", "j"];
    add_sig "concat"
      [mk_var_dec "a" (TBitArray (SVar "n")); mk_var_dec "b" (TBitArray (SVar "m"))]
      [mk_var_dec "o" (TBitArray (SAdd, SVar "n", SVar "m"))]
      ~params:["n", "m"] *)

let () =
  add_pervasives ()

