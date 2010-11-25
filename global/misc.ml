
(* Functions to decompose a list into a tuple *)
exception Arity_error of int * int (*expected, found*)

let try_1 = function
  | [v] -> v
  | l -> raise (Arity_error(1, List.length l))

let try_2 = function
  | [v1; v2] -> v1, v2
  | l -> raise (Arity_error(2, List.length l))

let assert_fun f l =
  try
    f l
  with
      Arity_error(expected, found) ->
        Format.eprintf "Internal compiler error: \
     wrong list size (found %d, expected %d).@." found expected;
        assert false

let assert_1 l = assert_fun try_1 l
let assert_2 l = assert_fun try_2 l

let mapfold f acc l =
  let l,acc = List.fold_left
                (fun (l,acc) e -> let e,acc = f acc e in e::l, acc)
                ([],acc) l in
  List.rev l, acc

let unique l =
  let tbl = Hashtbl.create (List.length l) in
  List.iter (fun i -> Hashtbl.replace tbl i ()) l;
  Hashtbl.fold (fun key _ accu -> key :: accu) tbl []
