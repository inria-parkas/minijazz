
(* Functions to decompose a list into a tuple *)
let _arity_error i l =
  Format.eprintf "Internal compiler error: \
     wrong list size (found %d, expected %d).@." (List.length l) i;
  assert false

let assert_1 = function
  | [v] -> v
  | l -> _arity_error 1 l

let assert_2 = function
  | [v1; v2] -> v1, v2
  | l -> _arity_error 2 l

let mapfold f acc l =
  let l,acc = List.fold_left
                (fun (l,acc) e -> let e,acc = f acc e in e::l, acc)
                ([],acc) l in
  List.rev l, acc
