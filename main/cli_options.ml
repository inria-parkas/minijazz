(* version of the compiler *)
let version = "0.1"

let verbose = ref false
let print_types = ref false

let show_version () =
  Format.printf "The Octagon compiler, version %s @." version
let errmsg = "Options are:"

let doc_verbose = "\t\t\tSet verbose mode"
and doc_version = "\t\tThe version of the compiler"
and doc_full_type_info = "\t\t\tPrint full type information"
