open Cli_options
open Octogon_compiler

let main () =
  try
    Arg.parse
      [
        "-v",Arg.Set verbose, doc_verbose;
        "-version", Arg.Unit show_version, doc_version;
        "-print-types", Arg.Set print_types, doc_full_type_info;
      ]
      compile_impl
      errmsg;
  with
    | Errors.Error -> exit 2;;

main ()


