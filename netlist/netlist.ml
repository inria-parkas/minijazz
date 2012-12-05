exception Parse_error of string

let find_file filename =
  try
    open_in filename
  with
    | _ -> raise (Parse_error ("No such file '"^filename^"%s'"))

(** [read_file filename] reads the [filename] file and outputs the corresponding
    Netlist_ast.program.*)
let read_file filename =
  let ic = find_file filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    Netlist_parser.program Netlist_lexer.token lexbuf
  with
    | e -> raise (Parse_error ("Syntax error (exception: "^(Printexc.to_string e)^")"))

(** [print_program oc p] prints the program [p] on the output channel [oc].
    For instance, to print on the standard output, use [print_program stdout p].
    To print to a file named 'filename', use the following:
        let out = open_out filename in
        print_program out p
*)
let print_program = Netlist_printer.print_program
