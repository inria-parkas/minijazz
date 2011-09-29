open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let df = function
  | After_rules ->
      (* Tell ocamlbuild about Menhir library (needed by --table). *)
      ocaml_lib ~extern:true ~dir:"+menhirLib" "menhirLib";

      (* Menhir does not come with menhirLib.cmxa so we have to manually by-pass
         OCamlbuild's built-in logic and add the needed menhirLib.cmxa. *)
      flag ["link"; "native"; "link_menhirLib"] (S [A "-I"; A "+menhirLib";
                                                    A "menhirLib.cmx"]);
      flag ["link"; "byte"; "link_menhirLib"] (S [A "-I"; A "+menhirLib";
                                                  A "menhirLib.cmo"]);

      flag ["ocaml"; "parser" ; "menhir" ; "use_menhir"] (S[A"--explain";
                                                            A"--table"]);

      flag ["ocaml"; "compile" ] (S[A"-w"; A"Ae"; A"-warn-error"; A"PU"]);

      (* Tell ocamlbuild about the ocamlgraph library. *)
      ocaml_lib ~extern:true ~dir:"+ocamlgraph" "graph"

  | _ -> ()

let _ = dispatch df
