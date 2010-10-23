open Format

let react cmd args = match cmd with
  | 'l' -> printf "Received 'l' with args: '%s'@." args
  | 'c' -> printf "Received 'c' with args: '%s'@." args
  | 'q' -> printf "Exiting@."; exit 0
  | _ -> printf "Unknown option"

let rec main () =
  printf "@\nPlease input command (l, c, q):@.";
  let s = read_line () in
    if String.length s > 0 then
      let cmd = s.[0] in
      let args = if String.length s < 2 then "" else
          String.sub s 2 ((String.length s) - 2) in
    react cmd args;
    main ()

let () =
  main ()
