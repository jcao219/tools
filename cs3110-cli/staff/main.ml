open Core.Std

(* Use OCAMLRUNPARAM to enable stack traces, unless you've set your own. *)
let config_env () =
  begin match (Sys.getenv "OCAMLRUNPARAM") with
    | Some _ -> ()
    | None   -> Unix.putenv ~key:"OCAMLRUNPARAM" ~data:"b"
  end

(* available subcommands *)
let targets = [
  ("clean", Clean.command)
]

(* TODO define staff & student commands separately *)
let main =
  Command.group
    ~summary:"cs3110 command line multitool"
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The highly recommended way to compile, test, and execute OCaml programs.";
      "Each subcommand handles one stage of the development process."
    ])
    targets

let _ =
  let () = config_env () in
  Command.run
    ~version:"2.0"
    ~build_info:"cs3110-cli by Ben C and Ben G. Built using Jane Street's Core library"
    main
