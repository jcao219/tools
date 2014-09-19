open Core.Std

(* Use OCAMLRUNPARAM to enable stack traces, unless you've set your own. *)
let config_env () =
  begin match (Sys.getenv "OCAMLRUNPARAM") with
    | Some _ -> ()
    | None   -> Unix.putenv ~key:"OCAMLRUNPARAM" ~data:"b"
  end

(* available subcommands *)
let basic_targets = [
  ("clean"       , Clean.command);
  ("compile"     , Compile.command);
  ("doc"         , Doc.command);
  ("run"         , Run.command);
  ("test"        , Test.command);
  ("inspiration" , Inspiration.command);
]
let staff_targets = [
  ("cms"     , Cms.command);
  ("diff"    , Diff.command);
  ("email"   , Email.command);
  ("harness" , Harness.command);
  ("smoke"   , Smoke.command);
]

let main =
  Command.group
    ~summary:"cs3110 command line multitool"
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The highly recommended way to compile, test, and execute OCaml programs.";
      "Each subcommand handles one stage of the development process."
    ])
    (basic_targets @ staff_targets)
    (* to compile for stutdents, comment out staff_targets. *)

let _ =
  let () = config_env () in
  Command.run
    ~version:"2.0"
    ~build_info:"cs3110-cli by Ben C and Ben G. Built using Jane Street's Core library"
    main
