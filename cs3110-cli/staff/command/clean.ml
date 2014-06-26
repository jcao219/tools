open Cli_constants
open Core.Std
open Process_util

let build () : unit =
  check_code (Sys.command "ocamlbuild -clean")

let cms () : unit =
  check_code (Sys.command (Format.sprintf "rm -f %s" cCMS_DIR))

let diff () : unit =
  check_code (Sys.command (Format.sprintf "rm -f %s" cDIFF_RESULTS))

let email () : unit =
  check_code (Sys.command (Format.sprintf "rm -rf %s" cEMAIL_DIR))

let harness () : unit =
  let _ = check_code (Sys.command (Format.sprintf "rm -rf %s" cCMS_DIR)) in
  check_code (Sys.command (Format.sprintf "rm -rf %s" cHARNESS_DIR))

let smoke () : unit =
  let () = check_code (Sys.command (Format.sprintf "rm -rf %s" cEMAIL_DIR)) in
  check_code (Sys.command (Format.sprintf "rm -rf %s" cNOCOMPILE_DIR))

let show_targets () = List.iter [
  "Targets are: ";
  "  build   -- removes the [_build] directory generated by cs3110 compile";
  "  cms     -- removes the [_cms] direcory, generated by cs3110 harness";
  "  diff    -- removes the diff results from resubmissions";
  "  email   -- removes the automatically generated emails";
  "  harness -- removes the [_cms] and [_output] directories";
  "  smoke   -- removes [_email] and [_nocompile] directories";
  ]
  ~f:print_endline

(** [clean ()] removes all files generated during compilation. *)
let rec clean targets () : unit = match targets with
  | []           -> ()
  | target::rest -> begin
    let () = match target with
      | "build"   -> build   ()
      | "cms"     -> cms     ()
      | "diff"    -> diff    ()
      | "email"   -> email   ()
      | "harness" -> harness ()
      | "smoke"   -> smoke   ()
      | _         -> begin
        (** invalid option *)
        Printf.printf "Invalid option: %s" target;
        show_targets ();
        exit 1
      end in
    clean rest ()
  end

let clean_command =
  Command.basic
    ~summary:"Removes all of the files generated during compilation."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The clean command removes all of the automatically generated files and";
      "directories of the cs3110 tool. You can pass a sequence of targets for";
      "cleaning. Targets are:";
      "  build   -- removes the [_build] directory generated by cs3110 compile";
      "  cms     -- removes the [_cms] direcory, generated by cs3110 harness";
      "  diff    -- removes the diff results from resubmissions";
      "  email   -- removes the automatically generated emails";
      "  harness -- removes the [_cms] and [_output] directories";
      "  smoke   -- removes [_email] and [_nocompile] directories";
    ])
    Command.Spec.(empty
    +> anon (maybe_with_default ["build";"cms";"diff";"email";"harness";"smoke"]
                                (sequence ("clean-targets" %: string))))
    clean

let run_clean_command () =
  Command.run ~version:"2.0" ~build_info:"Core" clean_command
