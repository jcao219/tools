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
  check_code (Sys.command (Format.sprintf "rm -rf %s" cOUTPUT_DIR))

let smoke () : unit =
  let () = check_code (Sys.command (Format.sprintf "rm -rf %s" cEMAIL_DIR)) in
  check_code (Sys.command (Format.sprintf "rm -rf %s" cNOCOMPILE_DIR))

(** [clean ()] removes all files generated during compilation. *)
let clean () : unit = build   ();
                      cms     ();
                      diff    ();
                      email   ();
                      harness ();
                      smoke   ()

let clean_command =
  Command.basic
    ~summary:"Removes all of the files generated during compilation."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The clean command removes all of the automatically generated files and";
      "directories of the cs3110 tool. These include:";
      "  1. The build directory from compilation.";
      "  2. The cms directory for spreadsheets.";
      "  3. The diff results for resubmissions.";
      "  4. The emails sent out to no-compiles.";
      "  5. The test harness results.";
      "  6. The smoke test results."
    ])
    Command.Spec.empty
    clean

let run_clean_command () =
  Command.run ~version:"2.0" ~build_info:"Core" clean_command
