open Cli_constants

module P = Process_util

(** [clean ()] remove all files generated during compilation *)
let build () : unit =
  P.check_code (Sys.command "ocamlbuild -clean")

let cms () : unit =
  P.check_code (Sys.command (Format.sprintf "rm -f %s" cCMS_DIR))

let diff () : unit =
  P.check_code (Sys.command (Format.sprintf "rm -f %s" cDIFF_RESULTS))

let email () : unit =
  P.check_code (Sys.command (Format.sprintf "rm -rf %s" cEMAIL_DIR))

let harness () : unit =
  let _ = P.check_code (Sys.command (Format.sprintf "rm -rf %s" cCMS_DIR)) in
  P.check_code (Sys.command (Format.sprintf "rm -rf %s" cOUTPUT_DIR))

let smoke () : unit =
  let () = P.check_code (Sys.command (Format.sprintf "rm -rf %s" cEMAIL_DIR)) in
  P.check_code (Sys.command (Format.sprintf "rm -rf %s" cNOCOMPILE_DIR))

let all () : unit =
  let () = build () in
  let () = cms () in
  let () = diff () in
  let () = email () in
  let () = harness () in
  let () = smoke () in
  ()
