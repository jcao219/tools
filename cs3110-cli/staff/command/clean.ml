open Cli_constants
open Core.Std
open Process_util

let targets_readme = String.concat ~sep:"\n" [
  "Targets are: ";
  "  all      removes everything generated by the cs3110 tool";
  "  compile  removes the [_build] directory generated by cs3110 compile";
  "  cms      removes the [_cms] direcory, generated by cs3110 harness";
  "  diff     removes the diff results from resubmissions";
  "  email    removes the automatically generated emails";
  "  harness  removes the [_cms] and [_output] directories";
  "  smoke    removes [_email] and [_nocompile] directories";
  ]

let compile_cmd : string = "ocamlbuild -clean"

let cms_cmd : string = Format.sprintf "rm -f %s" cCMS_DIR

let diff_cmd : string = Format.sprintf "rm -f %s" cDIFF_RESULTS

let email_cmd : string = Format.sprintf "rm -rf %s" cEMAIL_DIR

let harness_cmd : string = Format.sprintf "%s; rm -rf %s" cms_cmd cHARNESS_DIR

let smoke_cmd : string = Format.sprintf "%s; rm -rf %s" email_cmd cNOCOMPILE_DIR

let command_not_found (target : string) : string =
  let () = Printf.printf "Invalid option: %s\n%s\n" target targets_readme in
  "false"

let clean (target : string) : unit =
  let cmd =
    begin match target with
      | "all"     -> compile_cmd
      | "compile" -> compile_cmd
      | "cms"     -> cms_cmd
      | "diff"    -> diff_cmd
      | "email"   -> email_cmd
      | "harness" -> harness_cmd
      | "smoke"   -> smoke_cmd
      | _         -> command_not_found target
    end
  in
  check_code (Sys.command cmd)

let command =
  Command.basic
    ~summary:"Removes all of the files generated during compilation."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The clean command removes all of the automatically generated files and";
      "directories of the cs3110 tool.";
      targets_readme
    ])
    Command.Spec.(
      empty +>
      anon (maybe_with_default "compile" ("<target>" %: string)))
    (fun target () -> clean target)
