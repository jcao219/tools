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

(** [compile_cmd] clean files generated by [cs3110 compile] *)
let compile_cmd : string = "ocamlbuild -clean"

(** [cms_cmd] clean files generated by [cs3110 cms] *)
let cms_cmd : string     = Format.sprintf "rm -f %s" cCMS_DIR

(** [diff_cmd] clean files generated by [cs3110 diff] *)
let diff_cmd : string    = Format.sprintf "rm -f %s" cDIFF_RESULTS

(** [email_cmd] clean generated emails *)
let email_cmd : string   = Format.sprintf "rm -rf %s" cEMAIL_DIR

(** [harness_cmd] clean files generated by [cs3110 harness] *)
let harness_cmd : string = Format.sprintf "%s; rm -rf %s" cms_cmd cHARNESS_DIR

(** [smoke_cmd] clean generated emails and no-compiles, i.e. files saved
    by [cs3110 smoke] *)
let smoke_cmd : string   = Format.sprintf "%s; rm -rf %s" email_cmd cNOCOMPILE_DIR

(** [command_not_found t] Print a 'not found' message for [t], return a system
    command that will give a nonzero exit status when run. *)
let command_not_found (target : string) : string =
  let () = Printf.printf "[clean] Invalid option: '%s'.\n%s\n" target targets_readme in
  "false"

(** [clean_dir_command d] if [d] is a directory, visit [d], clean the build/compile, leave.
    Otherwise print an error message. *)
let clean_dir_cmd (dir : string) : string =
  begin match Sys.is_directory dir with
    | `Yes           ->
      Format.sprintf
        "cd %s; %s; cd %s"
        dir
        compile_cmd
        (Sys.getcwd ())
    | `No | `Unknown ->
      command_not_found dir
  end

(** [clean v? d? t] Clean the files/folders generated by the target [t]. If [t]
    is a directory, clean the compiled files inside it. When [v] is high, print
    debugging information. When [d] is set, change into that directory before
    cleaning. The option [d] is more for internal calls than command-line use. *)
let clean ?(verbose=false) ?dir (target : string) : unit =
  let cwd = Sys.getcwd () in
  let ()  = Sys.chdir (Option.value ~default:cwd dir) in
  let ()  = if verbose then Format.printf "%![clean] Cleaning target '%s'.\n%!" target in
  let cmd =
    begin match target with
      | "all"     -> compile_cmd
      | "compile" -> compile_cmd
      | "cms"     -> cms_cmd
      | "diff"    -> diff_cmd
      | "email"   -> email_cmd
      | "harness" -> harness_cmd
      | "smoke"   -> smoke_cmd
      | _         -> clean_dir_cmd target
    end
  in
  let () = check_code (Sys.command cmd) in
  (* 2014-07-18: Need to flush and print one character after running ocamlbuild... *)
  let () = if verbose then Format.printf "\n%!" in
  let () = Sys.chdir cwd in
  ()

let command =
  Command.basic
    ~summary:"Removes all of the files generated during compilation."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The clean command removes all of the automatically generated files and";
      "directories of the cs3110 tool. You can pass a sequence of targets for";
      "cleaning (i.e., commands to clean up after), or directories to visit";
      targets_readme
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose" no_arg ~doc:" Print debugging information."
      +> anon (maybe_with_default ["compile"] (sequence ("<target>" %: string)))
    )
    (fun v targets () ->
      List.iter
        ~f:(clean ~verbose:v)
        targets
    )
