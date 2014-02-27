open Cli_constants
open Core.Std
open Io_util
open Filepath_util
open Process_util

(* 2014-04-18: "ocamlbuild must be invoked from the root of the project"
 * http://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html *)
let assert_ocamlbuild_friendly_filepath (path : string) : unit =
  let is_relative =
    try let _ = Str.search_forward (Str.regexp "\\.\\./") path 0 in true
    with Not_found -> false in
  if (String.contains path '~' || path.[0] = '/' || is_relative)
  then
    let err_msg = String.concat ~sep:" " [
                                  "Must call cs3110 from the project root.";
                                  "Absolute or relative paths are not allowed.";
                                ] in
    raise (Invalid_filepath  err_msg)

(** [build] compile [m] into a bytecode executable. Relies on ocamlbuild. *)
let build run_quiet (main_module : string) (() : unit) : unit =
  assert_ocamlbuild_friendly_filepath main_module;
  assert_file_exists (main_module ^ ".ml");
  let target = Format.sprintf "%s.d.byte" main_module in
  let () = Format.printf "Compiling '%s.ml'\n%!" main_module in
  let dependencies = match Sys.file_exists cDEPEND_FILE with
    | `No  | `Unknown -> []
    | `Yes            -> ["-Is"; csv_of_file cDEPEND_FILE] in
  let libraries = match Sys.file_exists cLIB_FILE with
    | `No  | `Unknown -> ["-libs"; "assertions"]
    | `Yes            -> ["-libs"; "assertions," ^ csv_of_file cLIB_FILE] in
  let all_opam_packages = cSTD_OPAM_PACKAGES @
    match Sys.file_exists cOPAM_PACKAGES_FILE with
    | `No  | `Unknown -> []
    | `Yes            -> read_lines (open_in cOPAM_PACKAGES_FILE) in
  let opam_packages_str =
    String.concat ~sep:", "
                  (List.map all_opam_packages
                            ~f:(Format.sprintf "package(%s)")) in
  let ocamlbuild_flags =  [
    "-cflag";
    "-warn-error";
    "-cflag";
    "+a";                       (* treat the default warnings as errors *)
    "A-4-33-40-41-42-43-34-44"; (* Jane street's warnings as errors *)
    "-use-ocamlfind";
    "-no-links";
    "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), " ^ opam_packages_str;
    "-tag-line"; "<*.d.byte> : " ^ opam_packages_str;
    "-tag-line"; "<*.native> : " ^ opam_packages_str;
    target
  ] in
  check_code (run_process "ocamlbuild" (
    dependencies @
    libraries    @
    if run_quiet then "-quiet"::ocamlbuild_flags else ocamlbuild_flags))

let build_command =
  Command.basic
    ~summary:"Compiles into a bytecode executable. Relies on ocamlbuild."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The build command is a wrapper for the ocamlbuild tool. It takes the";
      "input file, resolves it's dependencies and compiles to a bytecode";
      "executable file. The object files produced during compilation are placed";
      "in a directory [_build], which will be created, if not already present,";
      "in the current working directory."
    ])
    Command.Spec.(
      empty
      +> flag "-q" no_arg ~doc:"Run quietly."
      +> anon ("filename" %: file))
    build

let run_build () =
  Command.run ~version:"2.0" ~build_info:"Core" build_command
