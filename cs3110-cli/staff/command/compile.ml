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
    with Not_found -> false
  in
  let is_absolute = String.contains path '~' || path.[0] = '/' in
  if (is_absolute || is_relative)
  then raise (Invalid_filepath "Must call cs3110 from the project root. Absolute or relative paths are not allowed.")

(* TODO change these constants *)
let get_dependencies () : string list =
  begin match Sys.file_exists cDEPEND_FILE with
    | `No  | `Unknown -> []
    | `Yes            -> ["-Is"; csv_of_file cDEPEND_FILE]
  end

let get_libraries () : string list =
  begin match Sys.file_exists cLIB_FILE with
    | `No  | `Unknown -> ["-libs"; "assertions"]
    | `Yes            -> ["-libs"; "assertions," ^ csv_of_file cLIB_FILE]
  end

let get_opam_packages () : string list = cSTD_OPAM_PACKAGES @
  begin match Sys.file_exists cOPAM_PACKAGES_FILE with
    | `No  | `Unknown -> []
    | `Yes            -> read_lines (open_in cOPAM_PACKAGES_FILE)
  end

(** [build] compile [m] into a bytecode executable. Relies on ocamlbuild. *)
let compile (run_quiet:bool) (main_module : string) : unit =
  let () =
    assert_ocamlbuild_friendly_filepath main_module;
    assert_file_exists (main_module ^ ".ml");
    Format.printf "Compiling '%s.ml'\n%!" main_module
  in
  let opam_packages_str =
    String.concat ~sep:", "
                  (List.map (get_opam_packages ())
                            ~f:(Format.sprintf "package(%s)"))
  in
  let target = Format.sprintf "%s.d.byte" main_module in
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
    (get_dependencies ()) @
    (get_libraries ())    @
    if run_quiet then "-quiet"::ocamlbuild_flags else ocamlbuild_flags))

let command =
  Command.basic
    ~summary:"Compiles into a bytecode executable. Relies on ocamlbuild."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The compile command is a wrapper for the ocamlbuild tool. It takes the";
      "input file, resolves dependencies, and compiles to a bytecode";
      "executable file. The object files produced during compilation are placed";
      "in a directory [_build], which will be created (if not already present)";
      "in the current working directory."
    ])
    Command.Spec.(
      empty
      +> flag "-q" no_arg ~doc:"Run quietly."
      +> anon ("filename" %: string))
    (fun quiet target () -> compile quiet target)
