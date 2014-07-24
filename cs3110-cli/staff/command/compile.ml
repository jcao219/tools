open Cli_constants
open Core.Std
open Io_util
open Filepath_util
open Process_util

(* Default compiler flags. These are Jane Street's settings.
   [-warn-error] says "treat the following warnings as errors".
   [+a] means "all warnings"
   [-4-33-34-39-40-41-42-43-44] means "except these"
   - [4]  : Fragile pattern matching (uses _)
   - [33] : Unused open statement
   - [34] : Unused type declaration
   - [39] : Unused rec flag
   - [40] : Constructor or label name used out of scope
   - [41] : Ambiguous constructor label or name
   - [42] : Disambiguated constructor label or name
   - [43] : Nonoptional label applied as optional
   - [44] : Open statement shadows an identifier
   Note that warnings 40-45 are for [ocamlopt] and not for [ocamlc].
   See more documentation here: http://caml.inria.fr/pub/docs/manual-ocaml/native.html
 *)
let default_compiler_flags = [
  "-warn-error";
  "+a-4-33-34-40-41-42-43-44";
]

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

(** [format_compiler_flags fs] prepend the string '-cflag' to each compiler
    flag for compliance with ocamlbuild *)
let format_compiler_flags (flags : string list) : string list =
  List.fold_right
    ~f:(fun f acc -> "-cflag" :: f :: acc)
    ~init:[]
    flags

let get_compiler_flags () : string list =
  (* TODO check init file *)
  let fs = default_compiler_flags in
  format_compiler_flags fs

(* TODO change these constants to use config file *)
(** [get_dependencies ()] read in config file,
    return list of folders to search recursively during compilation. *)
let get_dependencies () : string list =
  begin match Sys.file_exists cDEPEND_FILE with
    | `No  | `Unknown -> []
    | `Yes            -> ["-Is"; csv_of_file cDEPEND_FILE]
  end
(** [get_libraries ()] read config file,
    return list of OCaml libraries to include during compilation. *)
let get_libraries () : string list =
  begin match Sys.file_exists cLIB_FILE with
    | `No  | `Unknown -> ["-libs"; "assertions"]
    | `Yes            -> ["-libs"; "assertions," ^ csv_of_file cLIB_FILE]
  end

(** [get_opam_packages ()] read config file,
    return list of opam packages to include during compilation. *)
let get_opam_packages () : string list = cSTD_OPAM_PACKAGES @
  begin match Sys.file_exists cOPAM_PACKAGES_FILE with
    | `No  | `Unknown -> []
    | `Yes            -> read_lines (open_in cOPAM_PACKAGES_FILE)
  end

(** [compile q? v? d? main] compile [main] into a bytecode executable.
    Relies on ocamlbuild. When [q] is high, suppress compiler printouts.
    When [v] is high, print debugging information. When [d] is given,
    change into directory [d] before compiling [main]. *)
let compile ?(quiet=false) ?(verbose=false) ?dir (main_module : string) : int =
  let main      = ensure_ml main_module in
  let cwd       = Sys.getcwd () in
  let ()        = Sys.chdir (Option.value ~default:cwd dir) in
  let ()        = if verbose then Format.printf "[compile] Preparing to compile file '%s'\n" main in
  let target    = Format.sprintf "%s.d.byte" (strip_suffix main) in
  let ()        = if verbose then Format.printf "[compile] Target is '%s'\n" target in
  let deps      = get_dependencies () in
  let ()        = if verbose then Format.printf "[compile] Included directories are [%s]\n" (String.concat ~sep:"; " deps) in
  let libs      = get_libraries () in
  let ()        = if verbose then Format.printf "[compile] Linked libraries are [%s]\n"     (String.concat ~sep:"; " libs) in
  let cflags    = get_compiler_flags () in
  let ()        = if verbose then Format.printf "[compile] Compiler flags are [%s]\n"       (String.concat ~sep:"; " cflags) in
  let run_quiet = if quiet then ["-quiet"] else [] in
  let opkgs     = String.concat ~sep:", " (List.map ~f:(Format.sprintf "package(%s)")       (get_opam_packages ())) in
  let oflags    =  ["-use-ocamlfind";
                    "-no-links";
                    "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), " ^ opkgs;
                    "-tag-line"; "<*.d.byte> : "                  ^ opkgs;
                    "-tag-line"; "<*.native> : "                  ^ opkgs;
                    target] in
  let ()        = if verbose then Format.printf "[compile] ocamlbuild flags are [%s]\n"     (String.concat ~sep:"; " oflags) in
  let command   = deps @ libs @ cflags @ run_quiet @ oflags in
  (* 2014-07-23: Need to flush before ocamlbuild prints. *)
  let ()        = if verbose && (not quiet) then Format.printf "%!" in
  let exit_code = run_process "ocamlbuild" command in
  let ()        = Sys.chdir cwd in
  let ()        = if verbose && (exit_code = 0) then Format.printf "[compile] Compilation succeeded!\n" in
  exit_code

let command =
  Command.basic
    ~summary:"Compiles into a bytecode executable. Relies on ocamlbuild."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The compile command is a wrapper for the ocamlbuild tool. It takes the";
      "input file, resolves dependencies, and compiles to a bytecode";
      "executable file. The object files produced during compilation are placed";
      "in a directory [_build], which will be created (if not already present)";
      "in the current working directory.";
      "You may pass a file in the current directory or a file in a sub-directory.";
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-q"] "-quiet"   no_arg ~doc:" Compile quietly."
      +> flag ~aliases:["-v"] "-verbose" no_arg ~doc:" Print debugging information (about compiler options, etc.)."
      +> anon ("target" %: file)
    )
    (fun q v target () ->
      let () = assert_ocamlbuild_friendly_filepath target in
      check_code (compile ~quiet:q ~verbose:v target)
    )
