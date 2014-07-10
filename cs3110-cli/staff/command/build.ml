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

(** [build] compile [m] into a bytecode executable.
 * Relies on ocamlbuild. TODO quiet version? *)
let run (main_module : string) : int =
  let () =
    assert_ocamlbuild_friendly_filepath main_module;
    assert_file_exists (main_module ^ ".ml");
    Format.printf "Compiling '%s.ml'\n%!" main_module
  in
  let opam_packages_str =
      (String.concat ", "
      (List.map (fun p -> Format.sprintf "package(%s)" p) (cfg.compile.opam_packages)))
  in
  let target = Format.sprintf "%s.d.byte" main_module in
  (* TODO: add in [cfg.compile.compiler_options] *)
  run_process "ocamlbuild" (cfg.compile.include_drectories) @ (cfg.compile.ocaml_libraries) @ [
    (* "-cflag"; "-w"; "-cflag"; "A-4-33-40-41-42-43-34-44"; (* Jane street's warnings as errors *) *)
    "-cflag"; "-warn-error"; "-cflag"; "+a"; (* treat the default warnings as errors *)
    "-use-ocamlfind"; "-no-links"; "-build-dir"; cfg.compile.output_directory;
    "-tag-line"; "<**/*.ml{,i}> : thread";
    "-tag-line"; "<**/*.ml{,i}> : syntax(bin_prot), syntax(camlp4o), " ^ opam_packages_str;
    "-tag-line"; "<**/*.d.byte> : thread, " ^ opam_packages_str;
    "-tag-line"; "<**/*.native> : thread, " ^ opam_packages_str;
    target
  ])
