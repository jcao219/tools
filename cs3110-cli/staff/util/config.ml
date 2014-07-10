(** Config: utilities for parsing and getting data from .cs3110 files *)
type clean_command_options = {
}
type cms_command_options = {
  directory          : string;
  output_directory   : string;
  input_spreadsheet  : string;
  output_spreadsheet : string
}
type compile_command_options = {
  compiler_options    : string list;
  include_directories : string list;
  opam_packages       : string list;
  ocaml_libraries     : string list
  output_directory    : string       (* replaces _build *)
}
type email_command_options = {
  email_admins     : string list;
  email_subject    : string;
  output_directory : string
}
type diff_command_options = {
  temporary_file     : string; (* necessary? *)
  output_directory   : string;
  output_spreadsheet : string
}
type harness_command_options = {
  quickcheck_count        : int;
  tests_directory         : string;
  tests_to_run            : string list; (* [tests_to_run] should all be files within [tests_directory] *)
  temporary_results_file  : string;
  temporary_failures_file : string
}
type smoke_command_options = {
  nocompile_directory : string;
  compilation_targets : string list;
}
type test_command_options = {}
type run_command_options = {}

type t = {
  clean   : clean_command_options;
  compile : compile_command_options;
  email   : email_command_options;
  diff    : diff_command_options;
  harness : harness_command_options;
  test    : test_command_options;
  run     : run_command_options;
}

let cOUTPUT_DIR = "./_cs3110"
let cREQUIRED_OPAM_PACKAGES = [
  "pa_ounit.syntax";
  "oUnit";
  "pa_ounit";
  "qcheck"
]

let init () : t =
  failwith "not implemented"
