(** Config: utilities for parsing and getting data from .cs3110 files *)
type clean_command_options = {}
type compile_command_options = {
  compiler_options : string list
  include_directories : string list
  opam_packages : string list
  ocaml_libraries : string list
}
type email_command_options = {}
type diff_command_options = {}
type harness_command_options = {}
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

let init () : t =
  failwith "not implemented"
