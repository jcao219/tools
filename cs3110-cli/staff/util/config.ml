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
type diff_command_options = {
  temporary_file     : string; (* necessary? *)
  output_directory   : string;
  output_spreadsheet : string
}
type email_command_options = {
  email_admins     : string list;
  email_subject    : string;
  output_directory : string
}
type harness_command_options = {
  quickcheck_count        : int;
  temporary_failures_file : string
  temporary_results_file  : string;
  tests_directory         : string;
  tests_to_run            : string list;
}
type smoke_command_options = {
  nocompile_directory : string;
  compilation_targets : string list;
}
type test_command_options = {}
type run_command_options = {}

type t = {
  clean   : clean_command_options;
  cms     : cms_command_options;
  compile : compile_command_options;
  email   : email_command_options;
  diff    : diff_command_options;
  harness : harness_command_options;
  smoke   : smoke_command_options;
  test    : test_command_options;
  run     : run_command_options;
}

let cOUTPUT_DIRECTORY = "./_cs3110"
let cDEFAULT_CMS_DIRECTORY = Format.sprintf "%s/_cms" cOUTPUT_DIRECTORY
let cDEFAULT_COMPILE_DIRECTORY = "_build"
let cDEFAULT_EMAIL_DIRECTORY = Format.sprintf "%s/_email" cOUTPUT_DIRECTORY
let cDEFAULT_DIFF_DIRECTORY = Format.sprintf "%s/_diff" cOUTPUT_DIRECTORY
let cDEFAULT_HARNESS_DIRECTORY = Format.sprintf "%s/_harness" cOUTPUT_DIRECTORY
let cDEFAULT_SMOKE_DIRECTORY = Format.sprintf "%s/_smoke" cOUTPUT_DIRECTORY

let cDEFAULT_COMPILER_FLAGS = [
  "-w";
  "+a"
]
let cREQUIRED_OPAM_PACKAGES = [
  "pa_ounit.syntax";
  "oUnit";
  "pa_ounit";
  "qcheck"
]

let default_options = {
  clean   = {};
  cms     = {
    output_directory   = cDEFAULT_CMS_DIRECTORY;
    input_spreadsheet  = Format.sprintf "%s/CS3110_GRADES_TABLE.csv" cDEFAULT_CMS_DIRECTORY;
    output_spreadsheet = Format.sprintf "%s/spreadsheet.csv" cDEFAULT_CMS_DIRECTORY;
  };
  compile = {
    compiler_options    = cDEFAULT_COMPILER_FLAGS;
    include_directories = [];
    opam_packages       = cREQUIRED_OPAM_PACKAGES;
    ocaml_libraries     = [];
    output_directory    = cDEFAULT_COMPILE_DIRECTORY
  };
  diff    = {
    temporary_file     = Format.sprintf "%s/diff.txt" cDEFAULT_DIFF_DIRECTORY;
    output_directory   = cDEFAULT_DIFF_DIRECTORY;
    output_spreadsheet = Format.sprintf "%s/diff_results.csv" cDEFAULT_DIFF_DIRECTORY
  };
  email   = {
    email_admins = [];  (* Fill in course instructors *)
    email_subject = "[CS 3110 test harness] compile error";
    output_directory = cDEFAULT_EMAIL_DIRECTORY;
  };
  harness = {
    quickcheck_count        = 100;
    temporary_failures_file = "inline_test_failures.log";
    temporary_results_file  = "inline_tests.log";
    tests_directory         = "./tests";
    tests_to_run            = []
  };
  smoke   = {
    nocompile_directory = Format.sprintf "%s/nocompile" cOUTPUT_DIRECTORY;
    compilation_targets = []
  };
  test    = {};
  run     = {};
}

let get_local_config () : string = failwith "nope"
let get_global_config () : string = failwith "nope"

let init () : t =
  default_options
