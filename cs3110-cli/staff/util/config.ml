(** Config: utilities for parsing and getting data from .cs3110 files *)
(* type clean_command_options = {} *)
type cms_command_options = {
  output_directory   : string;
  output_spreadsheet : string
}
type compile_command_options = {
  compiler_options    : string list;
  include_directories : string list;
  opam_packages       : string list;
  ocaml_libraries     : string list;
  output_directory    : string       (* replaces _build *)
}
type diff_command_options = {
  temporary_file     : string; (* necessary? *)
  output_directory   : string;
  output_spreadsheet : string
}
type email_command_options = {
  admins           : string list;
  subject          : string;
  output_directory : string
}
type harness_command_options = {
  output_directory          : string;
  output_comments_directory : string;
  output_spreadsheet        : string;
  quickcheck_count          : int;
  temporary_failures_file   : string;
  temporary_results_file    : string;
  tests_directory           : string;
  tests_to_run              : string list;
}
type smoke_command_options = {
  nocompile_directory : string;
  compilation_targets : string list;
}
(* type test_command_options = {} *)
(* type run_command_options = {} *)

type t = {
  (* clean   : clean_command_options; *)
  cms     : cms_command_options;
  compile : compile_command_options;
  email   : email_command_options;
  diff    : diff_command_options;
  harness : harness_command_options;
  smoke   : smoke_command_options;
  (* test    : test_command_options; *)
  (* run     : run_command_options; *)
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
  (* clean   = {}; *)
  cms     = {
    output_directory   = cDEFAULT_CMS_DIRECTORY;
    output_spreadsheet  = Format.sprintf "%s/CS3110_GRADES_TABLE.csv" cDEFAULT_CMS_DIRECTORY;
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
    admins     = [];  (* Fill in course instructors *)
    subject    = "[CS 3110 test harness] compile error";
    output_directory = cDEFAULT_EMAIL_DIRECTORY;
  };
  harness = {
    quickcheck_count          = 100;
    output_directory          = cDEFAULT_HARNESS_DIRECTORY;
    output_comments_directory = Format.sprintf "%s/comments" cDEFAULT_HARNESS_DIRECTORY;
    output_spreadsheet        = Format.sprintf "%s/test-results.csv" cDEFAULT_HARNESS_DIRECTORY;
    temporary_failures_file   = "inline_test_failures.log";
    temporary_results_file    = "inline_tests.log";
    tests_directory           = "./tests";
    tests_to_run              = []
  };
  smoke   = {
    (* TODO rename to [output_directory] *)
    nocompile_directory = Format.sprintf "%s/nocompile" cOUTPUT_DIRECTORY;
    compilation_targets = []
  };
  (* test    = {}; *)
  (* run     = {}; *)
}

let get_local_config () : string = failwith "nope"
let get_global_config () : string = failwith "nope"

(* let get_dependencies () : string list =  *)
(*   if Sys.file_exists cDEPEND_FILE *)
(*   then ["-Is"; csv_of_file cDEPEND_FILE] *)
(*   else [] *)

(* let get_libraries () : string list =  *)
(*   if Sys.file_exists cLIB_FILE *)
(*   then ["-libs"; "assertions," ^ csv_of_file cLIB_FILE] *)
(*   else ["-libs"; "assertions"] *)

(* let get_opam_packages () : string list = cSTD_OPAM_PACKAGES @  *)
(*   if Sys.file_exists cOPAM_PACKAGES_FILE *)
(*   then (read_lines (open_in cOPAM_PACKAGES_FILE)) *)
(*   else [] *)

(* let smoke_targets =  *)
(*     if Sys.file_exists cfg.smoke.compilation_targets *)
(*     then List.map strip_suffix (read_lines (open_in cSMOKE_TARGETS)) *)
(*     else if Sys.file_exists cTESTS_DIR *)
(*     then Array.fold_right (fun f acc -> *)
(*       (fst(rsplit f '_')) :: acc) (Sys.readdir cTESTS_DIR) [] *)
(*     else raise (File_not_found cSMOKE_TARGETS) *)

let init () : t =
  default_options
