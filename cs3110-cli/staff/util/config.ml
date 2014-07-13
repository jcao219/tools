open Core.Std

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

let cLOCAL_CONFIG = "./.cs3110"
let cGLOBAL_CONFIG = "~/.cs3110"

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

let cPA_OUNIT_LOGFILE = "inline_tests.log"

let default_config = {
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
    temporary_file     = Format.sprintf "%s/diff.log" cDEFAULT_DIFF_DIRECTORY;
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
    temporary_results_file    = cPA_OUNIT_LOGFILE;
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

module Parser : sig
  (** [parse default fname] read the settings in [fname], create a config record
      from these settings using the config [default] as a fallback. *)
  val parse : string -> default:t -> t
end = struct

  type raw_config = string -> string option

  (** [strip2 (a,b)] remove leading and trailing whitespace from strings [a] and [b]. *)
  let strip2 ((a,b) : string * string) : string * string =
    String.strip a, String.strip b

  (** [valid_line l] check whether the config file line [l] is properly formatted. *)
  let valid_line (line : string) : bool =
    (* valid lines look like 'k = v'. i.e. they contain a single '=' sign. *)
    1 = String.count line ~f:(fun c -> c = '=')

  (** [parse_line f l] augment the map [f] with the setting in line [l]. Return [f]
      unchanged if [l] is not properly formatted.
      TODO verbose/doctor mode: print warning if line is bad. *)
  let parse_line (acc : raw_config) (line : string) : raw_config =
    if valid_line line then
      let k,v = strip2 (String.lsplit2_exn line ~on:'=') in
      (fun k' -> if k = k' then Some v else acc k')
    else
      acc

  (** [parse_whole_file f] read the settings in the file [f], return
      a function from labels to raw values. *)
  let parse_whole_file (fname : string) : raw_config =
    In_channel.with_file fname ~f:(fun t -> In_channel.fold_lines t ~init:(fun _ -> None) ~f:parse_line)

  (** [parse_string m key] get the raw value associated with the key [key] in the parsed
      config file [m]. *)
  let parse_string (m : raw_config) ~key : string option =
    m key

  (** [infer_sep s] Assumes [s] is a something-separated list of values. Tries to figure out
      the something. Examples of [s] might be 'itemA, itemB, itemC' or 'itemA; itemB' or 'itemA-itemB-itemC'.
      The inferred separators for these would be ',', ';', and '-', respectively. *)
  let infer_sep (s : string) : char =
    begin match String.find s ~f:(fun c -> not (Char.is_alphanum c)) with
      | Some c -> c
      | None   -> ' ' (* Hopefully it's a singleton list *)
    end

  (** [parse_string_list m key] interpret the raw value for [key] in raw config [m]
      as a list of strings. *)
  let parse_string_list (m : raw_config) ~key : string list option =
    begin match m key with
      | Some s -> Some (List.map (String.split s ~on:(infer_sep s)) ~f:String.strip)
      | None   -> None
    end

  (** [parse_filename m key suffix] Check that the raw value for [key] ends with [suffix]. *)
  let parse_filename (m : raw_config) ~key ~suffix : string option =
    begin match m key with
      | Some s when (String.is_suffix s ~suffix:suffix) -> Some s
      | Some _ | None                                   -> None
    end

  (** [parse_int m key] try to read an integer from the raw value associated with [key] in [m]. *)
  let parse_int (m : raw_config) ~key : int option =
    begin match m key with
      | Some s -> (try Some (int_of_string s) with Failure _ -> None)
      | None   -> None
    end

  (** [(%>) maybe default] alternate syntax for the builtin [Option.default]. Get the value out
      of the option [maybe], if anything, else return [default]. *)
  let (%>) (maybe : 'a option) default : 'a =
    begin match maybe with
      | Some m -> m
      | None   -> default
    end

  let parse (fname : string) ~default : t =
    begin match (Sys.file_exists fname) with
      | `No
      | `Unknown ->
         default
      | `Yes ->
         let m = parse_whole_file fname in
         {
           cms     = {
             output_directory    = ((parse_string m ~key:"cms.output_directory") %> default.cms.output_directory);
             output_spreadsheet  = (parse_filename m ~key:"cms.output_spreadsheet" ~suffix:".csv" %> default.cms.output_spreadsheet)
           };
           compile = {
             compiler_options    = (parse_string_list m ~key:"compile.compiler_options" %> default.compile.compiler_options);
             include_directories = (parse_string_list m ~key:"compile.include_directories" %> default.compile.include_directories);
             (* TODO this will append multiple times. Does that cause problems? Anyway, should append in [Compile] *)
             opam_packages       = (cREQUIRED_OPAM_PACKAGES @ (parse_string_list m ~key:"compile.opam_packages" %> default.compile.opam_packages));
             ocaml_libraries     = (parse_string_list m ~key:"compile.opam_packages" %> default.compile.ocaml_libraries);
             output_directory    = (parse_string m ~key:"compile.output_directory" %> default.compile.output_directory);
           };
           diff    = {
             temporary_file     = (parse_filename m ~key:"diff.temporary_file" ~suffix:".log" %> default.diff.temporary_file);
             output_directory   = (parse_string m ~key:"diff.output_directory" %> default.diff.output_directory);
             output_spreadsheet = (parse_filename m ~key:"diff.output_spreadsheet" ~suffix:".csv" %> default.diff.output_spreadsheet);
           };
           email   = {
             admins           = (parse_string_list m ~key:"email.admins" %> default.email.admins);
             subject          = (parse_string m ~key:"email.subject" %> default.email.subject);
             output_directory = (parse_string m ~key:"email.output_directory" %> default.email.output_directory);
           };
           harness = {
             quickcheck_count          = (parse_int m ~key:"harness.quickcheck_count" %> default.harness.quickcheck_count);
             output_directory          = (parse_string m ~key:"harness.output_directory" %> default.harness.output_directory);
             output_comments_directory = (parse_string m ~key:"harness.output_comments_directory" %> default.harness.output_comments_directory);
             output_spreadsheet        = (parse_filename m ~key:"harness.output_spreadsheet" ~suffix:".csv" %> default.harness.output_spreadsheet);
             temporary_failures_file   = (parse_filename m ~key:"harness.temporary_failures_file" ~suffix:".log" %> default.harness.temporary_failures_file);
             temporary_results_file    = cPA_OUNIT_LOGFILE; (* temporary_results_file is not overridable *)
             tests_directory           = (parse_string m ~key:"harness.tests_directory" %> default.harness.tests_directory);
             tests_to_run              = (parse_string_list m ~key:"harness.tests_to_run" %> default.harness.tests_to_run);
           };
           smoke   = {
             (* TODO rename to [output_directory] *)
             nocompile_directory = (parse_string m ~key:"smoke.nocompile_directory" %> default.smoke.nocompile_directory);
             compilation_targets = (parse_string_list m ~key:"smoke.compilation_targets" %> default.smoke.compilation_targets);
           };
         }
    end
end

let add_local_config (cfg : t) : t =
  Parser.parse cLOCAL_CONFIG ~default:cfg

let add_global_config (cfg : t) : t =
  Parser.parse cGLOBAL_CONFIG  ~default:cfg

let init () : t =
  add_local_config (add_global_config default_config)

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
