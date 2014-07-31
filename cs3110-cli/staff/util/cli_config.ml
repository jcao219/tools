(** Config: utilities for parsing and getting data from .cs3110 files *)
open Core.Std
open Cli_util

type cms_command_options = {
  column_names       : StringSet.t;
  comments_directory : string;
  delimiter          : char;
  output_spreadsheet : string;
}
type compile_command_options = {
  (* compiler_options    : string list; *) (* 2014-07-30: Not all compiler options seem to play nice with ocamlbuild. *)
  include_directories : StringSet.t;
  opam_packages       : StringSet.t;
  ocaml_libraries     : StringSet.t;
}
type diff_command_options = {
  nocompile_directory : string;
  output_spreadsheet  : string;
}
type doc_command_options = {
  output_directory : string;
}
type email_command_options = {
  admins          : StringSet.t;
  input_directory : string;
  subject         : string;
}
type harness_command_options = {
  input_directory         : string;
  output_directory        : string; (* For comments and postscript *)
  output_spreadsheet      : string;
  postscript              : bool;
  quickcheck_count        : int;
  temporary_failures_file : string;
  tests_directory         : string;
}
type smoke_command_options = {
  compilation_targets : StringSet.t;
  email_directory     : string;
  input_directory     : string;
  nocompile_directory : string;
}

type t = {
  cms     : cms_command_options;
  compile : compile_command_options;
  diff    : diff_command_options;
  doc     : doc_command_options;
  email   : email_command_options;
  harness : harness_command_options;
  smoke   : smoke_command_options;
}

let cLOCAL_CONFIG = "./.cs3110"
let cGLOBAL_CONFIG = "~/.cs3110"
let cOUTPUT_DIRECTORY = "./_cs3110"
let cBUILD_DIRECTORY = "./_build"
let cREQUIRED_OPAM_PACKAGES = [
  "pa_ounit.syntax";
  "oUnit";
  "pa_ounit";
  "qcheck"
]
let cPA_OUNIT_LOGFILE = "inline_tests.log"

let default_comments_directory  = Format.sprintf "%s/_harness/comments" cOUTPUT_DIRECTORY
let default_email_directory     = Format.sprintf "%s/_email"            cOUTPUT_DIRECTORY
let default_nocompile_directory = Format.sprintf "%s/_smoke/nocompile"  cOUTPUT_DIRECTORY
let default_config = {
  cms     = {
    column_names       = StringSet.empty;
    delimiter          = ',';
    comments_directory = default_comments_directory;
    output_spreadsheet = Format.sprintf "%s/_cms/CS3110_GRADES_TABLE.csv" cOUTPUT_DIRECTORY;
  };
  compile = {
    include_directories = StringSet.empty;
    opam_packages       = StringSet.of_list cREQUIRED_OPAM_PACKAGES;
    ocaml_libraries     = StringSet.empty;
  };
  diff    = {
    nocompile_directory = default_nocompile_directory;
    output_spreadsheet  = Format.sprintf "%s/_diff/diff_results.csv" cOUTPUT_DIRECTORY;
  };
  doc     = {
    output_directory = Format.sprintf "%s/_doc" cOUTPUT_DIRECTORY;
  };
  email   = {
    admins          = StringSet.empty;
    input_directory = default_email_directory;
    subject         = "[CS 3110 test harness] compile error";
  };
  harness = {
    input_directory         = "_release";
    output_directory        = default_comments_directory;
    output_spreadsheet      = Format.sprintf "%s/_harness/test-results.csv" cOUTPUT_DIRECTORY;
    postscript              = false;
    quickcheck_count        = 100;
    temporary_failures_file = "inline_test_failures.log";
    tests_directory         = "_tests";
  };
  smoke   = {
    compilation_targets = StringSet.empty;
    email_directory     = default_email_directory;
    input_directory     = "_release";
    nocompile_directory = default_nocompile_directory;
  };
}

module Parser : sig
  (** [parse default fname] read the settings in [fname], create a config record
      from these settings using the config [default] as a fallback. *)
  val parse : string -> default:t -> t
end = struct

  module StringMap = Map.Make(String)
  type raw_config = string StringMap.t

  (** [strip2 (a,b)] remove leading and trailing whitespace from strings [a] and [b]. *)
  let strip2 ((a,b) : string * string) : string * string =
    String.strip a, String.strip b

  (** [valid_line l] check whether the config file line [l] is properly formatted. *)
  let valid_line (line : string) : bool =
    (* valid lines look like 'k = v'. i.e. they contain a single '=' sign. *)
    1 = String.count line ~f:(fun c -> c = '=')

  (** [parse_line f l] augment the map [f] with the setting in line [l]. Return [f]
      unchanged if [l] is not properly formatted. *)
  let parse_line (acc : raw_config) (line : string) : raw_config =
    if valid_line line then
      let k,v = strip2 (String.lsplit2_exn line ~on:'=') in
      StringMap.add acc ~key:k ~data:v
    else
      acc

  (** [parse_whole_file f] read the settings in the file [f], return
      a map from labels to raw values. *)
  let parse_whole_file (fname : string) : raw_config =
    In_channel.with_file fname
      ~f:(fun t -> In_channel.fold_lines t
                     ~f:parse_line
                     ~init:StringMap.empty)

  (** [parse_bool map key] try to read a boolean from the raw value associated with [key] in [map]. *)
  let parse_bool (map : raw_config) ~key : bool option =
    begin match StringMap.find map key with
      | Some "true"   -> Some true
      | Some "false"  -> Some false
      | Some _ | None -> None
    end

  (** [parse_char map key] try to read a single character from the raw value associated with [key] in [map]. *)
  let parse_char (map : raw_config) ~key : char option =
    begin match StringMap.find map key with
      | Some s when String.length s = 1 -> Some s.[0]
      | Some _ | None                   -> None
    end

  (** [parse_string map key] get the raw value associated with the key [key] in the parsed
      config file [map]. *)
  let parse_string (map : raw_config) ~key : string option =
    StringMap.find map key

  (** [infer_sep s] Assumes [s] is a something-separated list of values. Tries to figure out
      the something. Examples of [s] might be 'itemA, itemB, itemC' or 'itemA; itemB' or 'itemA-itemB-itemC'.
      The inferred separators for these would be ',', ';', and '-', respectively. *)
  let infer_sep (s : string) : char =
    begin match String.find s ~f:(fun c -> not (Char.is_alphanum c)) with
      | Some c -> c
      | None   -> ' ' (* Hopefully it's a singleton list *)
    end

  (** [parse_string_list map key] interpret the raw value for [key] in raw config [map]
      as a list of strings. *)
  let parse_string_list (map : raw_config) ~key : string list option =
    begin match StringMap.find map key with
      | Some s -> Some (List.map (String.split s ~on:(infer_sep s)) ~f:String.strip)
      | None   -> None
    end

  (** [parse_string_set map key] interpret the raw value for [key] in raw config [map]
      as a set of strings. *)
  let parse_string_set (map : raw_config) ~key : StringSet.t option =
    begin match parse_string_list map ~key:key with
      | Some s -> Some (StringSet.of_list s)
      | None   -> None
    end

  (** [parse_filename map key suffix] Check that the raw value for [key] ends with [suffix]. *)
  let parse_filename (map : raw_config) ~key ~suffix : string option =
    begin match StringMap.find map key with
      | Some s when (String.is_suffix s ~suffix:suffix) -> Some s
      | Some _ | None                                   -> None
    end

  (** [parse_int map key] try to read an integer from the raw value associated with [key] in [map]. *)
  let parse_int (map : raw_config) ~key : int option =
    begin match StringMap.find map key with
      | Some s -> (try Some (int_of_string s) with _ -> None)
      | None   -> None
    end

  let parse (fname : string) ~default : t =
    begin match (Sys.file_exists fname) with
      | `No | `Unknown ->
        default
      | `Yes           ->
        let m = parse_whole_file fname in
        {
          cms     = {
            column_names        = Option.value (parse_string_set m ~key:"cms.column_names")
                                               ~default:default.cms.column_names;
            comments_directory  = Option.value (parse_string m ~key:"cms.comments_directory")
                                               ~default:default.cms.comments_directory;
            delimiter           = Option.value (parse_char m ~key:"cms.delimiter")
                                               ~default:default.cms.delimiter;
            output_spreadsheet  = Option.value (parse_filename m ~key:"cms.output_spreadsheet" ~suffix:".csv")
                                               ~default:default.cms.output_spreadsheet;
          };
          compile = {
             include_directories = Option.value (parse_string_set m ~key:"compile.include_directories")
                                                ~default:default.compile.include_directories;
             opam_packages       = List.fold_left cREQUIRED_OPAM_PACKAGES
                                     ~f:StringSet.add
                                     ~init:(Option.value (parse_string_set m ~key:"compile.opam_packages")
                                                         ~default:default.compile.opam_packages);
             ocaml_libraries     = Option.value (parse_string_set m ~key:"compile.ocaml_libraries")
                                                ~default:default.compile.ocaml_libraries;
          };
          doc     = {
            output_directory = Option.value (parse_string m ~key:"doc.output_directory")
                                            ~default:default.doc.output_directory;
          };
          diff    = {
            nocompile_directory = Option.value (parse_string m ~key:"diff.nocompile_directory")
                                               ~default:default.diff.nocompile_directory;
            output_spreadsheet  = Option.value (parse_filename m ~key:"diff.output_spreadsheet" ~suffix:".csv")
                                               ~default:default.diff.output_spreadsheet;
          };
          email   = {
             admins           = Option.value (parse_string_set m ~key:"email.admins")
                                             ~default:default.email.admins;
             input_directory = Option.value (parse_string m ~key:"email.input_directory")
                                             ~default:default.email.input_directory;
             subject          = Option.value (parse_string m ~key:"email.subject")
                                             ~default:default.email.subject;
           };
           harness = {
             input_directory           = Option.value (parse_string m ~key:"harness.input_directory")
                                                      ~default:default.harness.input_directory;
             output_directory          = Option.value (parse_string m ~key:"harness.output_directory")
                                                      ~default:default.harness.output_directory;
             output_spreadsheet        = Option.value (parse_filename m ~key:"harness.output_spreadsheet" ~suffix:".csv")
                                                      ~default:default.harness.output_spreadsheet;
             postscript                = Option.value (parse_bool m ~key:"harness.postscript")
                                                      ~default:default.harness.postscript;
             quickcheck_count          = Option.value (parse_int m ~key:"harness.quickcheck_count")
                                                      ~default:default.harness.quickcheck_count;
             temporary_failures_file   = Option.value (parse_filename m ~key:"harness.temporary_failures_file" ~suffix:".log")
                                                      ~default:default.harness.temporary_failures_file;
             tests_directory           = Option.value (parse_string m ~key:"harness.tests_directory")
                                                      ~default:default.harness.tests_directory;
           };
           smoke   = {
             compilation_targets = Option.value (parse_string_set m ~key:"smoke.compilation_targets")
                                                ~default:default.smoke.compilation_targets;
             email_directory     = Option.value (parse_string m ~key:"smoke.email_directory")
                                                ~default:default.smoke.email_directory;
             input_directory     = Option.value (parse_string m ~key:"smoke.input_directory")
                                                ~default:default.smoke.input_directory;
             nocompile_directory = Option.value (parse_string m ~key:"smoke.nocompile_directory")
                                                ~default:default.smoke.nocompile_directory;
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
