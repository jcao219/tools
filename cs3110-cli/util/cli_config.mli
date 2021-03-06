(** Structure for parsing and accessing [cs3110 cli] configuration options. *)

open Cli_util

type cms_command_options     = {
  column_names            : StringSet.t;  (** Names of the columns to extract from the input spreadsheet. Other columns are ignored. *)
  comments_directory      : string;       (** Directory from which to scrape students' test harness results. Saved as comments in the CMS spreadsheet. *)
  delimiter               : char;         (** Delimiter for the input spreadsheet. Must be a single character. Must be consistent throughout the file. *)
  output_spreadsheet      : string;       (** Filename of the final, CMS-ready spreadsheet. *)
}
type compile_command_options = {
  include_directories     : StringSet.t;  (** Sub-directories to search during compilation. *)
  opam_packages           : StringSet.t;  (** OPAM packages to include in compilation. *)
  ocaml_libraries         : StringSet.t;  (** Extra OCaml libraries to link during compilation. *)
  thread                  : bool;         (** Flag to load threading libraries. *)
}
type diff_command_options    = {
  nocompile_directory     : string;       (** Directory to scrape past submissions that failed to compile. *)
  output_spreadsheet      : string;       (** Spreadsheet to save results to. *)
}
type doc_command_options     = {
  output_directory        : string;       (** Directory to save generated documentation in. *)
}
type email_command_options   = {
  admins                  : StringSet.t;  (** Email addresses to include as bccs on every sent email. *)
  input_directory         : string;       (** Directory to scrape email messages from. *)
  subject                 : string;       (** Common subject on all emails sent out. *)
}
type harness_command_options = {
  input_directory         : string;       (** Directory of starter code. Also used as a staging area, to compile tests and extract unittest names. *)
  output_directory        : string;       (** Directory to save individual test results and (optionally) postscript output to. *)
  output_spreadsheet      : string;       (** Spreadsheet to save aggregate results to. *)
  postscript              : bool;         (** When set, generate postscript copies of each student's code and test results. *)
  temporary_failures_file : string;       (** Internal to harness. File to pipe [pa_ounit] results to. *)
  tests_directory         : string;       (** Directory to get test files from. *)
}
type smoke_command_options   = {
  compilation_targets     : StringSet.t;  (** Names of the files to compile. Other files in the directory are not directly compiled, but they may be linked. *)
  email_directory         : string;       (** Directory to save auto-generated email messages to. *)
  input_directory         : string;       (** Starter code / release code. Files are copied in to target directories before compiling. *)
  nocompile_directory     : string;       (** Directory in which to save submissions that fail to compile. *)
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

(** Root folder of all grading output. *)
val cOUTPUT_DIRECTORY : string
(** Folder generated during compilation. *)
val cBUILD_DIRECTORY : string
(** OPAM packages required for inline tests to compile. *)
val cREQUIRED_OPAM_PACKAGES : string list
(** Internal to harness: log file generated by [pa_ounit]. *)
val cPA_OUNIT_LOGFILE : string

(** [init ()] Parse config files [.cs3110] and [~/.cs3110]. The local file
    has precedence over the global config file stored in the home ([~/])
    directory. Unset options are filled by defaults set in [cli_config.ml].
    Note that command-line options take precedence over any local/global/default
    config. *)
val init : unit -> t
