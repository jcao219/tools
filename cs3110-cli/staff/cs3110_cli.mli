exception File_not_found of string
exception Invalid_rubric of string
(* constants, required files and folders *)
val expected_version : string
val cms_dir : string
val cms_fname : string
val depend_file : string
val diff_results : string
val email_dir : string
val email_subject : string
val fail_output : string
val lib_file : string
val nocompile_dir : string
val output_dir : string
val reverse_cms : string
val reverse_dir : string
val reverse_rubric : string
val rubric_file : string
val smoke_targets : string
val test_output : string
val tests_dir : string
(* postscript generation utilities *)
val ps_normal_font : string
val ps_header_font : string
val ps_code_font : string
val ps_set_font : out_channel -> string -> unit
val ps_open_channel : string -> string -> out_channel
(* i/o utilities *)
val read_lines : in_channel -> string list
val csv_of_file : string -> string
(* string/filepath utilities *)
val absolute_path : string -> string
val split : string -> char -> bool -> string * string
val lsplit : string -> char -> string * string
val rsplit : string -> char -> string * string
val strip_suffix : string -> string
val strip_trailing_slash : string -> string
val strip_trailing_slash_all : string list -> string list
val tag_of_path : string -> string
val assert_file_exists : string -> unit
val ensure_dir : string -> unit
val directories_of_list : string -> string list
val test_name_of_line : string -> string
(* process utilities *)
val return_code_of_exit_status : Unix.process_status -> int
val run_process : string -> string list -> int
val check_code : int -> unit
val config_env : unit -> unit
(* cs3110 commands *)
val clean : unit -> unit
val build : string -> int
val diff : string list -> unit
val email : unit -> unit
val test : string -> int
(* rubric creation utilities *)
val assert_valid_line : unit -> string -> unit
val assert_valid_rubric : unit -> unit
val create_rubric : string list -> string list -> unit
val dict_of_rubric_file : string -> (string, int) Hashtbl.t
val reverse_create_rubric : string -> string list -> unit
val reverse_dict : string -> (string, bool * int) Hashtbl.t
(* back to cs3110 commands *)
val harness : string -> string list -> unit
val run : string -> string list -> int
val reverse : string -> string -> string list -> unit
val smoke : string list -> unit
val help : unit -> unit
