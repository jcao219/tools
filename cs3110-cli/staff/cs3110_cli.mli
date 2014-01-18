exception File_not_found of string
(* constants *)
val expected_version : string
val email_subject : string
val depend_file : string
val lib_file : string
val tests_dir : string
val test_output : string
val fail_output : string
val ps_normal_font : string
val ps_header_font : string
val ps_code_font : string
(* postscript utils *)
val ps_set_font : out_channel -> string -> unit
(* string utils *)
val split : string -> char -> bool -> string * string
val lsplit : string -> char -> string * string
val rsplit : string -> char -> string * string
val strip_suffix : string -> string
val strip_trailing_slash : string -> string
val strip_trailing_slash_all : string list -> string list
val tag_of_path : string -> string
(* io utils *)
val assert_file_exists : string -> unit
val ensure_dir : string -> unit
val read_lines : in_channel -> string list
val csv_of_file : string -> string
val directories_of_list : string -> string list
(* process utils *)
val run_process : string -> string list -> int
val check_code : int -> unit
(* commands *)
val clean : unit -> unit
val build : string -> int
val diff : string list -> unit
val email : unit -> unit
val test : string -> int
val harness : string -> string list -> unit
val run : string -> string list -> int
val smoke : string list -> unit
val help : unit -> unit
val config_env : unit -> unit
