val expected_version : string
val depend_file : string
val lib_file : string
exception File_not_found of string
val strip_suffix : string -> string
val assert_file_exists : string -> unit
val read_lines : in_channel -> string list
val csv_of_file : string -> string
val run_process : string -> string list -> int
val check_code : int -> unit
val clean : unit -> unit
val build : string -> unit
val test : string -> unit
val run : string -> string list -> unit
val help : unit -> unit
val config_env : unit -> unit
