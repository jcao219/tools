val absolute_path : string -> string

val assert_file_exists : ?msg:string -> string -> unit

val directories_of_list : string -> string list

val do_if_directory : string -> (string -> 'a) -> string -> int -> 'a

val ensure_ml  : string -> string
val ensure_dir : string -> unit

val filter_by_extension : string -> string list -> string list

val get_extension : string -> string option

val get_files_with_extension : string -> string -> string list

val lsplit : string -> char -> string * string
val rsplit : string -> char -> string * string

val starts_with : string -> string -> bool

val is_suffix : string -> string -> bool

val strip_suffix : string -> string

val tag_of_path : string -> string

val unittest_name_of_line : string -> string

val filename_of_path : string -> string

val at_expand : string list -> string list

val all_files_exist : string list -> bool

val check_installed : string -> bool

val assert_installed : string -> unit

val file_is_empty : string -> bool

val is_valid_test_file : string -> bool

val soft_copy : string -> string -> int

val test_list_of_directory : ?verbose:bool -> string -> string list
