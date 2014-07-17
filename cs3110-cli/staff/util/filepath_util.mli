val absolute_path : string -> string

val assert_file_exists : string -> unit

val directories_of_list : string -> string list

val do_if_directory : string -> (string -> 'a) -> string -> int -> 'a

val ensure_dir : string -> unit

val filter_by_extension : string -> string list -> string list

val get_extension : string -> string option

val get_files_with_extension : string -> string -> string list

val lsplit : string -> char -> string * string
val rsplit : string -> char -> string * string

val starts_with : string -> string -> bool

val is_suffix : string -> string -> bool

val strip_suffix : string -> string

val strip_trailing_slash : string -> string
val strip_trailing_slash_all : string list -> string list

val tag_of_path : string -> string

val test_name_of_line : string -> string
val is_valid_test_file : string -> bool
val at_expand : string list -> string list
