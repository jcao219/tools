val assert_file_exists : ?msg:string -> string -> unit

val is_ml : string -> bool
val is_mli : string -> bool

val ensure_ml  : string -> string
val ensure_dir : string -> unit

val get_extension : string -> string option

val strip_suffix : string -> string

val filename_of_path : string -> string

val at_expand : string list -> string list

val files_exist : string list -> bool

val check_installed : string -> bool

val assert_installed : string -> unit

(** HMMM *)
val lsplit : string -> char -> string * string
val rsplit : string -> char -> string * string

val file_is_empty : string -> bool

val soft_copy : string -> string -> int

val filter_directory : f:(string->bool) -> string -> string list
