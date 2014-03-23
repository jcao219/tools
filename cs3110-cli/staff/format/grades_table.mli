type t

val add_row : t -> string -> (string * int list) list -> t

val init : (string * string list) list -> t
val init_from_file : string -> t

val write : t -> string -> unit
