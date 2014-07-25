type t

val close : t -> unit
val init : string -> string -> t
val write : t -> string -> unit
val write_code : t -> string -> unit
val write_results : t -> string -> unit
