type font = Normal | Header | Code
type t

val close : t -> unit
val init : string -> string -> t
val set_font : t -> font -> unit
val write : t -> string -> unit
