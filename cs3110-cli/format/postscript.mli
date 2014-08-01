(** Interface for building and saving postscript copies of students' code and test results.
    This module is not compatible with [Core.Std]. *)

(** Type representing a postscript document. *)
type t

(** [close ps] Saves and closes the document [ps]. *)
val close : t -> unit

(** [init fname title] Initialize a postscript document to be saved
    to the file [fname]. The string [title] is the title printed at
    the top of the new document. *)
val init : string -> string -> t

(** [write ps str] Write string [str] to the document [ps]. *)
val write : t -> string -> unit

(** [write_line ps str] Write string [str] and a trailing newline
    character to the document [ps]. *)
val write_line : t -> string -> unit

(** [write_code ps str] Write the string [str] to the document [ps]
    using the font and style reserved for code blocks. *)
val write_code : t -> string -> unit

(** [write_results ps str] Write the string [str] to the document [ps]
    using the font and style reserved for test harness results. *)
val write_results : t -> string -> unit
