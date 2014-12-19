(** Utility functions for the cs3110 command line tool.
    These are mostly string/filepath manipulation. *)

exception Command_not_found of string
exception File_not_found of string
exception Invalid_filepath of string

module StringSet : Core.Std.Set.S with type Elt.t = string

(** [assert_file_exists fname] raises [File_not_found] if [f] does not exist.
    Optional argument specifies the error message. *)
val assert_file_exists : ?msg:string -> string -> unit

(** [assert_installed cmd] raises [Command_not_found] if the unix tool [cmd]
    is not installed. *)
val assert_installed : string -> unit

(** [at_expand dirs] optionally expand a file containing a list into a list of directories.
    If the input is a singleton list where the first element is prefixed by and '@' character,
    treat this input as a file containing a list of newline-separated strings. Create directory
    names from these strings.
    Else return the input unchanged. *)
val at_expand : string list -> string list

(** [check_installed cmd] False is the unix tool [cmd] is not found. *)
val check_installed : string -> bool

(** [ensure_dir d] creates the directory [d] if it does not exist already. *)
val ensure_dir : string -> unit

(** [ensure_ml f] Check if [f] has a '.ml' suffix. If not, append one. *)
val ensure_ml  : string -> string

(** [file_is_empty fname] True if [fname] contains nothing. *)
val file_is_empty : string -> bool

(** [filename_of_path p] Return the last item along the path [p].
    It could be a filename or a directory name, don't care.
    Given 'dir1/dir2/dir3/', this function returns 'dir3'.
    Given 'file.ml', this function is the identity. *)
val filename_of_path : string -> string

(** [files_exist fs] True if all files in the list [fs] exist.
    False is any one file is a 'no' or 'unknown'. *)
val files_exist : string list -> bool

(** [filter_directory ~p d] Read files in directory [d], remove files
    for which predicate [p] is false. *)
val filter_directory : f:(string->bool) -> string -> string list

(** [get_extension file_name] gets the extension of the file
    [file_name], i.e. the characters occuring to the right
    of the right-most occurence of the '.' character. *)
val get_extension : string -> string option

(** [is_ml f] True if file [f] is a .ml file. *)
val is_ml : string -> bool

(** [is_mli f] True if file [f] is a .mli file. *)
val is_mli : string -> bool

(** [soft_copy d1 d2] Copy all files and directories from directory [d1]
    into directory [d2]. Do NOT overwrite any files in [d2]. *)
val soft_copy : string -> string -> int

(** [strip_suffix str] strips all characters after and including the
    rightmost period. *)
val strip_suffix : string -> string

(** [touch_all_files_from_directory src_dir tgt_dir ext] For every file
    in [src_dir] with file extension [ext], refresh/touch the corresponding
    file in [tgt_dir]. Creates [tgt_dir] files if they did not already
    exist.
    We use this in the harness to create empty solutions for students who
    did not submit anything. *)
val touch_all_files_from_directory : string -> string -> string -> unit
