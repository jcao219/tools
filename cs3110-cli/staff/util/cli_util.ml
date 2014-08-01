open Core.Std

exception Command_not_found of string
exception File_not_found of string
exception Invalid_filepath of string

module StringSet = Set.Make(String)

(** [assert_file_exists fname] raises [File_not_found] if [f] does not exist.
    Optional argument specifies the error message. *)
let assert_file_exists ?msg (fname : string) : unit =
  begin match Sys.file_exists fname with
    | `Yes           -> ()
    | `No | `Unknown -> raise (File_not_found (Option.value msg ~default:fname))
  end

(** [directories_of_list dir] Build a list of directories from the file [dir].
    The directory [dir] should contain a newline-separated list of filenames. *)
let directories_of_list (fname : string) : string list =
  let () = assert_file_exists fname in
  (* Argument is @path/to/list-of-directory-names.    *
   * Extract a list of directory names from the list. *)
  let dir_names = In_channel.read_lines fname in
  let prefix    = begin match String.rsplit2 ~on:'/' fname with
                    | Some (prefix,_) -> prefix
                    | None            -> ""
                  end
  in
  List.fold_right dir_names
    ~f:(fun name acc -> (Format.sprintf "%s/%s" prefix name) :: acc)
    ~init:[]

(** [at_expand dirs] optionally expand a file containing a list into a list of directories.
    If the input is a singleton list where the first element is prefixed by and '@' character,
    treat this input as a file containing a list of newline-separated strings. Create directory
    names from these strings.
    Else return the input unchanged. I would have named it '@_expand' but that didn't compile. *)
let at_expand (dirs : string list) : string list =
  begin match dirs with
    | []       -> dirs
    | fname::_ ->
       begin match String.lsplit2 ~on:'@' fname with
         | Some ("", tl) -> directories_of_list tl
         | Some _ | None -> dirs
       end
  end

(** [check_installed cmd] False is the unix tool [cmd] is not found. *)
let check_installed (cmd : string) : bool =
  (* Try to run the command's version info, pipe stdout to stderr *)
  let check_cmd = Format.sprintf "command %s -v > /dev/null 2>&1" cmd in
  0 = (Sys.command check_cmd)

(** [assert_installed cmd] raises [Command_not_found] if the unix tool [cmd]
    is not installed. *)
let assert_installed (cmd : string) : unit =
  if not (check_installed cmd) then
    let msg = Format.sprintf "Required command '%s' is not installed.\n" cmd in
    raise (Command_not_found msg)

(** [is_ml f] True if file [f] is a .ml file. *)
let is_ml (fname : string) : bool =
  String.is_suffix fname ~suffix:".ml"

(** [is_mli f] True if file [f] is a .mli file. *)
let is_mli (fname : string) : bool =
  String.is_suffix fname ~suffix:".mli"

(** [ensure_ml f] Check if [f] has a '.ml' suffix. If not, append one. *)
let ensure_ml (fname : string) : string =
  if is_ml fname
  then fname
  else fname ^ ".ml"

(** [ensure_dir d] creates the directory [d] if it does not exist already. *)
let ensure_dir (dir_name : string) : unit =
  begin match Sys.file_exists dir_name with
    | `No | `Unknown -> Unix.mkdir dir_name (* default perm is 0777 *)
    | `Yes           -> ()
  end

(** [file_is_empty fname] True if [fname] contains nothing. *)
let file_is_empty (fname : string) : bool =
  (* [test] returns 0 exit status if file is not empty *)
  0 <> (Sys.command (Format.sprintf "test -s %s" fname))

(** [files_exist fs] True if all files in the list [fs] exist.
    False is any one file is a 'no' or 'unknown'. *)
let rec files_exist (files : string list) : bool =
  begin match files with
    | []     -> true
    | hd::tl ->
       begin match Sys.file_exists hd with
         | `No | `Unknown -> false
         | `Yes           -> files_exist tl
       end
  end

(** [filter_directory ~p d] Read files in directory [d], remove the files not matching the
    predicate [p]. *)
let filter_directory ~f (dir : string) : string list =
  let all_files = Sys.readdir dir in
  Array.fold_right all_files
    ~f:(fun x acc -> if f x then x :: acc else acc)
    ~init:[]

(** [get_extension file_name] gets the extension of the file
    [file_name], i.e. the characters occuring to the right
    of the right-most occurence of the '.' character. *)
let get_extension (file_name : string) : string option =
  begin match String.rsplit2 ~on:'.' file_name with
    | Some (_,ext) -> Some ext
    | None         -> None
  end

(** [strip_trailing_slash str] removes the last character of [str],
    but only if that character is a / *)
let strip_trailing_slash (str : string) : string =
  begin match String.rsplit2 ~on:'/' str with
    | Some (hd, "") -> hd
    | Some _ | None -> str
  end

(** [filename_of_path p] Return the last item along the path [p].
    It could be a filename or a directory name, don't care.
    Given 'dir1/dir2/dir3/', this function returns 'dir3'.
    Given 'file.ml', this function is the identity. *)
let filename_of_path (path : string) : string =
  let path = strip_trailing_slash path in
  begin match String.rsplit2 ~on:'/' path with
    | Some (_,str) -> str
    | None         -> path
  end

(** [soft_copy d1 d2] Copy all files and directories from directory [d1]
    into directory [d2]. Do NOT overwrite any files in [d2]. *)
let soft_copy (dir1 : string) (dir2 : string) : int =
  Sys.command (Format.sprintf "cp -r -n %s/. %s" dir1 dir2)

(** [strip_suffix str] strips all characters after and including the
    rightmost period. *)
let strip_suffix (str : string) : string =
  begin match String.rsplit2 ~on:'.' str with
    | Some (hd,_) -> hd
    | None        -> str
  end
