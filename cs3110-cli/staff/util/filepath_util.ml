open Cli_constants
open Io_util

(** [strip_suffix str] strips all characters after and including the
 * rightmost period (.) *)
let strip_suffix (filename : string) : string =
  if (String.contains filename '.')
  then String.sub filename 0 (String.rindex filename '.')
  else filename

(** [strip_trailing_slash str] removes the last character of [s],
 * but only if that character is a / *)
let strip_trailing_slash (s : string) =
  let len = String.length s in
  if s.[len-1] = '/'
  then String.sub s 0 (len - 1)
  else s

(** [assert_file_exists f] raises [File_not_found] if [f] does not exist *)
let assert_file_exists ?msg (filename : string) : unit =
  if not (Sys.file_exists filename) then
    raise (File_not_found (Core.Std.Option.value msg ~default:filename))

(** [is_ml f] True if file [f] is a .ml file. *)
let is_ml (fname : string) : bool =
  Core.Std.String.is_suffix fname ~suffix:".ml"

(** [is_mli f] True if file [f] is a .mli file. *)
let is_mli (fname : string) : bool =
  Core.Std.String.is_suffix fname ~suffix:".mli"

(** [ensure_ml f] Check if [f] has a '.ml' suffix. If not, append one. *)
let ensure_ml (fname : string) : string =
  if is_ml fname
  then fname
  else fname ^ ".ml"

(** [ensure_dir d] creates the directory [d] if it does not exist already. *)
let ensure_dir (dir_name : string) =
  if not (Sys.file_exists dir_name) then
    Unix.mkdir dir_name 0o777

(**
 * [directories_of_list dir] Build a list of directories from the file [dir].
 * [dir] should contain a newline-separated list of directories.
 * Example: to test files { dir/a , dir/b , dir/c }, you may
 *   > cat "a\nb\nc" > dir/files
 *   > cs3110 smoke @dir/files
 * This is the same as
 *   > cs3110 smoke dir/a dir/b dir/c
 *)
let directories_of_list (fname : string) : string list =
  let () = assert_file_exists fname in
  (* Argument is @path/to/list-of-directory-names.    *
   * Extract a list of directory names from the list. *)
  let dir_names = read_lines (open_in fname) in
  let prefix = String.sub fname 0 (String.rindex fname '/') in
  (* Return the fully-inferred list of directories *)
  List.rev (List.fold_left (fun acc name -> (Format.sprintf "%s/%s" prefix name) :: acc) [] dir_names)

(** [at_expand dirs] optionally expand a file containing a list into a list of directories.
    If the input is a singleton list where the first element is prefixed by and '@' character,
    treat this input as a file containing a list of newline-separated strings. Create directory
    names from these strings.
    Else return the input unchanged. I would have named it '@_expand' but that didn't compile. *)
let at_expand (dirs : string list) : string list =
  begin match dirs with
    | [fname] when (0 < String.length fname) && fname.[0] = '@' ->
       (* Remove leading '@' *)
       directories_of_list (String.sub fname 1 ((String.length fname) - 1))
    | [] | _::_ -> dirs
  end

(** [get_extension file_name] gets the extension of the file
 *  [file_name]. The extension is defined to be the characters
 *  occuring to the right of the right-most occurence of the '.'
 *  character. *)
let get_extension (file_name : string) : string option =
  begin match Core.Std.String.rsplit2 ~on:'.' file_name with
    | Some (_,ext) -> Some ext
    | None         -> None
  end

(** [filter_directory ~p d] Read files in directory [d], remove the files not matching the
    predicate [p]. *)
let filter_directory ~f (dir : string) : string list =
  (* TODO catch exception / check if directory exists. *)
  let all_files = Sys.readdir dir in
  Core.Std.Array.fold_right all_files
    ~f:(fun x acc -> if f x then x :: acc else acc)
    ~init:[]

(** [filename_of_path p] Return the last item along the path [p].
    It could be a filename or a directory name, don't care.
    Given 'dir1/dir2/dir3/', this function returns 'dir3'.
    Given 'file.ml', this function is the identity. *)
let filename_of_path (path : string) : string =
  let path = strip_trailing_slash path in
  begin match Core.Std.String.rsplit2 ~on:'/' path with
    | Some (_,str) -> str
    | None         -> path
  end

(** [soft_copy d1 d2] Copy all files and directories from directory [d1]
    into directory [d2]. Do NOT overwrite any files in [d2]. *)
let soft_copy (dir1 : string) (dir2 : string) : int =
  Sys.command (Format.sprintf "cp -r -n %s/. %s" dir1 dir2)

let files_exist (files : string list) : bool =
  List.fold_left (fun acc f -> acc && Sys.file_exists f) true files

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

(** [file_is_empty fname] True if [fname] contains nothing. *)
let file_is_empty (fname : string) : bool =
  (* [test] returns 0 exit status if file is not empty *)
  0 <> (Sys.command (Format.sprintf "test -s %s" fname))
