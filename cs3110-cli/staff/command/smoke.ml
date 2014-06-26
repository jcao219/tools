open Cli_constants
open Io_util
open Filepath_util
open Process_util

(** [smoke_compile_one ms d] compile each module in the list [ms] under the
 * containing directory [d]. Generate emails and save the files for failures. *)
let smoke_compile_one (targets : string list) (dir_name : string) : unit =
  let () = Format.printf "\n## Smoke target '%s' ##\n" (tag_of_path dir_name) in
  let failed_targets : string list ref = ref [] in
  let compile_and_record (target : string) : unit =
    (* 2014-01-09: [build] directs compiler output to stdout/stderr.  *
     * Could redirect this to the email.                              *)
    let fname = Format.sprintf "%s.ml" target in
    if not (Sys.file_exists fname) then
      let _ = Format.printf "'%s' not found\n%!" fname in
      failed_targets := target :: !failed_targets
    (** Temporary patch *)
    else if (ignore (Build.build false target); 0) <> 0 then
      failed_targets := target :: !failed_targets
  in
  let cwd = Sys.getcwd () in
  let () = Sys.chdir dir_name in
  let () = List.iter compile_and_record targets in
  let () = Sys.chdir cwd in
  (* If there were failures, record an email message *)
  match !failed_targets with
    | [] -> ()
    | h::t ->
      let name = tag_of_path dir_name in
      (* Write the email message *)
      (* 2014-01-09: Sorry, I'd love to save the email message at the *
       * top of this file, but that's not allowed for format strings. *)
      let message = Format.sprintf "Dear %s, \n\
\n\
The following files from your CMS submission were either missing or failed to compile via `cs3110 compile` \n\
* %s.ml\n\n\
Please update your submission on CMS so that `cs3110 compile <file>` succeeds for each of the above files. If the required changes were small, you will not be charged a late/slip day. If the changes were non-trivial, you will lose a slip day if you have any remaining or the late penalty. \n\
Good luck!\n\
\n\
--- Automatically generated message from the CS3110 test harness ---\n\
" name (String.concat ".ml\n* " (List.rev !failed_targets)) in
      let email_chn = open_out (Format.sprintf "./_email/%s.txt" name) in
      let () = output_string email_chn message in
      let () = close_out email_chn in
      (* Save each failing source file *)
      let nocompile_dir = Format.sprintf "./%s/%s" cNOCOMPILE_DIR name in
      let () = ensure_dir nocompile_dir in
      let copy_file (target : string) =
        let fname = Format.sprintf "%s/%s.ml" dir_name target in
        (* Either copy the existing source code or initialize an empty file.
         * The empty file will give an 100% diff later, whereas no file would
         * raise an error. *)
        let exit_code =
          if not (Sys.file_exists fname)
          then Sys.command (Format.sprintf "touch %s/%s.ml" nocompile_dir target)
          else Sys.command (Format.sprintf "cp %s %s" fname nocompile_dir)
        in
        if exit_code <> 0
        then Format.printf "ERROR: Failed to save file %s/%s.ml\n" dir_name target
      in
      List.iter copy_file !failed_targets

(**
 * [smoke dirs] Attempt to compile each target in each directory of [dirs].
 * Failures are recorded in email messages and the files are saved under the
 * [cNOCOMPILE_DIR] folder
 *)
let run (directories : string list) : unit =
  let directories = strip_trailing_slash_all directories in
  (* setup *)
  let () = ensure_dir cEMAIL_DIR in
  let () = ensure_dir cNOCOMPILE_DIR in
  (* Try to infer targets from test names. Assuming all tests are
   * of form 'file_test.ml' *)
  let targets =
    if Sys.file_exists cSMOKE_TARGETS
    then List.map strip_suffix (read_lines (open_in cSMOKE_TARGETS))
    else if Sys.file_exists cTESTS_DIR
    then Array.fold_right (fun f acc ->
      (fst(rsplit f '_')) :: acc) (Sys.readdir cTESTS_DIR) []
    else raise (File_not_found cSMOKE_TARGETS)
  in
  (* Compile the targets in one directory. Accumulate an email message  *
   * outlining the failures.                                            *)
  List.iter (smoke_compile_one targets) directories
