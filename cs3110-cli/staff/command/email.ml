open Cli_constants
open Io_util
open Filepath_util

(** [email ()] send the email messages stored in the _email directory.  
 * Assumes that every folder name under _email is a valid Cornell netid 
 * Uses [mutt]. You must install [mutt]. *)
let run () : unit = 
  let () = assert_file_exists cEMAIL_DIR in
  let () = assert_file_exists cEMAIL_ADMINS in
  (* Use the [mail] command to package off the email message *)
  let num_sent = 
    Array.fold_left (fun num_sent (msg_file : string) ->
      let recipient = (strip_suffix msg_file) ^ "@cornell.edu" in
      let bcc = Format.sprintf "-b '%s'" (String.concat "' -b '" (read_lines (open_in cEMAIL_ADMINS))) in
      let cmd = Format.sprintf "mutt -s '%s' %s '%s' < %s/%s" cEMAIL_SUBJECT bcc recipient cEMAIL_DIR msg_file in
      let () = Format.printf "### Executing '%s'\n%!" cmd in
      if (Sys.command cmd) = 0 then
        num_sent +1
      else 
        let () = print_endline ("Failed to send message to: " ^ recipient) in 
        num_sent
    ) 0 (Sys.readdir cEMAIL_DIR)
  in
  Format.printf "Finished sending %d messages.\n%!" num_sent
