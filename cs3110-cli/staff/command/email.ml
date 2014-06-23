open Cli_constants
open Io_util
open Filepath_util

(** send one email message. Get recipient from the filename, get bcc from the
 * local file of important people. *)
let send_one_email (msg_file : string) (recipient : string) : bool =
  let bcc = Format.sprintf "-b '%s'" (String.concat "' -b '" (read_lines (open_in cEMAIL_ADMINS))) in
  let cmd = Format.sprintf "mutt -s '%s' %s '%s' < %s/%s" cEMAIL_SUBJECT bcc recipient cEMAIL_DIR msg_file in
  let () = Format.printf "### Executing '%s'\n%!" cmd in
  (Sys.command cmd) = 0

(** [send_all_emails ()] loop over email messages stored in [cEMAIL_DIR].
 * Send one to each student, record and return the number of messages sent. *)
let send_all_emails () : int =
  Array.fold_left (fun num_sent (msg_file : string) ->
    let recipient = (strip_suffix msg_file) ^ "@cornell.edu" in
    if send_one_email msg_file recipient then
      num_sent +1
    else 
      let () = print_endline ("Failed to send message to: " ^ recipient) in 
      num_sent
  ) 0 (Sys.readdir cEMAIL_DIR)

(** [email ()] send the email messages stored in the _email directory.  
 * Assumes that every folder name under _email is a valid Cornell netid 
 * Uses [mutt]. You must install [mutt]. *)
let run () : unit = 
  let () = assert_file_exists cEMAIL_DIR in
  let () = assert_file_exists cEMAIL_ADMINS in
  (* Use the [mail] command to package off the email message *)
  let num_sent = send_all_emails () in
  Format.printf "Finished sending %d messages.\n%!" num_sent
