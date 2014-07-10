open Cli_constants
open Io_util
open Filepath_util

let cfg = Config.init()

let get_recipient (msg_file : string) : string =
  (strip_suffix msg_file) ^ "@cornell.edu"

(** send one email message. *)
let send_one_email (msg_file : string) (recipient : string) : bool =
  let bcc = Format.sprintf "-b '%s'" (String.concat "' -b '" cfg.email.admins) in
  let cmd = Format.sprintf "mutt -s '%s' %s '%s' < %s/%s" cfg.email.subject bcc recipient cfg.email.output_directory msg_file in
  let () = Format.printf "### Executing '%s'\n%!" cmd in
  (Sys.command cmd) = 0

(** [send_all_emails ()] loop over email messages stored in [cEMAIL_DIR].
 * Send one to each student, record and return the number of messages sent. *)
let send_all_emails () : int =
  Array.fold_left (fun num_sent (msg_file : string) ->
    let recipient = get_recipient msg_file in
    if send_one_email msg_file recipient then
      num_sent +1
    else
      let () = print_endline ("Failed to send message to: " ^ recipient) in
      num_sent
  ) 0 (Sys.readdir cfg.email.output_directory)

(** [email ()] send the email messages stored in the _email directory.
 * Assumes that every folder name under _email is a valid Cornell netid
 * Uses the unix utility [mutt]. *)
let run () : unit =
  let () = assert_file_exists cfg.email.output_directory in
  (* Use the [mail] command to package off the email message *)
  let num_sent = send_all_emails () in
  Format.printf "Finished sending %d messages.\n%!" num_sent
