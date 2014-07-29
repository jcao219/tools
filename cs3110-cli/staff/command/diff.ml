open Core.Std
open Cli_constants
open Filepath_util

type diff_result = Ok | NotOk
type options = {
  verbose : bool
}

let diff_result_of_string = function
  | "ok" | "Ok" -> Ok
  | _           -> NotOk
let string_of_diff_result = function
  | Ok    -> "Ok"
  | NotOk -> "NOTOK"

module DiffSpreadsheet =
  Spreadsheet.Make(struct
    type row                       = string * diff_result

    let compare_row (id,_) (id',_) = Pervasives.compare id id'
    let filename : string          = cDIFF_RESULTS
    let row_of_string str          = begin match String.lsplit2 ~on:',' str with
                                       | Some (id, data) ->
                                          (id, (diff_result_of_string data))
                                       | None            ->
                                          let msg = Format.sprintf "Improperly formatted row '%s' in diff spreadsheet." str in
                                          raise (Spreadsheet.Invalid_spreadsheet msg)
                                     end
    let string_of_row (id,result)  = id ^ "," ^ (string_of_diff_result result)
    let title : string             = "NetID,DiffResult"
  end)

(** [show_repl_prompt ()] print a prompt string for the diff repl. *)
let show_repl_prompt () : unit =
  print_string "[diff] (y/n/d/s/?) >> "

(** [show_repl_help ()] print a help string for the diff repl. *)
let show_repl_help () : unit =
  Format.printf "%s\n\n" (String.concat ~sep:"\n" [
    "Choices are:";
    "  y : Accept the diff, no penalty";
    "  n : Reject the diff. Will need to deduct slip day on CMS";
    "  d : Show the diff again";
    "  s : Show the source files (first old, then new)"
  ])

(** [diff_repl o old new] Display a diff, prompt user for a response. *)
let diff_repl (opts : options) (old_file : string) (new_file : string) : diff_result =
  let () = if opts.verbose then Format.printf "[diff] Entering diff REPL for files '%s' and '%s'.\n" old_file new_file in
  let diff_cmd =
    let which_diff = if (check_installed "colordiff") then "colordiff" else "diff" in
    Format.sprintf "%s -u %s %s | less -R" which_diff old_file new_file
  in
  let show_diff ()  = ignore (Sys.command diff_cmd) in
  let show_files () = ignore (Sys.command(Format.sprintf "less %s %s" old_file new_file)) in
  let user_input = ref None in
  let () = (* repl loop *)
    let () = show_diff () in
    while (!user_input = None) do (
      let () = show_repl_prompt () in
      begin match (String.lowercase (read_line ())) with
        | "y" | "yes" | "ok" | "okay" -> user_input := Some Ok
        | "n" | "no" | "notok" | "nu" -> user_input := Some NotOk
        | "d" | "diff"                -> show_diff ()
        | "s" | "show"                -> show_files ()
        | "h" | "help" | "?"          -> show_repl_help ()
        | _                           -> let () = print_endline "Invalid option" in show_repl_help ()
      end
    ) done
  in
  let () = if opts.verbose then print_endline "[diff] Exiting diff REPL." in
  Option.value_exn
    ~message:"MAJOR PROBLEM WITH DIFF REPL! user input should NEVER be [None] at this point (but it is)."
    (!user_input)

(** [files_match f1 f2] True if [f1] and [f2] are textually identical. *)
let files_match (old_file : string) (new_file : string) : bool =
  let cmd = Format.sprintf "diff -q %s %s > /dev/null" old_file new_file in
  0 = (Sys.command cmd)

(** [diff_files o old new] Run a diff between files [old] and [new]. Prompt the user for judgment *)
let diff_files (opts : options) (old_file : string) (new_file : string) : diff_result =
  let ()   = if opts.verbose then Format.printf "[diff] Diffing files '%s' and '%s'.\n" old_file new_file in
  if not (files_exist [old_file; new_file]) then
    (* 2014-07-15: file2 is sure to exist, but whatever *)
    let () = if opts.verbose then Format.printf "[diff] Passes trivially. One of the files is missing.\n" in
    Ok
  else if files_match old_file new_file then
    let () = if opts.verbose then Format.printf "[diff] Passes trivially. Both files identical.\n" in
    Ok
  else if file_is_empty old_file then
    let () = if opts.verbose then Format.printf "[diff] Fails trivially. The old file was empty.\n" in
    NotOk
  else
    diff_repl opts old_file new_file

(** [diff_files o old new] run multiple diffs comparing the files in [new] against matching files in [old].
    We only care about files that exist in [old]. (Note: those files may be empty) *)
let diff_directories (opts : options) (old_dir : string) (new_dir : string) : diff_result =
  let old_files = Sys.readdir old_dir in
  Array.fold
    old_files
    ~init:Ok
    ~f:(fun r1 fname ->
        (* Need full path to each file *)
        let new_file = new_dir ^ "/" ^ fname in
        let old_file = old_dir ^ "/" ^ fname in
        let r2 = diff_files opts old_file new_file in
        begin match (r1, r2) with
          | Ok, Ok    -> Ok
          | _, NotOk -> NotOk
          | NotOk, _ -> NotOk
        end
       )

(** [diff_student o t d] diff the current submission [d] of a student against
    the most recent past submission. Save results in the spreadsheet [t]. *)
let diff_student (opts : options) (tbl : DiffSpreadsheet.t) (new_dir : string) : DiffSpreadsheet.t =
  let netid      = filename_of_path new_dir in
  let old_dir    = Format.sprintf "%s/%s" cNOCOMPILE_DIR netid in
  begin match Sys.file_exists old_dir with
    | `No | `Unknown ->
      let ()     = if opts.verbose then Format.printf "[diff] Skipping student '%s'. No prior submission.\n" netid in
      tbl
    | `Yes ->
      let ()     = if opts.verbose then Format.printf "[diff] Running diff on student '%s'.\n" netid in
      let result = diff_directories opts old_dir new_dir in
      let row    = (netid, result) in
      DiffSpreadsheet.add_row tbl ~row:row
  end

(** [diff o dirs] Run a diff comparing the submission in each directory of [dirs]
    with the result saved for the student on the last execution of [cs3110 smoke]. *)
let diff (opts : options) (dirs : string list) : unit =
  let tbl = List.fold dirs ~init:(DiffSpreadsheet.create ()) ~f:(diff_student opts) in
  let ()  = DiffSpreadsheet.write tbl ~file:cDIFF_RESULTS in
  let ()  = Format.printf "Finished diffing %d submissions. See '%s' for results.\n" (DiffSpreadsheet.count_rows tbl) cDIFF_RESULTS in
  ()

let command =
  Command.basic
    ~summary:"Run a diff on resubmissions, ask the user for a judgment."
    ~readme:(fun () -> Core.Std.String.concat ~sep:"\n" [
      "The [cs3110 diff] command is for checking no-compile fixes.";
      "It compares current submissions against submissions that didn't";
      "compile in the last run of [cs3110 smoke] using the Unix tool [diff].";
      "Each diff is displayed in the terminal and the user is asked to approve";
      "the changes. Results are stored in a .csv file for future reference.";
      "(The .csv cannot be uploaded directly to CMS.)"
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose" no_arg ~doc:" Print debugging information."
      +> anon (sequence ("submission" %: file))
    )
    (fun v subs () ->
      (* TODO replace constants with options *)
      let ()  = assert_installed "diff" in
      let opts = {
        verbose = v;
      } in
      diff opts (at_expand subs)
    )
