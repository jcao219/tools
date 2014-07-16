open Core.Std
open Cli_constants
open Io_util
open Filepath_util
open Process_util

type options = {
  email_directory     : string;
  nocompile_directory : string;
  targets             : string list;
  verbose             : bool;
}

(** [make_email_message n fs] Create the body of an email to send to student [n]
    because his/her files [fs] failed to compile. *)
let make_email_message (netid : string) (failed_targets : string list) : string list =
  let prefix = "* " in
  [
    Format.sprintf "Dear %s\n" netid;
    "The following files from your CMS submission were either missing or failed to compile via `cs3110 compile`:";
  ] @ (List.map ~f:((^) prefix) failed_targets) @ [
    "Please update your submission on CMS so that `cs3110 compile <file>` succeeds for each of the above files. If the required changes were small, you will not be charged a late/slip day. If the changes were non-trivial, you will lose either a slip day (if you have any remaining) or the late penalty.\n";
    "Good luck!\n\n";
    "--- Automatically generated message from the CS3110 test harness ---";
  ]

(** [copy_directory o n d] save a copy of the directory [dir] for later diffing.
    The options [o] specify where to save the copy. *)
let copy_directory (opts : options) (netid : string) (dir : string) : unit =
  let existing_dir = Format.sprintf "%s/%s" opts.nocompile_directory netid in
  let () =
    begin match Sys.file_exists existing_dir with
      | `No | `Unknown -> ()
      | `Yes           ->
        let () = if opts.verbose then Format.printf "[smoke] removing old diff directory '%s'.\n" existing_dir in
        ignore (Sys.command (Format.sprintf "rm -rf %s" existing_dir))
    end
  in
  let () = if opts.verbose then Format.printf "[smoke] copying directory '%s' to folder '%s'.\n" dir opts.nocompile_directory in
  let exit_code = Sys.command (Format.sprintf "cp -r %s %s" dir opts.nocompile_directory) in
  let () = if exit_code <> 0 then Format.printf "[smoke] ERROR failed to save directory '%s'\n" dir in
  ()

let write_email (opts : options) (netid : string) (failures : string list) : unit =
  let fname = Format.sprintf "%s/%s.txt" opts.email_directory netid in
  let message = make_email_message netid failures in
  Out_channel.write_lines fname message

(** [compile_target d t] change into directory [d], compile the file [t], return
    true if compilation succeeded. *)
let compile_target (dir : string) (target : string) : bool =
  let cwd = Sys.getcwd () in
  let () = Sys.chdir dir in
  (* TODO remove this Substring thing *)
  let target = String.slice target 0 ((String.length target) - 3) in
  let exit_code = Build.run target in (* TODO change to Compile.compile target *)
  (* TODO clean target *)
  let () = Sys.chdir cwd in
  0 = exit_code

let smoke_target (opts : options) (dir : string) (failed_targets : string list) (curr_target : string) : string list =
  let curr_target' = ensure_ml curr_target in
  let curr_path    = Format.sprintf "%s/%s" dir curr_target' in
  let () = if opts.verbose then Format.printf "[smoke] searching for target '%s'...\n" curr_path in
  begin match Sys.file_exists curr_path with
    | `Yes           -> (* Compile *)
      let () = if opts.verbose then Format.printf "[smoke] found target '%s', compiling...\n" curr_path in
      begin match compile_target dir curr_target' with
        | true  ->
           let () = if opts.verbose then Format.printf "[smoke] successfully compiled '%s'.\n" curr_path in
           failed_targets
        | false ->
           let () = if opts.verbose then Format.printf "[smoke] failed to compile '%s'.\n" curr_path in
           curr_target' :: failed_targets
      end
    | `No | `Unknown -> (* No file = compile error *)
      let () = if opts.verbose then Format.printf "[smoke] target '%s' not found.\n" curr_path in
      curr_target' :: failed_targets
  end

(** [smoke_directory o d] compile all targets in the folder [dir], generate
    an email message and save the results if any target fails to compile. *)
let smoke_directory (opts : options) (dir : string) : unit =
  let () = if opts.verbose then Format.printf "[smoke] compiling directory '%s'\n" dir in
  let failed_targets = List.rev (List.fold
    ~f:(smoke_target opts dir)
    ~init:[]
    opts.targets)
  in
  begin match failed_targets with
    | []    -> (* Success! Nothing more to do. *)
       let () = if opts.verbose then Format.printf "[smoke] successfully compiled all targets in '%s'\n" dir in
       ()
    | _::_ -> (* Something failed to compile. Save the directory, send an email. *)
       let () = if opts.verbose then Format.printf "[smoke] failed to compile all targets in '%s'. Saving directory and email.\n" dir in
       let netid = tag_of_path dir in
       let () = copy_directory opts netid dir in
       let () = write_email opts netid failed_targets in
       ()
end

let smoke (opts : options) (directories : string list) : unit =
  let directories = strip_trailing_slash_all directories in
  (* TODO fold here, collect a message, then write it? *)
  (* meh, the message should be done in smoke_dir *)
  List.iter ~f:(smoke_directory opts) directories

(** [get_smoke_targets o] figure out which files to compile. This could be
    set in the config file or given as a command line option. *)
let get_smoke_targets (tgts : string list) : string list =
  begin match tgts with
    | _::_ -> tgts
    | []   ->
       begin match Sys.file_exists cSMOKE_TARGETS with
         | `Yes           ->
            List.map ~f:strip_suffix (read_lines (open_in cSMOKE_TARGETS))
         | `No | `Unknown ->
            begin match Sys.file_exists cTESTS_DIR with
              | `Yes -> List.map
                          ~f:(fun f -> fst (rsplit f '_'))
                          (List.filter
                             ~f:is_valid_test_file
                             (Array.to_list (Sys.readdir cTESTS_DIR))
                          )
              | `No | `Unknown -> raise (File_not_found cSMOKE_TARGETS)
            end
       end
  end

let command =
  Command.basic
    ~summary:"Smoke test. Check if submissions compile. Save a copy & make an email for submissions that don't compile."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The smoke test is a sanity check for students. We make sure their submissions compile.";
      "If not, we generate an email that can be sent with [cs3110 email] and save a record of";
      "the submission. The policy is that students can resubmit a trivial fix with no penalty.";
      "Use [cs3110 diff] to compare saved copies with resubmissions. Specify which files to";
      "compile with the [-target] option or the config file.";
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose" no_arg ~doc:" Print debugging information."
      +> flag ~aliases:["-t"] "-target" (listed string) ~doc:"target Attempt to compile TARGET in each SUBMISSION directory."
      +> anon (sequence ("submission" %: string))
    )
    (fun v tgts subs () ->
     let () = ensure_dir cEMAIL_DIR in
     let () = ensure_dir cNOCOMPILE_DIR in
     let opts = {
       email_directory     = cEMAIL_DIR;
       nocompile_directory = cNOCOMPILE_DIR;
       targets             = get_smoke_targets tgts;
       verbose             = v;
     } in
     smoke opts (at_expand subs))
