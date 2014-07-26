open Core.Std
open Cli_constants
open Filepath_util
open Process_util

type options = {
  email_directory     : string;
  nocompile_directory : string;
  release_directory   : string;
  targets             : string list;
  verbose             : bool;
}

(** [make_email_message n fs] Create the body of an email to send to student [n]
    because his/her files [fs] failed to compile. *)
let make_email_message (netid : string) (failed_targets : string list) : string list =
  let prefix = "* " in
  [
    Format.sprintf "Dear %s\n" netid;
    "The following files from your CMS submission were either missing or failed to compile via `cs3110 compile`:\n";
  ] @ (List.map ~f:((^) prefix) (List.sort ~cmp:Pervasives.compare failed_targets)) @ [
    "\nPlease update your submission on CMS so that `cs3110 compile <file>` succeeds for each of the above files. If the required changes were small, you will not be charged a late/slip day. If the changes were non-trivial, you will lose either a slip day (if you have any remaining) or the late penalty.\n";
    "Good luck!\n\n";
    "--- Automatically generated message from the CS3110 test harness ---";
  ]

(** [copy_directory o n d] save a copy of the directory [dir] for later diffing.
    The options [o] specify where to save the copy. *)
let copy_directory (opts : options) (netid : string) (dir : string) : unit =
  let target_dir = Format.sprintf "%s/%s" opts.nocompile_directory netid in
  let () =
    begin match Sys.file_exists target_dir with
      | `No | `Unknown ->
        check_code (Sys.command (Format.sprintf "mkdir %s" target_dir))
      | `Yes           ->
        let () = if opts.verbose then Format.printf "[smoke] removing old diff directory '%s'.\n" target_dir in
        check_code (Sys.command (Format.sprintf "rm -rf %s; mkdir %s" target_dir target_dir))
    end
  in
  let () = if opts.verbose then Format.printf "[smoke] copying directory '%s' to folder '%s'.\n" dir target_dir in
  let exit_code = Sys.command (Format.sprintf "cp -r %s/. %s" dir target_dir) in
  let () = if exit_code <> 0 then Format.printf "[smoke] ERROR failed to save directory '%s'\n" dir in
  ()

(** [write_email o n fs] Write an email to student [n] explaining that files [fs]
    failed to compile. The options [o] specify where the email is saved. *)
let write_email (opts : options) (netid : string) (failures : string list) : unit =
  let fname = Format.sprintf "%s/%s.txt" opts.email_directory netid in
  let message = make_email_message netid failures in
  Out_channel.write_lines fname message

(** [compile_target d t] change into directory [d], compile the file [t], return
    true if compilation succeeded. *)
let compile_target (opts : options) (dir : string) (target : string) : bool =
  let cwd       = Sys.getcwd () in
  let ()        = Sys.chdir dir in
  let exit_code = Compile.compile ~verbose:opts.verbose target in
  let ()        = ignore (Clean.clean "compile") in
  let ()        = Sys.chdir cwd in
  0 = exit_code

(** [smoke_target o d fs c] Smoke test a single file [c] for directory [d].
    First check whether the file exists, then compile it. On failure, save data
    for a future [cs3110 diff] and add the target [c] to the list [fs] of failed targets. *)
let smoke_target (opts : options) (dir : string) (failed_targets : string list) (curr_target : string) : string list =
  let curr_target' = ensure_ml curr_target in
  let curr_path    = Format.sprintf "%s/%s" dir curr_target' in
  let () = if opts.verbose then Format.printf "[smoke] searching for target '%s'...\n" curr_path in
  begin match Sys.file_exists curr_path with
    | `Yes           -> (* Compile. Need to flush printouts here. *)
      let () = if opts.verbose then Format.printf "[smoke] found target '%s', compiling...\n" curr_path in
      begin match compile_target opts dir curr_target' with
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
  let () = if opts.verbose then Format.printf "[smoke] copying release files from '%s' to '%s'.\n" opts.release_directory dir in
  let () = ignore (soft_copy opts.release_directory dir) in
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
       let ()    = if opts.verbose then Format.printf "[smoke] failed to compile all targets in '%s'. Saving directory and email.\n" dir in
       let netid = tag_of_path dir in
       let ()    = copy_directory opts netid dir in
       let ()    = write_email opts netid failed_targets in
       ()
end

(** [smoke o ds] Smoke test each submission directory [ds]. *)
let smoke (opts : options) (directories : string list) : unit =
  List.iter ~f:(smoke_directory opts) directories

(** [get_smoke_targets o] figure out which files to compile. This could be
    set in the config file or given as a command line option. *)
let get_smoke_targets (tgts : string list) : string list =
  begin match tgts with
    | _::_ -> tgts
    | []   -> (* TODO infer, or read config *)
       raise (File_not_found "Could not determine which files to smoke test.")
  end

(** [validate_smoke_targets r tgts] Assert that the name of each target
    matches a file in the release directory. *)
let validate_smoke_targets ~release (targets : string list) : unit =
  let prefix = release ^ "/" in
  List.iter
    ~f:(fun t ->
        let msg = Format.sprintf "Smoke test target '%s' not included in release directory." t in
        assert_file_exists ~msg:msg (prefix ^ (ensure_ml t)))
    targets

let command =
  Command.basic
    ~summary:"Smoke test. Check if submissions compile. If not, save a copy & make an email message for that submission."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The smoke test is a sanity check for students. We make sure their submissions compile.";
      "If not, we generate an email that can be sent with [cs3110 email] and save a record of";
      "the submission. The policy is that students can resubmit a trivial fix with no penalty.";
      "Use [cs3110 diff] to compare saved copies with resubmissions.";
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose" no_arg          ~doc:" Print debugging information."
      +> flag ~aliases:["-t"] "-target"  (listed string) ~doc:"TARGET Attempt to compile file TARGET in each SUBMISSION directory."
      +> flag ~aliases:["-r"] "-release" (required file) ~doc:"DIR Copy starter code from directory DIR."
      +> anon (sequence ("submission" %: string))
    )
    (fun v tgts r subs () ->
      let () = ensure_dir cEMAIL_DIR in
      let () = ensure_dir cNOCOMPILE_DIR in
      let () = assert_file_exists r in
      let opts = {
        email_directory     = cEMAIL_DIR;
        nocompile_directory = cNOCOMPILE_DIR;
        release_directory   = r;
        targets             = get_smoke_targets tgts;
        verbose             = v;
      } in
      let () = validate_smoke_targets ~release:opts.release_directory opts.targets in
      smoke opts (at_expand subs)
    )
