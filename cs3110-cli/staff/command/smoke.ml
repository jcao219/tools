open Core.Std
open Cli_util
open Process_util

type options = Cli_config.smoke_command_options

(** [copy_directory ?v o n d] save a copy of the directory [dir] for later diffing.
    The options [o] specify where to save the copy. *)
let copy_directory ?(verbose=false) (opts : options) (netid : string) (dir : string) : unit =
  let target_dir = Format.sprintf "%s/%s" opts.nocompile_directory netid in
  let ()         =
    begin match Sys.file_exists target_dir with
      | `No | `Unknown ->
        check_code (Sys.command (Format.sprintf "mkdir %s" target_dir))
      | `Yes           ->
        let () = if verbose then Format.printf "[smoke] removing old diff directory '%s'.\n" target_dir in
        check_code (Sys.command (Format.sprintf "rm -rf %s; mkdir %s" target_dir target_dir))
    end
  in
  let ()         = if verbose then Format.printf "[smoke] copying directory '%s' to folder '%s'.\n" dir target_dir in
  let exit_code  = Sys.command (Format.sprintf "cp -r %s/. %s" dir target_dir) in
  let ()         = if exit_code <> 0 then Format.printf "[smoke] ERROR failed to save directory '%s'\n" dir in
  ()

(** [compile_target ?v d t] change into directory [d], compile the file [t], return
    true if compilation succeeded. *)
let compile_target ?(verbose=false) (dir : string) (target : string) : bool =
  let cwd       = Sys.getcwd () in
  let ()        = Sys.chdir dir in
  let exit_code = Compile.compile ~verbose:verbose target in
  let ()        = ignore (Clean.clean "compile") in
  let ()        = Sys.chdir cwd in
  0 = exit_code

(** [smoke_target ?v d fs c] Smoke test a single file [c] for directory [d].
    First check whether the file exists, then compile it. On failure, save data
    for a future [cs3110 diff] and add the target [c] to the list [fs] of failed targets. *)
let smoke_target ?(verbose=false) (dir : string) (curr_target : string) (failed_targets : string list) : string list =
  let curr_target' = ensure_ml curr_target in
  let curr_path    = Format.sprintf "%s/%s" dir curr_target' in
  let ()           = if verbose then Format.printf "[smoke] searching for target '%s'...\n" curr_path in
  begin match Sys.file_exists curr_path with
    | `Yes           -> (* Compile. Need to flush printouts here. *)
      let () = if verbose then Format.printf "[smoke] found target '%s', compiling...\n" curr_path in
      begin match compile_target ~verbose:verbose dir curr_target' with
        | true  ->
           let () = if verbose then Format.printf "[smoke] successfully compiled '%s'.\n" curr_path in
           failed_targets
        | false ->
           let () = if verbose then Format.printf "[smoke] failed to compile '%s'.\n" curr_path in
           curr_target' :: failed_targets
      end
    | `No | `Unknown -> (* No file = compile error *)
      let () = if verbose then Format.printf "[smoke] target '%s' not found.\n" curr_path in
      curr_target' :: failed_targets
  end

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

(** [write_email o n fs] Write an email to student [n] explaining that files [fs]
    failed to compile. The options [o] specify where the email is saved. *)
let write_email (opts : options) (netid : string) (failures : string list) : unit =
  let fname = Format.sprintf "%s/%s.txt" opts.email_directory netid in
  let message = make_email_message netid failures in
  Out_channel.write_lines fname message

(** [smoke_directory ?v o d] compile all targets in the folder [dir], generate
    an email message and save the results if any target fails to compile. *)
let smoke_directory ?(verbose=false) (opts : options) (dir : string) : unit =
  let ()             = if verbose then Format.printf "[smoke] copying release files from '%s' to '%s'.\n" opts.input_directory dir in
  let ()             = ignore (soft_copy opts.input_directory dir) in
  let ()             = if verbose then Format.printf "[smoke] compiling directory '%s'\n" dir in
  let failed_targets = StringSet.fold_right opts.compilation_targets
                         ~f:(smoke_target ~verbose:verbose dir)
                         ~init:[]
  in
  begin match failed_targets with
    | []    -> (* Success! Nothing more to do. *)
       let ()    = if verbose then Format.printf "[smoke] successfully compiled all targets in '%s'\n" dir in
       ()
    | _::_ -> (* Something failed to compile. Save the directory, send an email. *)
       let ()    = if verbose then Format.printf "[smoke] failed to compile all targets in '%s'. Saving directory and email.\n" dir in
       let netid = filename_of_path dir in
       let ()    = copy_directory ~verbose:verbose opts netid dir in
       let ()    = write_email opts netid failed_targets in
       ()
end

(** [smoke ?v o ds] Smoke test each submission directory [ds]. *)
let smoke ?(verbose=false) (opts : options) (directories : string list) : unit =
  List.iter ~f:(smoke_directory ~verbose:verbose opts) directories

(** [validate_smoke_targets r tgts] Assert that the name of each target
    matches a file in the release directory. *)
let validate_smoke_targets ~release (targets : StringSet.t) : unit =
  let prefix = release ^ "/" in
  StringSet.iter targets
    ~f:(fun t ->
        let msg = Format.sprintf "Smoke test target '%s' not included in release directory." t in
        assert_file_exists ~msg:msg (prefix ^ (ensure_ml t)))

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
      +> flag ~aliases:["-v"]               "-verbose"        no_arg           ~doc:" Print debugging information."
      +> flag ~aliases:["-t"]               "-target"        (listed string)   ~doc:"TARGET Attempt to compile file TARGET in each SUBMISSION directory."
      +> flag ~aliases:["-r";"-i";"-input"] "-release"       (optional file)   ~doc:"DIR Copy starter code from directory DIR."
      +> flag ~aliases:["-e"]               "-email-dir"     (optional string) ~doc:"DIR Save generated email messages to directory DIR."
      +> flag ~aliases:["-n"]               "-nocompile-dir" (optional string) ~doc:"DIR Save submissions that do not compile to directory DIR."
      +> anon (sequence ("submission" %: string))
    )
    (fun v targets release_dir email_dir nocompile_dir subs () ->
      let cfg = Cli_config.init () in
      let opts = ({
        compilation_targets = begin match targets with
                                | []   -> cfg.smoke.compilation_targets
                                | _::_ -> StringSet.of_list targets
                              end;
        email_directory     = Option.value email_dir     ~default:cfg.smoke.email_directory;
        input_directory     = Option.value release_dir   ~default:cfg.smoke.input_directory;
        nocompile_directory = Option.value nocompile_dir ~default:cfg.smoke.nocompile_directory;
      } : options) in
      let () = assert_file_exists opts.input_directory in
      let () = ensure_dir (Format.sprintf "%s/_smoke" Cli_config.cOUTPUT_DIRECTORY) in
      let () = ensure_dir         opts.email_directory in
      let () = ensure_dir         opts.nocompile_directory in
      let () = validate_smoke_targets ~release:opts.input_directory opts.compilation_targets in
      smoke ~verbose:v opts (at_expand subs)
    )
