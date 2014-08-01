open Core.Std
open Cli_util
open Process_util

(** [test ?q ?v ?c ?o ?d main] Run the inline tests in the executable created by [cs3110 compile main]. *)
let test ?(quiet=false) ?(verbose=false) ?(compile=false) ?output ?dir (main_module : string) : int =
  let main      = ensure_ml main_module in
  let ()        = if verbose then Format.printf "[test] Preparing to run inline tests from target '%s'.\n" main in
  let cwd       = Sys.getcwd () in
  let dir       = Option.value ~default:cwd dir in
  let ()        = if compile then check_code (Compile.compile ~quiet:quiet ~verbose:verbose ~dir:dir main) in
  let ()        = Sys.chdir dir in
  let exec      = Format.sprintf "%s/%s.d.byte" Cli_config.cBUILD_DIRECTORY (strip_suffix main) in
  (* [-log] required by [cs3110 harness] and nice to have in general.
     Generates the file './inline_tests.log' with all unit test names. *)
  let base_args = ["inline-test-runner"; "dummy"; "-log"] in
  let args      =
    begin match output with
      | Some dest ->
        if quiet
        then base_args @ ["2>& 1>/dev/null | grep '^File' > "; dest]
        else base_args @ ["-show-counts"; "&>"; dest]
      | None      ->
        if quiet
        then base_args @ ["&>"; "/dev/null"]
        else base_args @ ["-show-counts"]
    end
  in
  (* Note that pa_ounit doesn't give a nonzero exit status if tests are run. *)
  (* 2014-07-24: This match is a little messy, but cleaning it might affect the harness. Be careful. *)
   begin match Sys.file_exists exec with
    | `Yes           ->
       let test_cmd  = Format.sprintf "./%s %s" exec (String.concat ~sep:" " args) in
       let ()        = if verbose then Format.printf "[test] Test command is '%s'.\n" test_cmd in
       let exit_code = Sys.command test_cmd in
       let ()        = Sys.chdir cwd in
       exit_code
    | `No | `Unknown ->
      let ()         = Sys.chdir cwd in
      let ()         = Format.printf "%!" in
      let msg        = Format.sprintf "Could not find file '%s'. Have you compiled target '%s'?" exec main in
      raise (File_not_found msg)
  end

let command =
  Command.basic
    ~summary:"Checks unit tests in a compiled executable."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "[cs3110 test] dispatches the inline unit tests within a compiled";
      "executable. Test results are printed to the console. Make sure to";
      "include a call to [Pa_ounit_lib.Runtime.summarize ()] if you want";
      "to see aggregated results after each execution."
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose"   no_arg ~doc:" Print debugging information."
      +> flag ~aliases:["-r"] "-recompile" no_arg ~doc:" Compile target before testing."
      +> flag ~aliases:["-q"] "-quiet"     no_arg ~doc:" Run tests quietly."
      +> flag ~aliases:["-o"] "-output"    (optional string) ~doc:"FILE Save test output to the file FILE."
      +> anon ("target" %: file)
    )
    (fun v r q output target () ->
      check_code (begin match output with
      | Some dest -> test ~quiet:q ~compile:r ~verbose:v ~output:dest target
      | None      -> test ~quiet:q ~compile:r ~verbose:v target end)
    )
