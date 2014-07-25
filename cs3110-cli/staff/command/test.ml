open Core.Std
open Filepath_util
open Process_util

let test ?(quiet=false) ?(verbose=false) ?output ?dir (main_module : string) : int =
  let ()        = if verbose then Format.printf "[test] Preparing to run inline tests from target '%s'.\n" main_module in
  let cwd       = Sys.getcwd () in
  let ()        = Sys.chdir (Option.value ~default:cwd dir) in
  let ()        = if verbose then Format.printf "[test] Searching for build directory.\n" in
  let build_dir = "_build" in (* TODO abstract this *)
  (* TODO clean this all up *)
  let exec      = Format.sprintf "%s/%s.d.byte" build_dir (strip_suffix main_module) in
  (* -log required by harness, nice to have in general, but
     destination './inline_tests.log' is hardcoded *)
  let base_args = ["inline-test-runner"; "dummy"; "-log"] in
  let args =
    begin match output with
      | Some dest ->
        if quiet
        then base_args @ ["2>& 1>/dev/null | grep '^File' >"; dest]
        else base_args @ ["-show-counts"; "&>"; dest]
      | None      ->
        if quiet
        then base_args @ [">"; "/dev/null"]
        else base_args @ ["-show-counts"]
    end
  in
  let ()  = if verbose then Format.printf "[test] Test args are '%s'.\n" (String.concat ~sep:" " args) in
  (* Note that pa_ounit doesn't give a nonzero exit status if tests are run. *)
  (* 2014-07-24: This match is a little messy, but cleaning it might affect the harness. Be careful. *)
   begin match Sys.file_exists exec with
    | `Yes           ->
      let exit_code = run_process exec args in
      let ()        = Sys.chdir cwd in
      exit_code
    | `No | `Unknown ->
      let ()  = Sys.chdir cwd in
      let ()  = Format.printf "%!" in
      let msg = Format.sprintf "Could not find file '%s'. Have you compiled target '%s'?" exec main_module in
      raise (Cli_constants.File_not_found msg)
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
      +> flag ~aliases:["-r"] "-recompile" no_arg ~doc:" Compile target before testing."
      +> flag ~aliases:["-v"] "-verbose"   no_arg ~doc:" Print debugging information."
      +> flag ~aliases:["-q"] "-quiet"     no_arg ~doc:" Run tests quietly."
      +> flag ~aliases:["-o"] "-output"    (optional string) ~doc:"FILE Save test output to the file FILE."
      +> anon ("target" %: file)
    )
    (fun r v q output target () ->
      let () = if r then check_code (Compile.compile ~verbose:v target) in
      check_code (begin match output with
      | Some dest -> test ~quiet:q ~verbose:v ~output:dest target
      | None      -> test ~quiet:q ~verbose:v target end)
    )
