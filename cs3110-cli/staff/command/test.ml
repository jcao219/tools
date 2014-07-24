open Core.Std
open Filepath_util

let test ?(quiet=false) ?output ?dir (main_module : string) : int =
  let cwd = Sys.getcwd () in
  let ()  = Sys.chdir (Option.value ~default:cwd dir) in
  let build_dir = "_build" in (* TODO abstract this *)
  let main_module = strip_suffix main_module in
  (* TODO clean this all up *)
  let exec = Format.sprintf "%s/%s.d.byte" build_dir main_module in
  (* -log required by harness, nice to have in general, but
     destination './inline_tests.log' is hardcoded *)
  let base_cmd = [exec; "inline-test-runner"; "dummy"; "-log"] in
  let cmd =
    begin match output with
      | Some dest ->
        if quiet
        then base_cmd @ ["2>& 1>/dev/null | grep '^File' >"; dest]
        else base_cmd @ ["-show-counts"; "&>"; dest]
      | None      ->
        if quiet
        then base_cmd @ [">"; "/dev/null"]
        else base_cmd @ ["-show-counts"]
    end
  in
  (* Note that pa_ounit doesn't give a nonzero exit status if tests are run. *)
  let exit_code = Sys.command (String.concat ~sep:" " cmd) in
  let ()        = Sys.chdir cwd in
  exit_code

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
      +> flag ~aliases:["-q"] "-quiet" no_arg ~doc:" Run quietly. Do not print debug statements."
      +> flag ~aliases:["-o"] "-output" (optional string) ~doc:"FILE Save test output to the file FILE."
      +> anon ("target" %: file)
    )
    (fun r q output target () ->
      (* let () = if r then Command.compile main in *)
      Process_util.check_code (begin match output with
      | Some dest -> test ~quiet:q ~output:dest target
      | None -> test ~quiet:q target end)
    )
