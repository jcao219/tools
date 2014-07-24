open Core.Std
open Process_util
open Filepath_util

(** [run file args] run the executable generated by [cs3110 compile file] *)
let run ?(verbose=false) (main_module : string) (args : string list) : int =
  let ()        = if verbose then Format.printf "[run] Preparing to run target '%s' with args '%s'.\n"
                                                main_module (String.concat ~sep:" " args) in
  let main      = strip_suffix main_module in
  let ()        = if verbose then Format.printf "[run] Searching for build directory.\n" in
  let build_dir = "_build" in (* TODO abstract with config-file *)
  let exec      = Format.sprintf "%s/%s.d.byte" build_dir main in
  let ()        = if verbose then Format.printf "[run] Searching for executable '%s'.\n" exec in
  begin match Sys.file_exists exec with
    | `Yes           -> run_process exec args
    | `No | `Unknown ->
      let () = Format.printf "%!" in
      let msg = Format.sprintf "Could not find file '%s'. Have you compiled target '%s'?" exec main_module in
      raise (Cli_constants.File_not_found msg)
  end

let command =
  Command.basic
    ~summary:"Runs a compiled executable."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The run command runs a compiled executable.";
      "Compile with the compile command, then run."
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose"   no_arg ~doc:" Print debugging information."
      +> flag ~aliases:["-r"] "-recompile" no_arg ~doc:" Compile target before running"
      +> anon ("target" %: file)
      +> anon (sequence ("args" %: string))
    )
    (fun v recompile main args () ->
      let () = if recompile then check_code (Compile.compile ~verbose:v main) in
      check_code (run ~verbose:v main args)
    )
