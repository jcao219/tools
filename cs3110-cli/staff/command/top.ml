open Core.Std
open Filepath_util
open Process_util

let top ?(verbose=false) ?(utop=false) (main : string) : unit =
  let main = strip_suffix main in
  let top  = if utop then "utop" else "ocaml" in
  let cmd  = failwith "nope" in
  ignore (Sys.command cmd)

let command =
  Command.basic
    ~summary:"Load a file and its dependencies into the ocaml toplevel"
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "TODO: HOW DOES IT WORK?";
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose"   no_arg ~doc:" Print debugging information."
      +> flag                 "-utop"      no_arg ~doc:" Open [utop] instead of the standard toplevel."
      +> flag ~aliases:["-r"] "-recompile" no_arg ~doc:" Compile target before attempting to open toplevel."
      +> anon ("target" %: file)
    )
    (fun v utop recompile main () ->
      let () = if recompile then check_code (Compile.compile ~verbose:v main) in
      top ~verbose:v ~utop:utop main
    )
