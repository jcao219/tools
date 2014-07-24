open Core.Std
open Filepath_util
open Process_util

type options = {
  build_dir : string;
  output    : string;
  recompile : bool;
  verbose   : bool;
}

(* TODO replace with config file, ensure directories nicer *)
let cDOC_OUTPUT = "./_doc"
let cOCAMLDOC_OPTIONS = [
    "-v";             (* run verbose                                     *)
    "-sort";          (* sort the output modules                         *)
    "-stars";         (* remove leading blank characters in doc comments *)
    "-warn-error";    (* treat warnings as errors                        *)
    "-html";          (* html output by default                          *)
    "-colorize-code"; (* provide syntax highlighting in the HTML         *)
]

(** [doc o ts] Generate ocamldoc documentation for the targets [ts]. *)
let doc (opts : options) (targets : string list) : int =
  let tgts = List.map ~f:ensure_ml targets in
  let ()   = if opts.verbose   then Format.printf "[doc] Generating documentation for targets: '%s'\n" (String.concat ~sep:", " tgts) in
  let ()   = if opts.recompile then List.iter ~f:(fun t -> check_code (Compile.compile (strip_suffix t))) tgts in
  let args = cOCAMLDOC_OPTIONS @ ["-I"; opts.build_dir; "-d"; opts.output] @ tgts in
  let ()   = if opts.verbose   then Format.printf "[doc] Running ocamldoc with arguments '%s'.\n" (String.concat ~sep:" " args) in
  run_process "ocamldoc" args

let command =
  Command.basic
    ~summary:"Generate ocamldoc documentation."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "Running [cs3110 doc target] will generate the ocamldoc docmentation";
      "for the source file [target]. The target(s) must be compiled first.";
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose"    no_arg ~doc:" Print debugging information."
      +> flag ~aliases:["-r"] "-recompile"  no_arg ~doc:" Compile the target before generating documentation."
      +> flag ~aliases:["-o"] "-output-dir" (optional file) ~doc:"DIR Save outputted documentation to the directory DIR."
      +> anon (sequence  ("target" %: string))
    )
    (fun v r o ts () ->
      let opts = {
        build_dir = "_build"; (* TODO replace with a constant *)
        output    = Option.value o ~default:cDOC_OUTPUT;
        recompile = r;
        verbose   = v;
      } in
      let () = ensure_dir opts.build_dir in
      let () = ensure_dir opts.output in
      check_code (doc opts ts))
