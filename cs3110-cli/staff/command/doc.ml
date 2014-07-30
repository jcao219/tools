open Core.Std
open Cli_util
open Process_util

type options = {
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

(** [doc ?r o ts] Generate ocamldoc documentation for the targets [ts].
    If [r] is true, recompile before building docs. *)
let doc ?(recompile=false) (opts : Cli_config.doc_command_options) (targets : string list) : int =
  let opts = config.compile in
  let tgts = List.fold_right targets
               ~f:(fun tgt acc ->
                    if (String.is_suffix tgt ~suffix:".ml") || (String.is_suffix tgt ~suffix:".mli")
                    then tgt :: acc
                    else let () = Format.printf "[doc] WARNING skipping curious file '%s'.\n" tgt in acc)
               ~init:[]
  in
  let ()   = if opts.verbose   then Format.printf "[doc] Generating documentation for targets: '%s'\n" (String.concat ~sep:", " tgts) in
  let ()   = if opts.recompile then List.iter ~f:(fun t -> check_code (Compile.compile (strip_suffix t))) tgts in
  let args = cOCAMLDOC_OPTIONS @ ["-I"; opts.build_dir; "-d"; opts.output] @ tgts in
  let ()   = if opts.verbose   then Format.printf "[doc] Running ocamldoc with arguments '%s'.\n%!" (String.concat ~sep:" " args) in
  run_process "ocamldoc" args

let command =
  Command.basic
    ~summary:"Generate ocamldoc documentation."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "Running [cs3110 doc target] will generate the ocamldoc docmentation";
      "for the file [target]. The target could be an ml module (.ml) or an";
      "ml interface (.mli) or a list of modules and interfaces. The target(s)";
      "must be compiled before docs can be generated.";
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose"    no_arg ~doc:" Print debugging information."
      +> flag ~aliases:["-r"] "-recompile"  no_arg ~doc:" Compile the target before generating documentation."
      +> flag ~aliases:["-o"] "-output-dir" (optional file) ~doc:"DIR Save outputted documentation to the directory DIR."
      +> anon (sequence  ("target" %: string))
    )
    (fun v r o ts () ->
      let cfg  = Cli_config.init () in
      let opts = {
        output    = Option.value o ~default:cDOC_OUTPUT;
        recompile = r;
        verbose   = v;
      } in
      let () = ensure_dir opts.build_dir in
      let () = ensure_dir opts.output in
      check_code (doc opts ts))
