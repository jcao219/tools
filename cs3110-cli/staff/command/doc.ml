open Core.Std
open Cli_util
open Process_util

type options = Cli_config.doc_command_options

let default_ocamldoc_options = [
    "-v";             (* run verbose                                     *)
    "-sort";          (* sort the output modules                         *)
    "-stars";         (* remove leading blank characters in doc comments *)
    "-warn-error";    (* treat warnings as errors                        *)
    "-html";          (* html output by default                          *)
    "-colorize-code"; (* provide syntax highlighting in the HTML         *)
]

(** [filter_invalid_doc_targets tgts] Remove files from the list [tgts] that
    either do not exist or are not .ml or .mli files. *)
let filter_invalid_doc_targets (targets : string list) : string list =
  List.fold_right targets
    ~f:(fun tgt acc ->
          begin match Sys.file_exists tgt with
            | `No | `Unknown ->
              let () = Format.printf "[doc] WARNING skipping nonexistant file '%s'.\n" tgt in acc
            | `Yes           ->
              if (String.is_suffix tgt ~suffix:".ml") || (String.is_suffix tgt ~suffix:".mli")
              then tgt :: acc
              else let () = Format.printf "[doc] WARNING skipping curious file '%s'.\n" tgt in acc
          end)
    ~init:[]

(** [doc ?v ?r o ts] Generate ocamldoc documentation for the targets [ts].
    If [r] is true, recompile before building docs. *)
let doc ?(verbose=false) ?(recompile=false) (opts : options) (targets : string list) : int =
  let tgts = filter_invalid_doc_targets targets in
  let ()   = if verbose   then Format.printf "[doc] Generating documentation for targets: '%s'\n" (String.concat ~sep:", " tgts) in
  let ()   = if recompile then List.iter ~f:(fun t -> check_code (Compile.compile (strip_suffix t))) tgts in
  let args = default_ocamldoc_options @ ["-I"; Cli_config.cBUILD_DIRECTORY; "-d"; opts.output_directory] @ tgts in
  let ()   = if verbose   then Format.printf "[doc] Running ocamldoc with arguments '%s'.\n%!" (String.concat ~sep:" " args) in
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
      +> flag ~aliases:["-v"] "-verbose"    no_arg          ~doc:" Print debugging information."
      +> flag ~aliases:["-r"] "-recompile"  no_arg          ~doc:" Compile the target before generating documentation."
      +> flag ~aliases:["-o"] "-output-dir" (optional file) ~doc:"DIR Save outputted documentation to the directory DIR."
      +> anon (sequence  ("target" %: string))
    )
    (fun v r o ts () ->
      let cfg  = Cli_config.init () in
      let opts = ({
        output_directory = Option.value o ~default:cfg.doc.output_directory;
      } : Cli_config.doc_command_options)
      in
      let () = if not r then assert_file_exists Cli_config.cBUILD_DIRECTORY in
      let () = ensure_dir opts.output_directory in
      check_code (doc ~verbose:v ~recompile:r opts ts)
    )
