open Filepath_util
open Process_util

let run ?(src_dir=Sys.getcwd ()) (output_dir : string) : int =
  let () = Printf.printf "Generating documentation for directory: %s\n" src_dir in
  let () = ensure_dir output_dir in
  let build_dir =
    try
      if Sys.is_directory "_build" then "_build"
      else begin
        Printf.eprintf "Please run %s before generating the documentation."
                       "\'cs3110 compile <main-module>\'";
        exit 1
      end
    with _ -> "" in
  let ocamldoc_options = [
    "-v";             (* run verbose                                     *)
    "-sort";          (* sort the output modules                         *)
    "-stars";         (* remove leading blank characters in doc comments *)
    "-warn-error";    (* treat errors as warnings                        *)
    "-html";          (* html output by default                          *)
    "-colorize-code"; (* provide syntax highlighting in the HTML         *)
    "-I";
    build_dir;        (* includes the output directory of cs3110 compile *)
    "-d";
    output_dir;       (* put output in output_dir                        *)
  ] in
  let mlis = try get_files_with_extension "mli" src_dir with _ -> [] in
  run_process "ocamldoc" (ocamldoc_options @ mlis)
