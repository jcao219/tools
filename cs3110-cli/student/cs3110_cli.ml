let expected_version = "4.01.0"
let depend_file = "./.depend"
let lib_file = "./.libs"

exception File_not_found of string

(* Remove the trailing '.ml' (or whatever else) from the filename *)
let strip_suffix (filename : string) : string =
  if (String.contains filename '.')
  then String.sub filename 0 (String.rindex filename '.')
  else filename

let assert_file_exists (filename : string) : unit =
  if not (Sys.file_exists filename) then
    raise (File_not_found filename)

let read_lines (chn : in_channel) : string list =
  let rec read_aux (chn : in_channel) (out : string list) : string list =
    try 
      let output' = input_line chn :: out in
      read_aux chn output'
    with End_of_file -> out
  in
  read_aux chn []

(* [csv_of_file filename] reads all lines in [filename] and concatenates them
 * with commas *)
let csv_of_file (filename : string) : string =
  let chn = open_in filename in
  String.concat "," (read_lines chn)

(* Runs an executable and waits for termination, returning the exit code.
 * If the process is killed or stopped, prints a warning & returns -1. *)
let run_process (filename : string) (args : string list) : int =
  let open Unix in
  let pid = create_process filename (Array.of_list (filename :: args))
    stdin stdout stderr in
  let (_, exit_status) = Unix.waitpid [] pid in
  match exit_status with
    | WEXITED exit_code -> exit_code
    | WSIGNALED n ->
        Format.printf "Sub-process killed (signal %d)\n%!" n;
        -1
    | WSTOPPED n ->
        Format.printf "Sub-process stopped (signal %d)\n%!" n;
        -1

let check_code (return_code : int) : unit =
  if return_code <> 0 then begin
    Format.printf "Error (exit code %d)\n%!" return_code;
    exit return_code
  end

(* Use ocamlbuild to clean. *)
let clean () : unit =
  check_code (Sys.command "ocamlbuild -clean")

(* Uses the oUnit and cs3110 packages and the oUnit syntax extension. *)
let build (main_module : string) : unit =
  assert_file_exists (main_module ^ ".ml");
  let target = Format.sprintf "%s.d.byte" main_module in
  let _ = print_endline "Compiling..." in
  let dependencies = 
    if Sys.file_exists depend_file
    then ["-Is"; csv_of_file depend_file]
    else [] in
  let libraries = 
    if Sys.file_exists lib_file
    then ["-libs"; "assertions,"^csv_of_file lib_file]
    else ["-libs"; "assertions"] in
  check_code (run_process "ocamlbuild" (dependencies @ libraries @ [
    "-cflag"; "-warn-error"; "-cflag"; "+a"; (* treat the default warnings as errors *)
    "-use-ocamlfind"; "-no-links"; 
    "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), \
                               package(pa_ounit.syntax), \
                               package(oUnit)";
    "-tag-line"; "<*.d.byte> : package(pa_ounit), \
                               package(oUnit)";
    "-tag-line"; "<*.native> : package(pa_ounit), \
                               package(oUnit)";
    target
  ]))

(* Runs unit tests using the oUnit syntax extension's test runner. *)
let test (main_module : string) : unit =
  let cmd = Format.sprintf "_build/%s.d.byte" main_module in
  assert_file_exists cmd;
  check_code (Sys.command (Format.sprintf
    "%s inline-test-runner dummy" cmd))

let run (main_module : string) (args : string list) =
  let cmd = Format.sprintf "_build/%s.d.byte" main_module in
  assert_file_exists cmd;
  check_code (run_process cmd args)

let help () =
  print_string "\
Usage: cs3110 COMMMAND [args]

  cs3110 compile <file>   Compile file.ml.
  cs3110 run <file>       Run the program file.ml.
  cs3110 test <file>      Run the tests in file.ml.
  cs3110 clean            Removes files created by 'cs3110 compile'.
  cs3110 help             Displays this message.
"

(* Use OCAMLRUNPARAM to enable stack traces, unless you've set your own. *)
let config_env () =
  try
    let _ = Unix.getenv "OCAMLRUNPARAM" in
    ()
  with
  | Not_found -> Unix.putenv "OCAMLRUNPARAM" "b"

let () = 
  config_env ();
  try
    match Array.to_list Sys.argv with
    | [ _; "help" ]  -> help ()
    | [ _; "clean" ] -> clean ()
    | [ _; "compile"; target ] -> build (strip_suffix target)
    | [ _; "test"; target ] -> 
        let target' = strip_suffix target in test target'
    | _ :: "run" :: target :: args -> 
        let target' = strip_suffix target in run target' args
    | _ -> print_endline "Invalid arguments."; help ()
  with File_not_found filename ->
    Format.printf "Could not find the file %s.\n%!" filename
