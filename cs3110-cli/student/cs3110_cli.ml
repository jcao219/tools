let expected_version = "4.01.0"
let depend_file = "./.depend"
let lib_file = "./.libs"
(* std_opam_packages may NOT be empty! Need pa_ounit, at least, to compile *)
let std_opam_packages = ["pa_ounit.syntax"; "oUnit"; "pa_ounit"; "qcheck"]
let opam_packages_file = "./.opam_packages"

exception File_not_found of string
exception Invalid_filepath of string

(* 2014-04-18: "ocamlbuild must be invoked from the root of the project"
 * http://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html *)
let assert_ocamlbuild_friendly_filepath (path : string) : unit =
  let is_relative =
    try let _ = Str.search_forward (Str.regexp "\\.\\./") path 0 in true
    with Not_found -> false
  in
  if (String.contains path '~' || path.[0] = '/' || is_relative)
  then raise (Invalid_filepath "Must call cs3110 from the project root. Absolute or relative paths are not allowed.")

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
  let () = assert_ocamlbuild_friendly_filepath main_module in
  let () = assert_file_exists (main_module ^ ".ml") in
  let target = Format.sprintf "%s.d.byte" main_module in
  let _ = print_endline "Compiling..." in
  let dependencies = 
    if Sys.file_exists depend_file
    then ["-Is"; csv_of_file depend_file]
    else [] 
  in
  let libraries = 
    if Sys.file_exists lib_file
    then ["-libs"; "assertions,"^csv_of_file lib_file]
    else ["-libs"; "assertions"] 
  in
  let all_opam_packages = std_opam_packages @ 
    if Sys.file_exists opam_packages_file
    then (read_lines (open_in opam_packages_file))
    else []
  in
  let opam_packages_str = 
      (String.concat ", " 
      (List.map (fun p -> Format.sprintf "package(%s)" p) all_opam_packages))
  in
  check_code (run_process "ocamlbuild" (dependencies @ libraries @ [
    (* "-cflag"; "-w"; "-cflag"; "A-4-33-40-41-42-43-34-44"; (* Jane street's warnings as errors *) *)
    "-cflag"; "-warn-error"; "-cflag"; "+a"; (* treat the default warnings as errors *)
    "-use-ocamlfind"; "-no-links"; 
    "-tag-line"; "<**/*.ml{,i}> : thread";
    "-tag-line"; "<**/*.ml{,i}> : syntax(bin_prot), syntax(camlp4o), " ^ opam_packages_str;
    "-tag-line"; "<**/*.d.byte> : thread, " ^ opam_packages_str;
    "-tag-line"; "<**/*.native> : thread, " ^ opam_packages_str;
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
  cs3110 inspiration      Displays an inspirational quote.
  cs3110 help             Displays this message.
"

let inspiration () =
  Inspiration.print_inspiration ()

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
    | [ _; "inspiration" ] -> inspiration ()
    | [ _; "clean" ] -> clean ()
    | [ _; "compile"; target ] -> build (strip_suffix target)
    | [ _; "test"; target ] -> 
        let target' = strip_suffix target in test target'
    | _ :: "run" :: target :: args -> 
        let target' = strip_suffix target in run target' args
    | _ -> print_endline "Invalid arguments."; help ()
  with File_not_found filename ->
    Format.printf "Could not find the file %s.\n%!" filename
