exception File_not_found of string

let expected_version = "4.01.0"
let email_subject = "[CS3110 test harness] compile error"

let depend_file = "./.depend"
let lib_file = "./.lib"
let tests_dir = "./tests"
let nocompile_dir = "./_nocompile"
let smoke_targets = "./smoke_test"

let test_output = "inline_tests.log"
let fail_output = "inline_test_failures.log"

let ps_normal_font = "Palatino-Roman10"
let ps_header_font = "Palatino-Bold10"
let ps_code_font = "Courier-New10"

let ps_set_font (chn : out_channel) (font : string) : unit =
  output_string chn (Format.sprintf "\n\001font{%s}" font)

(** [split s c b] safely splits string [s] at the left/right-most occurence
 * of character [c]. [b] chooses. If [c] does not appear, returns two 
 * copies of [s]. *)
let split (s : string) (c : char) (b : bool) : string * string = 
  if not (String.contains s c) then
    (s, s)
  else 
    let i = 1 + if b then String.index s c else String.rindex s c in
    let left = String.sub s 0 (i-1) in
    let right = String.sub s i ((String.length s) - i) in 
    (String.trim left, String.trim right)

let lsplit (s : string) (c : char) : string * string = 
  split s c true

let rsplit (s : string) (c : char) : string * string =
  split s c false

(** [strip_suffix str] strips all characters after and including the 
 * rightmost period (.) *)
let strip_suffix (filename : string) : string =
  if (String.contains filename '.')
  then String.sub filename 0 (String.rindex filename '.')
  else filename

(** [strip_trailing_slash str] removes the last character of [s],
 * but only if that character is a / *)
let strip_trailing_slash (s : string) =
  let len = String.length s in
  if s.[len-1] = '/'
  then String.sub s 0 (len - 1) 
  else s

(** [strip_trailing_slash_all strs] remove the trailing slash from a 
 * list of files *)
let strip_trailing_slash_all (directories : string list) = 
  List.map (fun s -> strip_trailing_slash s) directories

(** [tag_of_path p] strips all characters up to and including the rightmost / *)
let tag_of_path (path : string) = 
  if String.contains path '/' then 
    let i = 1 + String.rindex path '/' in
    String.sub path i ((String.length path) - i)
  else 
    path

(** [assert_file_exists f] raises [File_not_found] if [f] does not exist *)
let assert_file_exists (filename : string) : unit =
  if not (Sys.file_exists filename) then
    raise (File_not_found filename)

(** [ensure_dir d] creates the directory [d] if it does not exist already. *)
let ensure_dir (dir_name : string) =
  if not (Sys.file_exists dir_name) then 
    Unix.mkdir dir_name 0o777

(** [read_lines c] reads all lines from channel [c] into a list *)
let read_lines (chn : in_channel) : string list =
  let rec read_aux (chn : in_channel) (out : string list) : string list =
    try 
      let output' = input_line chn :: out in
      read_aux chn output'
    with End_of_file -> 
      let _ = close_in chn in 
      out
  in
  List.rev (read_aux chn [])

(** [csv_of_file filename] reads all lines in [filename] and concatenates them
 * with commas *)
let csv_of_file (filename : string) : string =
  let chn = open_in filename in
  String.concat "," (read_lines chn)

(**
 * [directories_of_list dir] Build a list of directories from the file [dir].
 * [dir] should contain a newline-separated list of directories.
 * Example: to test files { dir/a , dir/b , dir/c }, you may
 *   > cat "a\nb\nc" > dir/files
 *   > cs3110 smoke @dir/files
 * This is the same as
 *   > cs3110 smoke dir/a dir/b dir/c
 *)
let directories_of_list (dir : string) : string list = 
  let dir = strip_trailing_slash dir in
  let len = String.length dir in
  (* Remove the leading @ to get a filename *)
  let fname = String.sub dir 1 (len-1) in
  let _ = assert_file_exists fname in
  (* Argument is @path/to/list-of-directory-names.    *
   * Extract a list of directory names from the list. *)
  let dir_names = read_lines (open_in fname) in
  let prefix = String.sub fname 0 (String.rindex dir '/') in
  (* Return the fully-inferred list of directories *)
  List.rev (List.fold_left (fun acc name -> (prefix ^ name) :: acc) [] dir_names)

let return_code_of_exit_status (st : Unix.process_status) : int =
  match st with
    | Unix.WEXITED exit_code -> 
      (* let _ = Printf.printf "run_process EXIT : %s\n%!" (String.concat " "(filename :: args)) in *)
      exit_code
    | Unix.WSIGNALED n ->
      Format.printf "Sub-process killed (signal %d)\n%!" n;
      -1
    | Unix.WSTOPPED n ->
      Format.printf "Sub-process stopped (signal %d)\n%!" n;
      -1

(* Runs an executable and waits for termination, returning the exit code.
 * If the process is killed or stopped, prints a warning & returns -1. *)
let run_process (filename : string) (args : string list) : int =
  let open Unix in
  let pid = create_process filename (Array.of_list (filename :: args))
    stdin stdout stderr in
  let (_, exit_status) = waitpid [] pid in
  return_code_of_exit_status exit_status

let check_code (return_code : int) : unit =
  if return_code <> 0 then begin
    Format.printf "Error (exit code %d)\n%!" return_code;
    exit return_code
  end

(* Use ocamlbuild to clean. *)
let clean () : unit =
  check_code (Sys.command "ocamlbuild -clean")

(** [build m] compile [m] into a bytecode executable. 
 * Relies on ocamlbuild. *) 
let build (main_module : string) : int =
  assert_file_exists (main_module ^ ".ml");
  let target = Format.sprintf "%s.d.byte" main_module in
  let _ = Format.printf "Compiling '%s.ml'\n%!" main_module in
  let dependencies = 
    if Sys.file_exists depend_file
    then ["-Is"; csv_of_file depend_file]
    else [] in
  let libraries = 
    if Sys.file_exists lib_file
    then ["-libs"; "assertions"; csv_of_file lib_file]
    else ["-libs"; "assertions"] in
  run_process "ocamlbuild" (dependencies @ libraries @ [
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
  ])

(** [diff ()] if any files under the _nocompile directory match a target,       *
 * perform a diff on the pre-and post submission. Record results in a csv file. *)
let diff (directories : string list) : unit =
  let _ = assert_file_exists nocompile_dir in
  (* Space to store outputs *)
  let diff_tmp = ".diff" in
  (* maps netid -> dir *)
  let netid_map = Hashtbl.create 27 in 
  List.iter (fun dir -> 
    Hashtbl.add netid_map (tag_of_path dir) dir)
    (strip_trailing_slash_all directories);
  let num_diffed = ref 0 in
  let results_chn = open_out "_diff_results.csv" in
  (* For each file in the nocompile folder, check if it has a match in [directories] *)
  Array.iter (fun netid -> 
    if Hashtbl.mem netid_map netid then
      let dir = Hashtbl.find netid_map netid in
      (* Found a match. diff this submission *)
      let user_input = ref 2 in
      let result = Array.fold_left (fun acc fname ->
        if acc = 0 then 
          (* Already rejected student's fix for previous file.  *
           * They suffer the penalty.                           *)
          0
        else begin
          (* Display the diff, wait on a valid user input *)
          let new_file = Format.sprintf "%s/%s" dir fname in
          let cmd = Format.sprintf "diff %s %s/%s/%s" new_file nocompile_dir netid fname in
          Format.printf "\n### Executing '%s' ###\n%!" cmd;
          (* Check if file exists. Run diff *)
          let _ = if not (Sys.file_exists new_file) then
              Format.printf "File '%s' does not exist!\n%!" new_file
            else 
            (* ignore(Sys.command cmd); *)
              let _ = Sys.command (Format.sprintf "%s > %s" cmd diff_tmp) in
              List.iter print_endline (read_lines (open_in diff_tmp))
          in
          user_input := 2;
          print_string "** ";
          while (!user_input <> 0 && !user_input <> 1) do (
            print_endline "Please enter 0 or 1 as judgement. 1 is good.";
            try 
              let the_int = int_of_string (read_line ()) in
              user_input := the_int
            with Failure "int_of_string" -> 
              ()
          ) done;
          incr num_diffed;
          !user_input
        end
      ) 1 (Sys.readdir (Format.sprintf "%s/%s" nocompile_dir netid)) in
      output_string results_chn (Format.sprintf "%s,%d\n" netid result)
  ) (Sys.readdir "./_nocompile");
  (* Close up *)
  let _ = 
    ignore(Sys.command (Format.sprintf "rm -f %s" diff_tmp));
    close_out results_chn
  in
  Format.printf "Finished diffing %d files. See '_diff_results.csv' for results\n%!" !num_diffed

(** [email ()] send the email messages stored in the _email directory.  *
 * Assumes that every folder name under _email is a valid Cornell netid *)
let email () : unit = 
  assert_file_exists ("./_email");
  let messages = Sys.readdir "./_email" in
  let num_sent = ref 0 in
  let send_mail (msg_file : string) : unit = 
    let recipient = (strip_suffix msg_file) ^ "@cornell.edu" in
    let cmd = Format.sprintf "mail -s '%s' '%s' < _email/%s" email_subject recipient msg_file in
    let _ = Format.printf "*** executing '%s'\n%!" cmd in
    let exit_code = Sys.command cmd in
    if exit_code <> 0 then 
      print_endline ("Failed to send message to: " ^ recipient)
    else
      incr num_sent
  in
  Array.iter send_mail messages;
  Format.printf "Finished sending %d messages.\n%!" !num_sent

let test_parameterized (main_module : string) (output : string) : int =
  let open Unix in
  let exec = Format.sprintf "_build/%s.d.byte" main_module in
  let _ = assert_file_exists exec in
  let cmd = Format.sprintf "%s inline-test-runner dummy -log %s" exec output in
  Sys.command cmd

(* Redirects stderr to stdout's destination, changes stdout's 
 * destination to null, saves the error messages to a file *)
let test_logging_errors (main_module :string) : int =
  (* 2014-01-11: Steals only the first line of output for each error *)
  let dest = "2>& 1>/dev/null | grep '^File' > " ^ fail_output in
  test_parameterized main_module dest

let test_quiet (main_module : string) =
  test_parameterized main_module "> /dev/null"

(** [test file] executes the unit tests within [file].
 * Inner workings documented here: [https://github.com/janestreet/pa_ounit]
 * Standard output is ignored; exceptions still print.
 * This is the default and preferred way of running test. *)
let test (main_module : string) : int =
  test_quiet main_module

(** [harness_collect_output tbl] reads the generated files for
 * test output and test failures and organizes the results *)
let harness_collect_output () : string list =
  let _ = assert_file_exists test_output in
  let _ = assert_file_exists fail_output in
  (* Collect test names *)
  let test_names = List.fold_left (fun acc line -> 
    let name = snd (rsplit line ':') in
    name :: acc) [] (read_lines (open_in test_output)) 
  in
  (* Organize error messages *)
  let errors_by_name = Hashtbl.create 27 in
  let _ = List.iter (fun line -> 
    let name = fst (lsplit (snd (rsplit line ':')) ' ') in 
    Hashtbl.add errors_by_name name line
  ) (read_lines (open_in fail_output)) in
  (* Generate final list of output. Test name + pass/fail message *)
  List.fold_left (fun acc name ->
    let msg = if Hashtbl.mem errors_by_name name then
        (* a fail-er *)
        let err = Hashtbl.find errors_by_name name in
        Format.sprintf "FAIL -- %s : %s" name err
      else 
        (* a passer *)
        Format.sprintf "PASS -- %s" name
    in msg :: acc) [] test_names

(** [harness tests targets] run each set of unit tests under [tests] 
 * against [targets] *)
let harness (test_dir : string) (directories : string list) : unit =
  assert_file_exists test_dir;
  ensure_dir "./_output";
  (* 2014-01-12: dammit, appears to have introduced errors *)
  let cwd = Sys.getcwd () in
  let directories = strip_trailing_slash_all directories in
  (* [test_suite] is a list of files containing tests. *)
  let test_suite = Array.fold_right (fun f acc -> 
    ((strip_suffix f)) :: acc) (Sys.readdir test_dir) [] in
  (* For each implementation to test, copy in the tests, build, and run. *)
  let _ = List.iter (fun dir -> 
    (* Prepare for testing *)
    let netid = tag_of_path dir in
    let txt_chn = open_out (Format.sprintf "./_output/%s.md" netid) in
    let _ = Sys.chdir dir in
    (* Print titles *)
    let _ = 
      Format.printf "\n## Running tests for '%s' ##\n%!" netid;
      output_string txt_chn (Format.sprintf "## Automated test results for %s ##\n" netid)
    in
    (* Build and run *)
    let _ = List.iter (fun test_name ->
      (* Prepare postscript document *)
      let ps_chn =
        let title = fst (rsplit test_name '_') in
        Unix.open_process_out (Format.sprintf "enscript --quiet -p %s/_output/%s-%s.ps -b '%s\t\t%s.ml' -M Letter --fancy-header --escapes=\001 --no-formfeed" cwd netid title netid title)
      in
      (* copy source file, print header in ps stream *)
      let _ =
        ps_set_font ps_chn ps_header_font;
        let src_fname = Format.sprintf "%s.ml" (fst(rsplit test_name '_')) in
        let _ = if not (Sys.file_exists src_fname) then
            output_string ps_chn "SOURCE NOT FOUND\n"
          else begin
            ps_set_font ps_chn ps_code_font;
            List.iter (fun line ->
              output_string ps_chn line; output_string ps_chn "\n"
            ) (read_lines (open_in src_fname))
          end
        in
        flush ps_chn;
        output_string txt_chn (Format.sprintf "### %s ###\n" test_name)
      in
      let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name) in
      let exit_code = build test_name in
      let output_by_line = 
        if exit_code <> 0 then 
          ["NO COMPILE"]
        else begin
          (* Run tests, organize output *)
          let exit_code = test_logging_errors test_name in
          if exit_code <> 0
          then ["TIMEOUT"]
          else harness_collect_output ()
        end
      in
      (* Postscript title *)
      let _ =
        ps_set_font ps_chn ps_header_font;
        output_string ps_chn "\nTest Results:\n";
        ps_set_font ps_chn ps_normal_font
      in
      (* Print results for each test case *)
      let _ = List.iter (fun msg -> 
        print_endline msg; 
        output_string txt_chn "    "; output_string txt_chn msg; output_string txt_chn "\n";
        output_string ps_chn msg; output_string ps_chn "\n";
      ) output_by_line; print_endline ""; output_string ps_chn "\n"
      in
      (* Flush and close postscript *)
      let _ = 
        flush ps_chn;
        flush txt_chn;
        Unix.close_process_out ps_chn
      in
      (* Remove generated files *)
      let _ = 
        (* 2014-01-12: Leave the tests be. Do not remove them *)
        (* ignore(Sys.command (Format.sprintf "rm %s.ml" test_name)); *)
        if Sys.file_exists test_output then 
          ignore(Sys.command ("rm " ^ test_output));
        if Sys.file_exists fail_output then 
          ignore(Sys.command ("rm " ^ fail_output));
        ()
      in
      ()
    ) test_suite in
    (* Finished with [dir]. Clean up, move out. *)
    let _ = 
      close_out txt_chn;
      Sys.chdir cwd 
    in
    ()
  ) directories in
  ()

(** [run file args] run the executable generated by [cs3110 compile file] *)
let run (main_module : string) (args : string list) : int =
  let cmd = Format.sprintf "_build/%s.d.byte" main_module in
  assert_file_exists cmd;
  run_process cmd args

(** [smoke_compile_one ms d] compile each module in the list [ms] under the 
 * containing directory [d]. Generate emails and save the files for failures. *)
let smoke_compile_one (targets : string list) (dir_name : string) : unit =
  let _ = Format.printf "\n## Smoke target '%s' ##\n" (tag_of_path dir_name) in
  let failed_targets : string list ref = ref [] in
  let compile_and_record (target : string) : unit = 
    (* 2014-01-09: [build] directs compiler output to stdout/stderr.  *
     * Could redirect this to the email.                              *)
    let fname = Format.sprintf "%s.ml" target in
    if not (Sys.file_exists fname) then 
      let _ = Format.printf "'%s' not found\n%!" fname in 
      failed_targets := target :: !failed_targets
    else if build target <> 0 then
      failed_targets := target :: !failed_targets
  in
  let cwd = Sys.getcwd () in
  let _ = Sys.chdir dir_name in
  List.iter compile_and_record targets;
  let _ = Sys.chdir cwd in
  (* If there were failures, record an email message *)
  match !failed_targets with 
    | [] -> ()
    | h::t -> 
      let name = tag_of_path dir_name in 
      (* Write the email message *)
      (* 2014-01-09: Sorry, I'd love to save the email message at the *
       * top of this file, but that's not allowed for format strings. *)
      let message = Format.sprintf "Dear %s, \n\
\n\
The following files from your CMS submission were either missing or failed to compile via `cs3110 compile` \n\
* %s.ml\n\n\
Please update your submission on CMS so that `cs3110 compile <file>` succeeds for each of the above files. If the required changes were small, you will not be charged a late/slip day. If the changes were non-trivial, you will lose a slip day if you have any remaining or the late penalty. \n\
Good luck!\n\
\n\
--- Automatically generated message from the CS3110 test harness ---\n\
" name (String.concat ".ml\n* " (List.rev !failed_targets)) in
      let email_chn = open_out (Format.sprintf "./_email/%s.txt" name) in
      output_string email_chn message;
      close_out email_chn;
      (* Save each failing source file *)
      let nocompile_dir = Format.sprintf "./_nocompile/%s" name in
      ensure_dir nocompile_dir;
      let copy_file (target : string) =
        let fname = Format.sprintf "%s/%s.ml" dir_name target in
        (* Either copy the existing source code or initialize an empty file. 
         * The empty file will give an 100% diff later, whereas no file would
         * raise an error. *)
        let exit_code = 
          if not (Sys.file_exists fname) 
          then Sys.command (Format.sprintf "touch %s/%s.ml" nocompile_dir target)
          else Sys.command (Format.sprintf "cp %s %s" fname nocompile_dir)
        in
        if exit_code <> 0 
        then Format.printf "ERROR: Failed to save file %s/%s.ml\n" dir_name target
      in
      List.iter copy_file !failed_targets

(** 
 * [smoke dirs] Attempt to compile each target in each directory of [dirs]. 
 * Failures are recorded in email messages and the files are saved under the 
 * ./_nocompile folder
 *)
let smoke (directories : string list) : unit =
  let directories = strip_trailing_slash_all directories in
  (* setup *)
  let _ = 
    ensure_dir "_email";
    ensure_dir "_nocompile";
  in
  (* Try to infer targets from test names. Assuming all tests are 
   * of form 'file_test.ml' *)
  let targets = 
    if Sys.file_exists smoke_targets
    then List.map strip_suffix (read_lines (open_in smoke_targets))
    else if Sys.file_exists tests_dir
    then Array.fold_right (fun f acc -> 
      ((fst(rsplit f '_'))) :: acc) (Sys.readdir tests_dir) [] 
    else raise (File_not_found smoke_targets)
  in
  (* Compile the targets in one directory. Accumulate an email message  *
   * outlining the failures.                                            *)
  List.iter (smoke_compile_one targets) directories

let help () =
  print_string "\
Usage: cs3110 COMMMAND [args]

  cs3110 compile <file>      Compile file.ml.
  cs3110 diff <targets>      Compare supplied files with those stored as no compiles
  cs3110 email               Send the emails generated by the smoke test
  cs3110 run <file>          Run the program file.ml.
  cs3110 test <file>         Run the tests in file.ml.
  cs3110 smoke <test_dir>    Compile all targets.
  cs3110 harness <test_dir>  Run the tests in the directory ./tests against all targets.
  cs3110 clean               Removes files created by 'cs3110 compile'.
  cs3110 help                Displays this message.
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
    | [ _; "compile"; target ] -> check_code (build (strip_suffix target))
    |  _ :: "diff" :: arg1 :: args -> 
      if arg1.[0] = '@'
      then diff (directories_of_list arg1)
      else diff (arg1 :: args)
    | [ _; "email" ] -> email ()
    | _ :: "harness" :: arg1 :: args -> 
      (* Make sure test dir exists *)
      let _ = assert_file_exists tests_dir in
      (* convert to absolute path *)
      let abs_dir = Format.sprintf "%s/%s" (Sys.getcwd ()) tests_dir in
      (* Run harness *)
      if arg1.[0] = '@'
      then harness abs_dir (directories_of_list arg1)
      else harness abs_dir (arg1 :: args)
    | _ :: "run" :: target :: args -> 
        let target' = strip_suffix target in check_code (run target' args)
    | _ :: "smoke" :: arg1 :: args -> 
      if arg1.[0] = '@'
      then smoke (directories_of_list arg1)
      else smoke (arg1 :: args)
    | [ _; "test"; target ] -> 
        let target' = strip_suffix target in check_code (test target')
    | _ -> print_endline "Invalid arguments."; help ()
  with File_not_found filename ->
    Format.printf "Could not find the file %s.\n%!" filename
