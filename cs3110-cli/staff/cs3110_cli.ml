(**** exceptions and constants ************************************************)
exception File_not_found of string

let expected_version = "4.01.0"
let email_subject = "[CS3110 test harness] compile error"
let email_admins = "_email_bcc.txt"

let cms_dir = "./_cms"
let cms_fname = Format.sprintf "%s/spreadsheet.csv" cms_dir
let depend_file = "./.depend"
let diff_results = "_diff_results.csv"
let email_dir = "./_email"
let lib_file = "./.libs"
let nocompile_dir = "./_nocompile"
let opam_packages_file = "./.opam_packages"
let output_dir = "./_output"
let smoke_targets = "./smoke_test"
(* std_opam_packages may NOT be empty! Need pa_ounit, at least, to compile *)
let std_opam_packages = ["pa_ounit.syntax"; "oUnit"; "pa_ounit"; "qcheck"]
let tests_dir = "./tests"

let test_output = "inline_tests.log"
let fail_output = "inline_test_failures.log"

(**** postscipt utilities *****************************************************)
let ps_normal_font = "Palatino-Roman10"
let ps_header_font = "Palatino-Bold10"
let ps_code_font = "Courier-New10"

(** [ps_set_font c f] Resets the font of text output to channel [c] *)
let ps_set_font (chn : out_channel) (font : string) : unit =
  output_string chn (Format.sprintf "\n\001font{%s}" font)

(** [ps_open_channel f t] opens a stream to file [f] with title [t]
 * that pipes its input into a postscript-formatted file *)
let ps_open_channel (fname : string) (title : string) : out_channel =
  Unix.open_process_out (Format.sprintf "enscript --quiet -p %s -b '%s' -M Letter --fancy-header --escapes=\001 --no-formfeed" fname title)

(**** i/o utilities ***********************************************************)

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

(**** string/filepath utilities ***********************************************)
(** [absolute_path f] Prepend the current working directory to
 * the filepath [f] *)
let absolute_path (f : string) =
  Format.sprintf "%s/%s" (Sys.getcwd ()) f

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

let rec starts_with_aux (s: string) (p : string) (i : int) =
  if i >= String.length p then true
  else if i >= String.length s then false
  else s.[i] = p.[i] && starts_with_aux s p (i + 1)
(** [starts_with s p] checks if the string [p] matches the leading characters of [s] *)
let starts_with (s : string) (p : string) = 
  starts_with_aux s p 0

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

(** [test_name_of_line s] extract the test name from a line printed by the 
 * inline test runner. Name should be the last 'word' of the string, separated
 * from everything else by a colon *)
let test_name_of_line (line : string) : string =
  fst (lsplit (snd (rsplit line ':')) ' ')

(**** process utilities *******************************************************)

(** [return_code_of_exit_status st] Convert the process_status into an int *)
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

(** [run_process f args] Runs an executable and waits for termination, returning 
 * an integer exit code.
 * If the process is killed or stopped, prints a warning & returns code -1. *)
let run_process (filename : string) (args : string list) : int =
  let open Unix in
  let pid = create_process filename (Array.of_list (filename :: args))
    stdin stdout stderr in
  let (_, exit_status) = waitpid [] pid in
  return_code_of_exit_status exit_status

(** [check_code c] exit uncleanly if [c] indicates an error *)
let check_code (return_code : int) : unit =
  if return_code <> 0 then begin
    Format.printf "Error (exit code %d)\n%!" return_code;
    exit return_code
  end

(* Use OCAMLRUNPARAM to enable stack traces, unless you've set your own. *)
let config_env () =
  try
    let _ = Unix.getenv "OCAMLRUNPARAM" in
    ()
  with
  | Not_found -> Unix.putenv "OCAMLRUNPARAM" "b"

(**** spreadsheet *********************************************************)

module Grades_table : sig 
  type t

  val add_row : t -> string -> (string * int list) list -> t

  val init : (string * string list) list -> t
  val init_from_file : string -> t

  (* [close ()] prints and closes the spreadsheet *) 
  val write : t -> string -> unit

end = struct 

  (* NetID, scores *)
  type row = string * int list

  module S = Set.Make(struct 
    type t = row
    let compare (id1,_) (id2,_) = Pervasives.compare id1 id2
  end)

  type t = { 
    test_cases_by_file : (string * (string list)) list; 
    data : S.t; 
  }

  let cSEPARATOR = ","

  (* [add_row t netId scores_by_test] ALMOST adds data directly to the sheet.
   * There is one preprocessing step: converted the bucketed [scores_by_test]
   * to scores and totals *)
  let add_row t netId scores_by_test =
    (* Currently ignore name. We can improve this *)
    let case_scores =
      List.fold_right (fun (test_name,scores) all_scores ->
        match scores with
          | [] -> (* No compile *)
            print_endline "HAHAHHAHAHAHAHAAHAHAHA";
            (* Find the number of test cases *)
            let cases = List.fold_left (fun acc (name, cases) ->
              if test_name = name then cases else acc) [] t.test_cases_by_file
            in
            print_endline ("NUM CASES = " ^ string_of_int (List.length cases));
            (* Padding *)
            (List.fold_left (fun acc _ -> 0 :: acc) all_scores cases)
          | h::t -> 
            (scores @ all_scores)
      ) scores_by_test []
    in
    { t with data = S.add (netId,case_scores) t.data }

  (* [init fname names_by_file] create a new spreadsheet named [fname]
   * columns are:
   * - one for each test case
   * - totals for each test file *)
  let init names_by_file : t = 
    {
      test_cases_by_file = names_by_file;
      data = S.empty;
    }

  (* Parse exisiting spreadsheet for column names and values *)
  let init_from_file (fname : string) : t =
    (* First collect names, then iteratively add rows *)
    failwith "init from file NOT IMPLEMENTED"

  (* Print the spreadsheet *)
  let write t filename =
    let chn = open_out filename in
    (* Print header *)
    let () = output_string chn "NetID" in
    (* Print test case names *)
    let () =
      List.iter (fun (_,cases) -> 
        List.iter (fun name ->
          output_string chn (cSEPARATOR ^ name)
        ) cases
      ) t.test_cases_by_file
    in
    (* Print totals. These are formatted for CMS *)
    let () = 
      List.iter (fun (test_name,_) ->
        output_string chn (cSEPARATOR ^ (String.capitalize (fst (lsplit test_name '_'))))
      ) t.test_cases_by_file
    in
    let () = output_string chn (cSEPARATOR ^ "Total\n") in
    (* Print data *)
    let () = 
      S.iter (fun (id,rs) ->
        let results_str = String.concat cSEPARATOR (List.map string_of_int rs) in
        output_string chn ((String.concat cSEPARATOR [id; results_str]) ^ "\n")
      ) t.data
    in
    ignore (close_out chn)

end

(**** cs3110 commands *********************************************************)

(** [clean ()] remove all files generated during compilation *)
let clean () : unit =
  check_code (Sys.command "ocamlbuild -clean")

(** [build] compile [m] into a bytecode executable. 
 * Relies on ocamlbuild. TODO quiet version? *) 
let build (main_module : string) : int =
  assert_file_exists (main_module ^ ".ml");
  let target = Format.sprintf "%s.d.byte" main_module in
  let _ = Format.printf "Compiling '%s.ml'\n%!" main_module in
  let dependencies = 
    if Sys.file_exists depend_file
    then ["-Is"; csv_of_file depend_file]
    else []
  in
  let libraries = 
    if Sys.file_exists lib_file
    then ["-libs"; "assertions," ^ csv_of_file lib_file]
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
  run_process "ocamlbuild" (dependencies @ libraries @ [
    (* "-cflag"; "-w"; "-cflag"; "A-4-33-40-41-42-43-34-44"; (* Jane street's warnings as errors *) *)
    "-cflag"; "-warn-error"; "-cflag"; "+a"; (* treat the default warnings as errors *)
    "-use-ocamlfind"; "-no-links"; 
    "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), " ^ opam_packages_str;
    "-tag-line"; "<*.d.byte> : " ^ opam_packages_str;
    "-tag-line"; "<*.native> : " ^ opam_packages_str;
    target
  ])

(** [get_extension file_name] gets the extension of the file
    [file_name]. The extension is defined to be the characters
    occuring to the right of the right-most occurence of the '.'
    character. *)
let get_extension (file_name : string) : string option =
  try 
    let start  = String.rindex file_name '.' + 1 in
    let length = String.length file_name - start in
    Some (String.sub file_name start length)
  with
    Not_found          -> None
  | Invalid_argument _ -> failwith "get_extension : invalid file extension."

(** The [do_if_directory] function is used to process directories. So
    [do_if_directory dir f err_msg ~exit_code:n] executes the process
    [f] if [dir] is a valid directory and prints the error message
    [err_msg] to standard out and exits the current process with exit
    code [n] if not. The default exit code is 1. *)
let do_if_directory (dir : string)
                    (f : string -> 'a)
                    (err_msg : string)
                    (exit_code : int) : 'a =
  if Sys.file_exists dir && Sys.is_directory dir then f dir
  else prerr_endline err_msg; exit exit_code 

(** [filter_by_extension desired_extension files] returns a list of
    the files in [files] that have the desired extension. *)
let filter_by_extension (desired_extension : string)
                        (files : string list) : string list =
  let has_desired_extension file = match get_extension file with
    | None -> false
    | Some ext -> ext = desired_extension in
  List.filter has_desired_extension files

(** [get_files_with_extension] returns a list containing all of the
    filenames in the given directory that have the desired extension. *)
let get_files_with_extension (desired_extension : string)
                             (dir : string) : string list =
  let get_files dir =
    filter_by_extension desired_extension (Array.to_list (Sys.readdir dir)) in
  try get_files dir with _ -> []

let doc ?(src_dir=Sys.getcwd ()) (output_dir : string) : int =
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

(** [diff ()] if any files under the [nocompile_dir] match a target,
 * perform a diff on the pre-and post submission. Record results in a csv file. *)
let diff (directories : string list) : unit =
  let _ = assert_file_exists nocompile_dir in
  (* Space to store outputs *)
  let diff_tmp = ".diff" in
  (* maps netid -> dir, to make it easy to get new input from the 
  * name of a previous no-compile *)
  let netid_map = Hashtbl.create 27 in 
  let _ = List.iter (fun dir -> 
    Hashtbl.add netid_map (tag_of_path dir) dir)
    (strip_trailing_slash_all directories) in
  let num_diffed = ref 0 in
  let results_chn = open_out diff_results in
  (* For each file in the nocompile folder, check if it has a match in [directories] *)
  let all_nocompiles= Sys.readdir "./_nocompile" in
  let () = Array.sort Pervasives.compare all_nocompiles in
  Array.iter (fun netid -> 
    if Hashtbl.mem netid_map netid then
      let dir = Hashtbl.find netid_map netid in
      (* Found a match. diff this submission *)
      let user_input = ref 2 in
      let result = Array.fold_left (fun acc fname ->
        if acc = 0 then 
          (* Already rejected student's fix for previous file. 
           * They suffer the penalty. *)
          0
        else begin
          (* Run a diff, ask user for judgment. 
           * Automatically handles missing files and 'no difference' resubmits *)
          let new_file = Format.sprintf "%s/%s" dir fname in
          let old_file = Format.sprintf "%s/%s/%s" nocompile_dir netid fname in
          let simple_cmd = Format.sprintf "diff -q %s %s" old_file new_file in
          let pretty_cmd = 
            let diff_cmd = if (Sys.command "which colordiff > /dev/null" = 0) 
              then "colordiff"
              else "diff" in 
            Format.sprintf "%s -u %s %s | less -R" diff_cmd old_file new_file in
          let _ = Format.printf "\n### Executing '%s' \n%!" simple_cmd in
          if not (Sys.file_exists new_file) then
            (* Resubmission does not contain a file. 
             * Return 1 because they do not have a late submission *)
            let _ = Format.printf "File '%s' does not exist!\n%!" new_file in
            1
          else begin
            (* First, check if there are any differences *)
            if (Sys.command simple_cmd) = 0 then
              (* No differences. Print and return 1. It's 1 
               * because they did not submit late *)
              let _ = Format.printf "diff of '%s/%s' finished with no differences\n%!" netid fname in
              1
            (* Next, check if either file was empty *)
            else if (Sys.command (Format.sprintf "test -s %s" old_file)) <> 0 then
              (* Empty old file! That's a slip day *)
              0
            else
              (* There are differences. Display a prettier diff, wait for user
               * response *)
              let show_diff = fun () -> Sys.command pretty_cmd in
              let () = (* set up repl *)
                ignore (show_diff ());
                user_input := 2
              in
              (* Collect input *)
              let () = while (!user_input <> 0 && !user_input <> 1) do (
                print_string "#### Decision time! Choose one of (y/n/d/s) or type 'h' for help.\n> ";
                match String.lowercase (read_line ()) with
                | "y" | "yes" -> user_input := 1
                | "n" | "no"  -> user_input := 0
                | "d"         -> ignore (show_diff ())
                | "s"         -> ignore (Sys.command(Format.sprintf "less %s %s" old_file new_file))
                | str         -> if str <> "h" then print_endline "#### Invalid option"; print_endline "#### Choices are:\n       y : Accept the diff, no penalty\n       n : Reject the diff. Will need to deduct slip day on CMS\n       d : Show the diff again\n       s : Show the source files (first old, then new)\n" 
              ) done in
              (* done with repl *)
              let () = 
                print_endline "Ok! ";
                incr num_diffed
              in
              !user_input
          end
        end 
      (* btw, initial accumulator for the fold is 1 because of the guard on 0 
       * If [acc = 0], we stop doing diffs for the student. *)
      ) 1 (Sys.readdir (Format.sprintf "%s/%s" nocompile_dir netid)) in
      (* Save the results to the .csv *)
      output_string results_chn (Format.sprintf "%s,%d\n" netid result)
  ) all_nocompiles;
  (* Close up *)
  let _ = 
    ignore(Sys.command (Format.sprintf "rm -f %s" diff_tmp));
    close_out results_chn
  in
  Format.printf "Finished inputting decisions for %d files. See '%s' for results\n%!" (!num_diffed) diff_results

(** [email ()] send the email messages stored in the _email directory.  *
 * Assumes that every folder name under _email is a valid Cornell netid *)
let email () : unit = 
  let _ = 
    assert_file_exists email_dir;
    assert_file_exists email_admins;
  in
  let messages = Sys.readdir email_dir in
  let num_sent = ref 0 in
  (* Use the [mail] command to package off the email message *)
  Array.iter (fun (msg_file : string) ->
    let recipient = (strip_suffix msg_file) ^ "@cornell.edu" in
    let bcc = Format.sprintf "-b '%s'" (String.concat "' -b '" (read_lines (open_in email_admins))) in
    let cmd = Format.sprintf "mutt -s '%s' %s '%s' < %s/%s" email_subject bcc recipient email_dir msg_file in
    let _ = Format.printf "### Executing '%s'\n%!" cmd in
    let exit_code = Sys.command cmd in
    if exit_code <> 0 then 
      print_endline ("Failed to send message to: " ^ recipient)
    else
      incr num_sent
  ) messages;
  Format.printf "Finished sending %d messages.\n%!" !num_sent

(* The general form of the test command *)
let test_parameterized (main_module : string) (output : string) : int =
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

(** [harness_collect_output rubric] Iterate over test results,
 * store pass/fail information in [sheet], return pretty-printed output *)
let harness_collect_output () : int list * string list =
  (* Make sure output was generated. 
   * Need to manipulate these files *)
  let () = assert_file_exists test_output in
  let () = assert_file_exists fail_output in
  (* OKAY, things are a little confusing here. 
   * There are 2 files of interest: 
   *   [test_output], containing names of tests, and 
   *   [fail_output], containing names of failed tests and error messages
   * The protocol is to 
   *   1. Iterate over [fail_output], organize errors by name
   *   2. Iterate over [test_output], record whether tests passed or failed in order, pretty-print result
   *)
  (* Step 1: Organize error messages *)
  let errors_by_name = Hashtbl.create 27 in
  let () = List.iter (fun line -> 
    let name = test_name_of_line line in
    Hashtbl.add errors_by_name name line
  ) (read_lines (open_in fail_output)) in
  (* Step 2: Collect pretty output. Returns a TUPLE. *)
  List.fold_right (fun line (ints,strs) ->
    let name = snd (rsplit line ':') in
    if Hashtbl.mem errors_by_name name then
      (* Failed *)
      let err_msg = Hashtbl.find errors_by_name name in
      let msg = Format.sprintf "FAIL -- %s : %s" name err_msg in
      (0::ints, msg::strs)
    else 
      (* Passed *)
      let msg = Format.sprintf "PASS -- %s" name in
      (1::ints, msg::strs)
  ) (read_lines (open_in test_output)) ([],[])

(** [harness_sanitize_fname str] Check whether the file named [str] contains
 * any unit tests. If so, prompt the user to edit the file. Re-prompt until
 * file is clean. *)
let harness_sanitize_src (fname : string) : unit =
  (* Use grep -q, which returns 0 if any matches are found *)
  let cmd = Format.sprintf "grep -q \"TEST\" %s" fname in
  let clean = ref (Sys.command(cmd)) in
  while (!clean = 0) do (
    Format.printf "\
********************************************************************************\n\
*** WARNING : file '%s' contains the string TEST. This is probably BAD!         \n\
***           Please edit the file and delete all unit tests, then press RETURN.\n\
*** Input the string \"ACCEPT\" to accept this file anyway (case-sensitive).    \n\
********************************************************************************\n\
" fname;
    if ("ACCEPT" = (read_line()))
    then clean := 1
    else clean := Sys.command(cmd);
  ()) done

(* Search all directories for one that passes all tests *)
let find_compiling_implementation test_suite dirs : string option = 
  let cwd = Sys.getcwd () in
  List.fold_left (fun dir_opt new_dir ->
    match dir_opt with 
      | Some _ -> dir_opt (* already found a good student, skip rest of directories *)
      | None -> (* Go to student's folder, copy in the tests, compile each, check output *)
        let () = Sys.chdir new_dir in
          (* Copy the tests over, compile  each *) 
        let all_compile = List.fold_left (fun acc test_name ->
          acc && (let _ = Sys.command (Format.sprintf "cp %s ." test_name) in
                  (* TODO build is failing *)
                  let exit_code = build (strip_suffix (snd (rsplit test_name '/'))) in exit_code = 0)
        ) true test_suite in
        let () = Sys.chdir cwd in
        if all_compile then Some new_dir else None
  ) None dirs

(* [find_all_test_names t i] using compiling implementation [i], collect 
 * the names of all tests in the suite [t] *)
(* TODO does not work in isolation *)
let find_all_test_names test_names impl = 
  let cwd = Sys.getcwd () in
  let () = Sys.chdir impl in
  let names = List.rev (List.fold_left (fun all_names test_file_full ->
    let test_file = strip_suffix test_file_full in
    (* Run the test to print names *)
    let cmd = Format.sprintf "./_build/%s.d.byte inline-test-runner dummy -list-test-names > %s" test_file test_output in
    let _ = Sys.command cmd in
    (* Generated file contains one line per unit test *)
    let names = List.rev (List.fold_left (fun acc line ->
      let name = test_name_of_line line in
      name :: acc
    ) [] (read_lines (open_in test_output))) in
    (test_file , names) :: all_names
  ) [] test_names) in
  let () = Sys.chdir cwd in
  names

(** [harness tests targets] run each set of unit tests under [tests] 
 * against [targets] *)
let harness (test_dir : string) (directories : string list) : unit =
  let cwd = Sys.getcwd () in
  let directories = strip_trailing_slash_all directories in
  (* [test_suite] is a list of files containing tests. 
   * 2014-03-22: Maybe someday this should be a module. *)
  let test_names,test_abs_paths = 
    Array.fold_right (fun fname (a1,a2) -> 
      ((strip_suffix fname)::a1, (test_dir^"/"^fname)::a2)
    ) (Sys.readdir test_dir) ([],[]) in
  let good_student = match find_compiling_implementation test_abs_paths directories with 
    | Some dir -> dir 
    | None -> let () = Format.printf "Error: could not find compiling implementation. Cannot create spreadsheet (but I guess you don't need one).\nExiting...\n" in exit(1)
  in
  let test_names_by_file = find_all_test_names test_names good_student in
  (* Initialize spreadsheet for CMS *)
  let sheet = 
    if Sys.file_exists cms_fname
    then Grades_table.init_from_file cms_fname
    else Grades_table.init test_names_by_file
  in
  (* For each implementation to test, copy in the tests, build, and run. *)
  (* Pass around the spreadsheet *)
  let sheet = List.fold_left (fun sheet dir -> 
    (* Prepare for testing *)
    let netid = tag_of_path dir in
    let txt_fname = Format.sprintf "./%s/%s.md" output_dir netid in
    let txt_chn = open_out txt_fname in
    (* Change into student dir for testing, print titles *)
    let () = Sys.chdir dir in
    let () = Format.printf "\n## Running tests for '%s' ##\n%!" netid in
    let () = output_string txt_chn (Format.sprintf "## Automated test results for %s ##\n" netid) in
    (* Build and run *)
    let scores_by_test = List.rev (List.fold_left (fun scores test_name ->
      (* Prepare postscript document *)
      let ps_chn =
        let fname = Format.sprintf "%s/%s/%s-%s.ps" cwd output_dir netid test_name in
        let title = Format.sprintf "%s\t\t%s.ml" netid test_name in
        ps_open_channel fname title
      in
      (* copy source file, print header in ps stream *)
      let _ =
        ps_set_font ps_chn ps_header_font;
        (* Copy source to postscript. Obtaining source is a hack, but it's not fatal if it fails *)
        let _ = 
          let src_fname = Format.sprintf "%s.ml" (fst (rsplit test_name '_')) in
          if not (Sys.file_exists src_fname) then
            output_string ps_chn "SOURCE NOT FOUND\n"
          else begin
            let () = 
              Sys.chdir cwd;
              harness_sanitize_src (dir^"/"^src_fname);
              Sys.chdir dir
            in
            output_string ps_chn (Format.sprintf "Source code for file '%s':\n" src_fname);
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
      (* collect output for printing *)
      let part_scores, output_by_line =
        if exit_code <> 0 then 
          [], ["NO COMPILE"]
        else begin
          (* Run tests, organize output *)
          let _ = test_logging_errors test_name in
          harness_collect_output ()
        end
      in
      (* Postscript title *)
      let () =
        ps_set_font ps_chn ps_header_font;
        output_string ps_chn "\nTest Results:\n";
        ps_set_font ps_chn ps_normal_font
      in
      (* Print results for each test case *)
      let () = 
        List.iter (fun msg -> 
          print_endline msg; 
          output_string txt_chn "    "; output_string txt_chn msg; output_string txt_chn "\n";
          output_string ps_chn msg; output_string ps_chn "\n";
        ) output_by_line;
        print_endline ""; 
        output_string ps_chn "\n"
      in
      (* Flush and close postscript *)
      let _ = 
        flush ps_chn;
        flush txt_chn;
        Unix.close_process_out ps_chn
      in
      (* Remove generated files *)
      let () = 
        (* 2014-01-19: Removing tests so they don't screw with reverse harness *)
        ignore(Sys.command (Format.sprintf "rm %s.ml" test_name));
        if Sys.file_exists test_output then 
          ignore(Sys.command ("rm " ^ test_output));
        if Sys.file_exists fail_output then 
          ignore(Sys.command ("rm " ^ fail_output));
        ()
      in
      (test_name, part_scores) :: scores
    ) [] test_names) in
    (* Finished with one student. *)
    let () = close_out txt_chn in
    let () = Sys.chdir cwd in
    (* Carry new sheet onto next iteration *)
    Grades_table.add_row sheet netid scores_by_test
  ) sheet directories in
  Grades_table.write sheet cms_fname

(** [run file args] run the executable generated by [cs3110 compile file] *)
let run (main_module : string) (args : string list) : int =
  let cmd = Format.sprintf "_build/%s.d.byte" main_module in
  assert_file_exists cmd;
  run_process cmd args

(** [smoke_compile_one ms d] compile each module in the list [ms] under the 
 * containing directory [d]. Generate emails and save the files for failures. *)
let smoke_compile_one (targets : string list) (dir_name : string) : unit =
  let () = Format.printf "\n## Smoke target '%s' ##\n" (tag_of_path dir_name) in
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
    ensure_dir email_dir;
    ensure_dir nocompile_dir
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

(** [stat fname] read in the test results from [fname], create 
 * a mapping of test names to #passes/#failures, print results
 * as a dump and as histograms *)
let stat (sheet_name : string) : unit =
  let () = assert_file_exists sheet_name in
  (* Table of testname -> (numpass, numfail) *)
  let results_by_test = Hashtbl.create 19 in
  (* Separately track nocompiles *)
  let nocompile = ref 0 in
  (* Open the file, start reading lines. This works very naively.
   * Searches for 3 desired patterns, ignores all other lines *)
  let chn = open_in sheet_name in
  let line = ref "" in
  try
    while (true) do (
      line := String.trim (input_line chn);
      if (starts_with (!line) "NO COMPILE") then
        (* Failed to compile, update the nocompile count *)
        incr(nocompile)
      else if (starts_with (!line) "PASS") then begin
        (* Test passes! *)
        let test_name = fst(rsplit (snd(rsplit (!line) '-')) '"') in
        (if not (Hashtbl.mem results_by_test test_name) 
         then Hashtbl.add results_by_test test_name (ref 0, ref 0));
        incr(fst(Hashtbl.find results_by_test test_name))
      end
      else if (starts_with (!line) "FAIL") then begin
        (* Very messy extracting the test name *)
        let test_name = fst(lsplit (String.sub (!line) 8 (String.length (!line) - 8)) ':') in
        (if not (Hashtbl.mem results_by_test test_name) 
         then Hashtbl.add results_by_test test_name (ref 0, ref 0));
        incr(snd(Hashtbl.find results_by_test test_name))
      end
      else (* Nothing happens. Ignore line. *) ();
      ()) done
  with End_of_file -> 
      (* Routine finished, print results *)
    let () = close_in chn in 
    (* Save longest string for pretty-printing *)
    let hfill_ref = ref 0 in
    (* Convert Hashtbl into list to get alphabetical results *)
    let rs = Hashtbl.fold (fun name (pass,fail) xs -> 
      let () = hfill_ref := max (!hfill_ref) (String.length name) in
      (name,!pass,!fail) :: xs) results_by_test [] in
    let hfill = !hfill_ref in
    let rs = List.sort (fun (a,_,_) (b,_,_) -> Pervasives.compare a b) rs in
      (* First, print raw counts *)
    let () = Format.printf "## TEST HARNESS STATISTICS\n\n" in
    let () = Format.printf "### Raw numbers\n" in
    let () = List.iter (fun (n,p,f) -> Format.printf "    %s%s\t%d pass\t%d fail\n" n (String.make (hfill - String.length n) ' ') p f) rs in
      (* Passing histogram *)
    let () = Format.printf "\n### Successes by test\n" in
    let () = List.iter (fun (n,p,_) -> Format.printf "    %s%s\t%s\n" n (String.make (hfill - String.length n) ' ') (String.make p 'x')) rs in
    (* Failing histogram *)
    let () = Format.printf "\n### Failures by test\n" in
    let () = List.iter (fun (n,_,f) -> Format.printf "    %s%s\t%s\n" n (String.make (hfill - String.length n) ' ') (String.make f 'x')) rs in
    (* Lastly, print nocompiles *)
    Format.printf "\n### %d students failed to compile\n" (!nocompile)

let help () =
  print_string "\
Usage: cs3110-staff COMMMAND [args]

  cs3110 clean                         Removes files created by 'cs3110 compile'
  cs3110 compile <file>                Compile file.ml.
  cs3110 diff <targets>                Compare supplied files with those stored
                                        as no compiles.
  cs3110 doc <output_dir> [src_dir]    Generates the ocamldoc documentation for
                                       the .mli files in [src_dir] and dumps
                                       output to <output_dir>. Default src_dir
                                       is the current working directory.
  cs3110 email                         Send the emails generated by the smoke
                                        test.
  cs3110 harness <targets>             Run the tests in the directory ./tests
                                        against all targets.
  cs3110 help                          Displays this message.
  cs3110 run <file>                    Run the program file.ml.
  cs3110 stat <spreadsheet>            Compute statistics given a harness-generated spreadsheet 
  cs3110 smoke <targets>               Compile all targets.
  cs3110 test <file>                   Run the tests in file.ml.
"

(**** main function ***********************************************************)

let () = 
  let _ = 
    config_env ();
  in
  try
    match Array.to_list Sys.argv with
    | [ _; "help" ]  -> help ()
    | [ _; "clean" ] -> clean ()
    | [ _; "compile"; target ] -> check_code (build (strip_suffix target))
    |  _ :: "diff" :: arg1 :: args -> 
      if arg1.[0] = '@'
      then diff (directories_of_list arg1)
      else diff (arg1 :: args)
    | [ _; "doc"; output_dir; src_dir] ->
      check_code (doc ~src_dir:src_dir output_dir)
    | [ _; "doc"; output_dir] ->
      check_code (doc output_dir)
    | [ _; "email" ] -> email ()
    | _ :: "harness" :: arg1 :: args -> 
      (* Make sure test dir exists *)
      let () = assert_file_exists tests_dir in
      (* Ensure output directories *)
      let () = ensure_dir cms_dir in
      let () = ensure_dir output_dir in
      (* convert to absolute path *)
      let abs_dir = absolute_path tests_dir in 
      (* Run harness *)
      if arg1.[0] = '@'
      then harness abs_dir (directories_of_list arg1)
      else harness abs_dir (arg1 :: args)
    | _ :: "run" :: target :: args -> 
        let target' = strip_suffix target in 
        check_code (run target' args)
    | _ :: "smoke" :: arg1 :: args -> 
      if arg1.[0] = '@'
      then smoke (directories_of_list arg1)
      else smoke (arg1 :: args)
    | [ _; "stat"; sheet ] ->
      stat sheet
    | [ _; "test"; target ] -> 
        let target' = strip_suffix target in check_code (test target')
    | _ -> print_endline "Invalid arguments."; help ()
  with File_not_found filename ->
    Format.printf "Could not find the file %s.\n%!" filename
