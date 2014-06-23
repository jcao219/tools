open Cli_constants
open Io_util
open Filepath_util

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
                  let exit_code = Build.run (strip_suffix (snd (rsplit test_name '/'))) in exit_code = 0)
        ) true test_suite in
        let () = Sys.chdir cwd in
        if all_compile then Some new_dir else None
  ) None dirs

(** [find_all_test_names t i] using compiling implementation [i], collect
 * the names of all tests in the suite [t] *)
(* TODO does not work in isolation *)
let find_all_test_names test_names impl =
  let cwd = Sys.getcwd () in
  let () = Sys.chdir impl in
  let names = List.rev (List.fold_left (fun all_names test_file_full ->
    let test_file = strip_suffix test_file_full in
    (* Run the test to print names *)
    let cmd = Format.sprintf "./_build/%s.d.byte inline-test-runner dummy -list-test-names > %s" test_file cTEST_OUTPUT in
    let _ = Sys.command cmd in
    (* Generated file contains one line per unit test *)
    let names = List.rev (List.fold_left (fun acc line ->
      let name = test_name_of_line line in
      name :: acc
    ) [] (read_lines (open_in cTEST_OUTPUT))) in
    (test_file , names) :: all_names
  ) [] test_names) in
  let () = Sys.chdir cwd in
  names

(* Determine whether a string came from [Assertions.qcheck] *)
let is_qcheck (msg : string) : bool =
  let stripped = snd (rsplit (fst (rsplit msg '(')) 'A') in
  stripped = "ssertions.QCheck_result" (* 'Qcheck_result' missing the Q *)

(* Exract the number of test failures from a qcheck result string *)
let parse_num_failed (msg : string) : int =
  (* Convert '...Assertions.Qcheck_result(9001,"heyheyhey")...' into 9001 *)
  int_of_string (fst (lsplit (snd (rsplit msg '(')) ','))

let success_message (test_name : string) : string =
  Format.sprintf "PASS -- %s" test_name

let failure_message (test_name : string) (error_message : string) : string =
  Format.sprintf "FAIL -- %s : %s" test_name error_message

(** [harness_collect_output rubric] Iterate over test results,
 * store pass/fail information in [sheet], return pretty-printed output *)
let harness_collect_output () : int list * string list =
  (* Make sure output was generated.
   * Need to manipulate these files *)
  let () = assert_file_exists cTEST_OUTPUT in
  let () = assert_file_exists cFAIL_OUTPUT in
  (* OKAY, things are a little confusing here.
   * There are 2 files of interest:
   *   [test_output], containing names of tests, and
   *   [fail_output], containing names of failed tests and error messages
   * The protocol is to
   *   1. Iterate over [fail_output], organize errors by name
   *   2. Iterate over [test_output], record whether tests passed or failed in order, pretty-print result
   * 2014-03-24: New complication! Quickcheck tests get partial credit.
   * Scan the [fail_output] for instances of [Assertions.QCheck_result]. Scrape the integer
   * argument from this constructor (it's always the first) -- that's the number of failed
   * tests. Subtract that from the total for the proper part score.
   *)
  (* Step 1: Organize error messages *)
  let errors_by_name = Hashtbl.create 27 in
  let () = List.iter (fun line ->
    let name = test_name_of_line line in
    Hashtbl.add errors_by_name name line
  ) (read_lines (open_in cFAIL_OUTPUT)) in
  (* Step 2: Collect pretty output. Returns a TUPLE. *)
  List.fold_right (fun line (ints,strs) ->
    let name = snd (rsplit line ':') in
    if Hashtbl.mem errors_by_name name then
      (* Failed, or quickcheck *)
      let err_msg = Hashtbl.find errors_by_name name in
      if is_qcheck err_msg then
        (* Is qcheck. May be success. *)
        let score = cNUM_QCHECK - parse_num_failed err_msg in
        let msg =
          if score = cNUM_QCHECK
          then success_message name
          else failure_message name err_msg
        in
        (score::ints, msg::strs)
      else
        (* Is normal. A failure *)
        let msg = failure_message name err_msg in
        (0::ints, msg::strs)
    else
      (* Passed *)
      let msg = success_message name in
      (1::ints, msg::strs)
  ) (read_lines (open_in cTEST_OUTPUT)) ([],[])

(** [harness_sanitize_fname str] Check whether the file named [str] contains
 * any unit tests. If so, prompt the user to edit the file. Re-prompt until
 * file is clean. *)
let harness_sanitize_src (fname : string) : unit =
  (* Use grep -q, which returns 0 if any matches are found *)
  let cmd = Format.sprintf "grep -q \"TEST\" %s" fname in
  let is_clean = ref (Sys.command(cmd)) in
  while (!is_clean = 0) do (
    Format.printf "\
********************************************************************************\n\
*** WARNING : file '%s' contains the string TEST. This is probably BAD!         \n\
***           Please edit the file and delete all unit tests, then press RETURN.\n\
*** Input the string \"ACCEPT\" to accept this file anyway (case-sensitive).    \n\
********************************************************************************\n\
" fname;
    if ("ACCEPT" = (read_line()))
    then is_clean := 1
    else is_clean := Sys.command(cmd);
  ()) done

(** [harness tests targets] run each set of unit tests under [tests]
 * against [targets] *)
let run (test_dir : string) (directories : string list) : unit =
  let cwd = Sys.getcwd () in
  let directories = strip_trailing_slash_all directories in
  (* [test_suite] is a list of files containing tests.
   * 2014-03-22: Maybe someday this should be a module. *)
  let test_names,test_abs_paths =
    Array.fold_right (fun fname (a1,a2) ->
      (* Check for dotfiles *)
      if String.length fname = 0 || fname.[0] = '.' then
        let () = Format.printf "WARNING: skipping empty/dotfile file in test folder '%s/%s'\n%!" test_dir fname in
        (a1, a2)
      else
        ((strip_suffix fname)::a1, (test_dir^"/"^fname)::a2)
    ) (Sys.readdir test_dir) ([],[]) in
  let good_student = match find_compiling_implementation test_abs_paths directories with
    | Some dir -> dir
    | None -> let () = Format.printf "Error: could not find compiling implementation. Cannot create spreadsheet (but I guess you don't need one).\nExiting...\n" in exit(1)
  in
  let test_names_by_file = find_all_test_names test_names good_student in
  (* Initialize spreadsheet for CMS *)
  let sheet =
    if Sys.file_exists cCMS_FNAME
    then Grades_table.init_from_file cCMS_FNAME
    else Grades_table.init test_names_by_file
  in
  (* For each implementation to test, copy in the tests, build, and run. *)
  (* Pass around the spreadsheet *)
  let sheet = List.fold_left (fun sheet dir ->
    (* Prepare for testing *)
    let netid = tag_of_path dir in
    let txt_fname = Format.sprintf "./%s/%s.md" cOUTPUT_DIR netid in
    let txt_chn = open_out txt_fname in
    (* Change into student dir for testing, print titles *)
    let () = Sys.chdir dir in
    let () = Format.printf "\n## Running tests for '%s' ##\n%!" netid in
    let () = output_string txt_chn (Format.sprintf "## Automated test results for %s ##\n" netid) in
    (* Build and run *)
    let scores_by_test = List.rev (List.fold_left (fun scores test_name ->
      (* Prepare postscript document *)
      let ps_doc =
        let fname = Format.sprintf "%s/%s/%s-%s.ps" cwd cOUTPUT_DIR netid test_name in
        let title = Format.sprintf "%s\t\t%s.ml" netid test_name in
        Postscript.init fname title
      in
      (* copy source file, print header in ps stream *)
      let _ =
        Postscript.set_font ps_doc Postscript.Header;
        (* Copy source to postscript. Obtaining source is a hack, but it's not fatal if it fails *)
        let _ =
          let src_fname = Format.sprintf "%s.ml" (fst (rsplit test_name '_')) in
          if not (Sys.file_exists src_fname) then
            Postscript.write ps_doc "SOURCE NOT FOUND\n"
          else begin
            let () =
              Sys.chdir cwd;
              harness_sanitize_src (dir^"/"^src_fname);
              Sys.chdir dir
            in
            let () = Postscript.write ps_doc (Format.sprintf "Source code for file '%s':\n" src_fname) in
            let () = Postscript.set_font ps_doc Postscript.Code in
            List.iter (fun line ->
              Postscript.write ps_doc line; Postscript.write ps_doc "\n"
            ) (read_lines (open_in src_fname))
          end
        in
        output_string txt_chn (Format.sprintf "### %s ###\n" test_name)
      in
      let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name) in
      let exit_code = Build.run test_name in
      (* collect output for printing *)
      let part_scores, output_by_line =
        if exit_code <> 0 then
          [], ["NO COMPILE"]
        else begin
          (* Run tests, organize output *)
          let _ = Test.test_logging_errors test_name in
          harness_collect_output ()
        end
      in
      (* Postscript title *)
      let () =
        Postscript.set_font ps_doc Postscript.Header;
        Postscript.write ps_doc "\nTest Results:\n";
        Postscript.set_font ps_doc Postscript.Normal
      in
      (* Print results for each test case *)
      let () =
        List.iter (fun msg ->
          print_endline msg;
          output_string txt_chn "    "; output_string txt_chn msg; output_string txt_chn "\n";
          Postscript.write ps_doc msg; Postscript.write ps_doc "\n";
        ) output_by_line;
        print_endline "";
        Postscript.write ps_doc "\n"
      in
      (* Flush and close postscript *)
      let _ =
        flush txt_chn;
        Postscript.close ps_doc
      in
      (* Remove generated files *)
      let () =
        (* 2014-01-19: Removing tests so they don't screw with reverse harness *)
        ignore(Sys.command (Format.sprintf "rm %s.ml" test_name));
        if Sys.file_exists cTEST_OUTPUT then
          ignore(Sys.command ("rm " ^ cTEST_OUTPUT));
        if Sys.file_exists cFAIL_OUTPUT then
          ignore(Sys.command ("rm " ^ cFAIL_OUTPUT));
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
  Grades_table.write sheet cCMS_FNAME
