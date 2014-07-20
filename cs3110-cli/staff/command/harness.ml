open Core.Std
open Cli_constants
open Io_util
open Filepath_util

(* aka 'ordered list' *)
(* TODO remove this *)
module StringSet = Set.Make(struct
  type t = string
  let compare = Pervasives.compare
end)

type test_file = {
  absolute_path : string;
  name          : string;
  unit_tests    : StringSet.t;
}

(* for organizing test files. keep tests alphabetized by filename *)
module TestSuite = Set.Make(struct
  type t            = test_file
  let compare t1 t2 = Pervasives.compare t1.name t2.name
end)

(* the results of running one test file. Keep unit test names alphabetized. *)
module TestResults = Set.Make(struct (* TODO perhaps the name should be TestFile.unittest *)
  type t                        = string * int * (string option) (* unit_test_name , points_earned , error_message *)
  let compare (n1,_,_) (n2,_,_) = Pervasives.compare n1 n2
end)

(* TODO all these options in all these commands should match the options in the config file *)
type options = {
  fail_output : string;
  num_quickcheck        : int;
  test_suite            : TestSuite.t;
  release_directory     : string;
  output_directory      : string;
  postscript            : bool;
  spreadsheet_location  : string;
  verbose               : bool;
}

(** [success_message t] Printed when submission passes test [t]. *)
let success_message (test_name : string) : string =
  Format.sprintf "PASS -- %s" test_name

(** [failure_message t e] Printed when submission fails test [t] with error [e]. *)
let failure_message (test_name : string) (error_message : string) : string =
  Format.sprintf "FAIL -- %s : %s" test_name error_message

(** [string_of_test_results tr] Pretty-print a batch of test results. Simple
    set to string conversion. *)
let string_of_test_results (results : TestResults.t) : string =
  String.concat ~sep:"\n"
    (TestResults.fold_right
       ~f:(fun (unit_name, _, err_msg) acc ->
           begin match err_msg with
             | Some e -> failure_message unit_name e
             | None   -> success_message unit_name
           end :: acc)
       ~init:[]
       results)

(* Determine whether a string came from [Assertions.qcheck] *)
let is_qcheck (msg : string) : bool =
  let stripped = snd (rsplit (fst (rsplit msg '(')) 'A') in
  stripped = "ssertions.QCheck_result" (* very janky *)

(* Extract the number of test failures from a qcheck result string *)
let parse_num_failed (msg : string) : int =
  (* Convert '...Assertions.Qcheck_result(9001,"heyheyhey")...' into 9001 *)
  int_of_string (fst (lsplit (snd (rsplit msg '(')) ','))

(** [sanitize_file f] Check if the file [f] contains the word TEST. Ask
    user to remove all occurrences or approve the file as-is. You'd want
    to approve if TEST was in a comment or part of a variable name like
    [cTESTING_MY_BOUNDARIES]. *)
let sanitize_file (fname : string) : unit =
  (* Use [grep -q], which returns 0 if any matches are found *)
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

(** [sanitize_directory d] Check all [.ml] files in directory [d]
    for unit tests. If any, ask the user to remove them before continuing. *)
let sanitize_directory (dir : string) : unit =
  let ml_files = Array.filter (String.is_suffix ~suffix:".ml") (Sys.readdir dir) in
  Array.iter ~f:sanitize_file ml_files

(** [pre_harness o d] Prepare the directory [d] for testing. *)
let pre_harness (opts : options) (dir : string) : unit =
  let () = if opts.verbose then Format.printf "[harness] Preparing directory '%s' for testing.\n" in
  let () = if opts.verbose then Format.printf "[harness] Checking for unit tests in submission files...\n" in
  let () = sanitize_directory dir in
  let () = if opts.verbose then Format.printf "[harness] Copying release files...\n" in
  let () = ignore (soft_copy opts.release_directory dir) in
  let () = if opts.verbose then Format.printf "[harness] Preparation complete!\n" in
  ()

(** [harness_run_test t d] Run test [t] on the submission in dir [d]. Collect
    the results of each unit test. *)
let harness_run_test ~output ~dir (test_file : string) : TestResults.t =
  let test_name = filename_of_path test_file in
  let ()        = ignore (Sys.command (Format.sprintf "cp %s %s" test_file dir)) in
  let ()        = Test.test ~quiet:true ~output:output ~dir:dir test_name in
  (* TODO reeeeeed! *)
  (* For each unit test, check if failed or not, save results in a TestResults entry *)
  (* List.fold *)
  (*   ~f:() *)
  (*   ~init:TestResults.empty *)
  (*   (In_channel.read_lines (Format.sprintf "%s/%s" dir output)) *)
  failwith "you jell"

(** [harness_student] Run all tests in the harness on student [dir].
    The list of results will have have element per test file. Sort results
    alphabetically by test file name. *)
let harness_student (opts : options) (dir : string) : (string * TestResults.t) list =
  let results =
    TestSuite.fold opts.test_suite
      ~f:(fun acc test_file -> (test_file, harness_run_test test_file ~dir:dir ~output:opts.fail_output) :: acc)
      ~init:[]
  in
  List.sort results
    ~cmp:(fun (s1,_) (s2,_) -> Pervasives.compare s1 s2)

(** [write_comments o d tr] Create a markdown file summarizing the test results [tr] for
    directory [d]. These comments can be uploaded directly to CMS. *)
let write_comments (opts : options) (dir : string) (tr : (string * TestResults.t) list) : unit =
  let netid = filename_of_path dir in
  let fname = Format.sprintf "%s/%s.md" opts.output_directory netid in
  let title = Format.sprintf "## Automated test results for '%s' ##" netid in
  let body  =
    List.fold_right
      ~f:(fun (test_file, results) acc ->
          let title = Format.sprintf "### %s ###\n%s" test_file in
          let body  = string_of_test_results results in
          title :: body :: acc)
      ~init:[]
      tr
  in
  Out_channel.write_lines fname (title::body)

(** [write_postscript o d tr] Create a postscript file summarizing the test results [tr] for
    directory [d]. Useful for on-paper grading. *)
let write_postscript (opts : options) (dir : string) (results : (string * TestResults.t) list) : unit =
  let netid = filename_of_path dir in
  List.iter
    ~f:(fun (test_name, results) ->
        (* collect data *)
        let fname = Format.sprintf "%s/%s-%s.ps" opts.output_directory netid test_name in
        let title = Format.sprintf "%s\t\t%s.ml" netid                 test_name in
        let src   = Format.sprintf "%s/%s.ml"    dir                   (fst (rsplit test_name '_')) in
        let body  = string_of_test_results results in
        (* write postscript *)
        let chn   = Postscript.init          fname title in
        let ()    = Postscript.write_code    chn src in
        let ()    = Postscript.write_results chn body in
        let ()    = Postscript.close         chn in
        ()
       )
    results

(** [post_harness o d] Clean up the directory [d] after testing.
    Remove generated files/logs, clean. *)
let post_harness (opts : options) ~dir (tr : (string * TestResults.t) list) : unit =
  (* TODO remove _test files? *)
  let () = if opts.verbose then Format.printf "[harness] Aggregating results for '%s'.\n" dir in
  let () = write_comments opts dir tr in
  let () = if opts.postscript then write_postscript opts dir tr in
  let () = if opts.verbose then Format.printf "[harness] Cleaning directory '%s'.\n" dir in
  let () = Clean.clean ~dir:dir ["compile"; "test"] in
  let () = if opts.verbose then Format.printf "[harness] Cleaning complete!\n" dir in
  ()

(** [harness o submissions] Initialize a spreadsheet with unit test names as columns.
    Iterate through students, filling out the sheet. *)
let harness (opts : options) (subs : string list) : unit =
  let () = if opts.verbose then Format.printf "[harness] Initializing spreadsheet...\n" in
  (* define spreadsheet *)
  let num_test_files = TestSuite.length opts.test_suite in
  let num_unit_tests = TestSuite.fold_right
                         ~f:(fun acc t -> StringSet.length t.unit_tests :: acc)
                         ~init:[]
                         opts.test_suite
  in
  let module HarnessSpreadsheet =
    (* TODO abstract these files. Parameter is just the test suite. *)
    Spreadsheet.Make(struct
      type row              = string * ((string * TestResults.t) list)
      let compare_row r1 r2 = Pervasives.compare (fst r1) (fst r2)     (* compare netids *)
      let filename : string = opts.spreadsheet_location
      let row_of_string str = failwith "cannot read sheets yet"
        (* (\* string SHOULD be netid, unit_test_scores *\) *)
        (* let columns = String.split str ~on:',' in *)
        (* let netid   = List.hd columns in *)
        (* let all_results = *)
        (*   List.rev (List.fold *)
        (*     ~f:(fun (acc,files_left,unit_tests_left) v -> *)
        (*         (\* is files_left negative? If so, parse error: too many columns *\) *)
        (*         let v = int_of_string v in *)
        (*         let file_index = num_test_files - files_left in *)
        (*         let unit_index = (List.nth_exn num_unit_tests file_index) - unit_tests_left in *)
        (*         let unit_name  = (\* get unit test name from opts.test_suite *\) in *)
        (*         (\* create a new acc with same str, unit_name + v added *\) *)
        (*         (\* check if any unit_tests_left. If so, pass acc *\) *)
        (*         (\* if not, check if any files left *\) *)
        (*        ) *)
        (*     ~init:([],num_test_files,num_unit_tests) *)
        (*     (List.tl columns)) *)
        (* in *)
        (* (netid, all_results) *)
      let string_of_row r =
        Format.sprintf "%s,%s" (fst r)
          (String.concat ~sep:",,"
                         (List.map
                                 (snd r)
                                 ~f:(fun (_,results) ->
                                     String.concat ~sep:"," (TestResults.fold_right
                                       results
                                       ~f:(fun (_,score) acc -> string_of_int score :: acc)
                                       ~init:[])
                            ) ))
      let title : string =
        (* For each test file in the suite, concat all unit test names *)
        Format.sprintf "NetID,%s"
          (String.concat ~sep:",,"
            (TestSuite.fold_right opts.test_suite
              ~init:[]
              ~f:(fun test acc -> (String.concat ~sep:"," (StringSet.to_list test.unit_tests)) :: acc)
            )
          )
    end)
  in
  let () = if opts.verbose then Format.printf "[harness] Running all tests...\n" in
  List.fold
    ~f:(fun sheet dir ->
       let ()          = pre_harness opts dir in
       let netid       = filename_of_path dir in
       let all_results = harness_student opts dir in
       let ()          = post_harness opts ~dir:dir all_results in
       let row         = (netid, all_results) in
       HarnessSpreadsheet.add_row sheet row
       )
    ~init:(HarnessSpreadsheet.create ())
    subs

(** [get_unit_test_names d t] Extract the names of all unit tests from the
    file [test] by compiling it in directory [d]. Raise an error if the file
    [test] does not exist.
    Relies on the file 'inline_tests.log' automatically generated when running
    giving pa_ounit the [-log] option. *)
let get_unit_test_names ~staging_dir ~test_name (test_abs_path : string) : StringSet.t =
  begin match Sys.file_exists test with
    | `No | `Unknown ->
      let msg = Format.printf "[harness] ERROR could not find test file '%s'. Shutting down...\n" test_abs_path in
      raise (File_not_found msg)
    | `Yes           ->
      (* Copy test into dir, compile test, run to get names out. *)
      let ()  = ignore (Sys.command (Format.sprintf "cp %s %s" test_abs_path staging_dir)) in
      let ()  = Test.test ~quiet:true ~dir:staging_dir test_name in
      let raw = In_channel.read_lines (Format.sprintf "%s/%s" staging_dir cTEST_OUTPUT) in
      let ()  = Clean.clean ~dir:staging_dir ["compile"; "test"] in
      StringSet.of_list (List.map ~f:test_name_of_line raw)
  end

(** [test_suite_of_list ts] Convert a list of relative paths to tests [ts] into a test suite.
    The test suite is a more convenient representation. *)
let test_suite_of_list ~staging_dir (tests : string list) : TestSuite.t =
  List.fold_right
    ~f:(fun path suite ->
        let () = assert_file_exists path in (* hmmm, the error raised here may not be clear. *)
        let name = strip_suffix (filename_of_path path) in
        let t = {
          absolute_path = path;
          name          = name;
          unit_tests    = get_unit_test_names ~staging_dir:staging_dir ~test_name:name path;
        } in
        TestSuite.add suite t)
    ~init:TestSuite.empty
    tests

let command =
  Command.basic
    ~summary:"Run a test harness on a list of submission folders."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The harness command runs a collection of tests against a list of submission directories.";
      "By default, the tests are inferred from the configuration, but may be given explicitly";
      "through command-line options. Output is generated for each submission directory, detailing";
      "the tests passed and failed by each submission. By default, we save a spreadsheet and";
      "markdown (.md) comments for upload to CMS. Additionally, you can generate postscript files";
      "containing students code and test results in a printer-ready format."
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"]          "-verbose"     (no_arg)          ~doc:" Print debugging information."
      +> flag ~aliases:["-p"; "-ps"]   "-postscript"  (no_arg)          ~doc:" Generate postscript output."
      +> flag ~aliases:["-t"]          "-test"        (listed file)     ~doc:"FILE Use the unit tests in the module FILE."
      +> flag ~aliases:["-r"]          "-release"     (required file)   ~doc:"DIR Release directory. Used to get starter code and unit test names."
      +> flag ~aliases:["-n"]          "-num-qcheck"  (optional int)    ~doc:(Format.sprintf "INT Set the number of quickcheck tests to run (default = %d)" cNUM_QCHECK)
      +> flag ~aliases:["-d"]          "-directory"   (optional file)   ~doc:"DIR Use all unit tests in all modules under directory DIR."
      +> flag ~aliases:["-o"]          "-output"      (optional string) ~doc:"DIR Set the output directory."
      +> flag ~aliases:["-s";"-sheet"] "-spreadsheet" (optional string) ~doc:"FILE Location to write the spreadsheet."
      +> anon (sequence ("submission" %: string))
    )
    (fun v ps tests release_dir qc test_dir output_dir sheet_location subs () ->
      let () = if v then Format.printf "[harness] Parsing options...\n" in
      let tests_dir = Optional.value test_dir ~default:cTESTS_DIR in
      let opts = {
        fail_output          = cFAIL_OUTPUT;
        num_qcheck           = qc;
        output_directory     = Optional.value output_dir ~default:cHARNESS_DIR;
        postscript           = ps;
        relase_directory     = release_dir;
        spreadsheet_location = Optional.value sheet_location ~default:cHARNESS_SHEET;
        test_suite           = test_suite_of_list ~staging_dir:release_dir (ts @ test_list_of_directory ~verbose:v tests_dir);
        verbose              = v;
      } in
      harness opts (at_expand subs))
