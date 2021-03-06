(** 2014-07-30: Warning: the harness options are a little different from [Cli_config.harness_command_options].
    This is because the harness uses a type [TestSuite.t] that is defined in this module rather than
    in [Cli_config]. Now you know. *)

open Core.Std
open Cli_util
open Process_util

(* aka 'ordered list', used only to keep unit test names ordered within a test file. *)
module UnittestSet = StringSet

type test_file = {
  absolute_path : string;        (* Exact path to the test file. Ends with the filename. *)
  name          : string;        (* Short name of the test file. *)
  unit_tests    : UnittestSet.t; (* Names of the unit tests within the file *)
}

(* For organizing test files. Keep tests alphabetized by filename. *)
module TestFileSet = Set.Make(struct
  type t            = test_file
  let compare t1 t2 = Pervasives.compare t1.name t2.name
  let sexp_of_t _   = failwith "not implemented"
  let t_of_sexp _   = failwith "not implemented"
end)

type unittest_result = {
  unittest_name : string;
  points_earned : int;
  error_message : string option;
}
(* The results of running one test file. It's a set to keep unit test names alphabetized. *)
module TestFileResult = Set.Make(struct
  type t            = unittest_result
  let compare r1 r2 = Pervasives.compare r1.unittest_name r2.unittest_name
  let sexp_of_t _   = failwith "not implemented"
  let t_of_sexp _   = failwith "not implemented"
end)

module TestFileResultSet = Set.Make(struct
  type t                    = string * TestFileResult.t
  let compare (n1,_) (n2,_) = Pervasives.compare n1 n2
  let sexp_of_t _           = failwith "not implemented"
  let t_of_sexp _           = failwith "not implemented"
end)

(* 2014-07-30 The [test_suite] field subsumes the [tests_directory] field in [Cli_config]. *)
type options = {
  input_directory         : string;
  output_directory        : string;
  output_spreadsheet      : string;
  postscript              : bool;
  quickcheck_count        : int;
  temporary_failures_file : string;
  test_suite              : TestFileSet.t;
}

(** [success_message t] Printed when submission passes test [t]. *)
let success_message (test_name : string) : string =
  Format.sprintf "PASS -- %s" test_name

(** [failure_message t e] Printed when submission fails test [t] with error [e]. *)
let failure_message (test_name : string) (error_message : string) : string =
  let safe_err = String.tr ~target:'"' ~replacement:'\'' error_message in
  Format.sprintf "FAIL -- %s : %s" test_name safe_err

(** [string_of_test_results tr] Pretty-print a batch of test results. Simple
    set-to-string conversion. *)
let string_of_test_results (results : TestFileResult.t) : string =
  String.concat ~sep:"\n"
    (TestFileResult.fold_right
       ~f:(fun r acc ->
           begin match r.error_message with
             | None   -> success_message r.unittest_name
             | Some e -> failure_message r.unittest_name e
           end :: acc)
       ~init:[]
       results)

(** [get_scores tr] Get just the scores (in order) from the test results [t]. *)
let get_scores (tr : TestFileResult.t) : int list =
  TestFileResult.fold_right tr
    ~f:(fun r acc -> r.points_earned :: acc)
    ~init:[]

(** [is_dotfile fname] true if the file [fn] is a dotfile (aka begins with a '.' character) *)
let is_dotfile (fname : string) : bool =
  String.length fname = 0 || (* Filter nonsense *)
  fname.[0] = '.'

(** [is_valid_test_file fn] true if filename matches the expected format for tests, false otherwise *)
let is_valid_test_file (fname : string) : bool =
  (not (is_dotfile fname)) &&
  String.is_suffix fname ~suffix:"_test.ml"

(** [is_valid_submission fn] true if filename is a '.ml' but doesn't match format for tests *)
let is_valid_submission (fname : string) : bool =
  (not (is_dotfile fname))             &&
  String.is_suffix fname ~suffix:".ml" &&
  (not (is_valid_test_file fname))

(** [unittest_name_of_line s] extract the test name from a line printed by the
    inline test runner. Name should be the last 'word' of the string, separated
    from everything else by a colon *)
let unittest_name_of_line (line : string) : string =
  let pattern = Str.regexp "^File \".*\", line [0-9]+, characters [0-9]+-[0-9]+: \\([^ ]*\\).*$" in
  try
    let () = ignore (Str.search_forward pattern line 0) in
    Str.matched_group 1 line
  with _ -> failwith (Format.sprintf "SERIOUS ERROR: Harness.unittest_name_of_line couldn't parse line '%s' of failures file." line)

(** [sanitize_file ?v f] Check if the file [f] contains the word TEST. Ask
    user to remove all occurrences or approve the file as-is. You'd want
    to approve if TEST was in a comment or part of a variable name like
    [cTESTING_MY_BOUNDARIES]. *)
let sanitize_file ?(verbose=false) (fname : string) : unit =
  let () = if verbose then Format.printf "[harness] Sanitizing file '%s'\n%!" fname in
  (* Use [grep -q], which returns 0 if any matches are found *)
  let cmd = Format.sprintf "grep -q \"TEST\" %s" fname in
  let not_clean = ref (0 = (Sys.command cmd)) in
  while (!not_clean) do (
    let () = Format.printf "\
********************************************************************************\n\
*** WARNING : file '%s' contains the string TEST. This is probably BAD!         \n\
***           Please edit the file and delete all unit tests, then press RETURN.\n\
*** Input the string \"ACCEPT\" to accept this file anyway (case-sensitive).    \n\
********************************************************************************\n\
" fname in
    if ("ACCEPT" = (read_line()))
    then not_clean := false
    else not_clean := (0 = (Sys.command cmd))
  ) done

(** [sanitize_directory ?v d] Check all [.ml] files in directory [d]
    for unit tests. If any, ask the user to remove them before continuing. *)
let sanitize_directory ?(verbose=false) (dir : string) : unit =
  let ml_filenames = filter_directory ~f:is_valid_submission  dir in
  let all_paths    = List.map ~f:(fun nm -> Format.sprintf "%s/%s" dir nm) ml_filenames in
  List.iter ~f:(sanitize_file ~verbose) all_paths

(** [pre_harness ?v o d] Prepare the directory [d] for testing. *)
let pre_harness ?(verbose=false) (opts : options) (dir : string) : unit =
  let () = if verbose then Format.printf "[harness] Preparing directory '%s' for testing.\n" dir in
  let () = if verbose then Format.printf "[harness] Checking for unit tests in submission files...\n%!" in
  let () = sanitize_directory ~verbose dir in
  let () = if verbose then Format.printf "[harness] Copying release files...\n%!" in
  (* If dir2 is empty, then we want to add some files. *)
  let () = touch_all_files_from_directory opts.input_directory dir "ml" in
  let () = ignore (soft_copy opts.input_directory dir) in
  let () = if verbose then Format.printf "[harness] Preparation complete!%!\n" in
  ()

(** [is_qcheck m] Determine whether the message [m] was generated by running
    a quickcheck unit test. *)
let is_qcheck (msg : string) : bool =
  (* 2014-07-20: Quickchecks always raise exceptions. See assertions.ml for details. *)
  let cQCHECK_REGEXP = Str.regexp "Assertions\\.QCheck_result(\\([0-9]+\\)," in
  try (* Return true if any match found *)
    ignore (Str.search_forward cQCHECK_REGEXP msg 0); true
  with Not_found -> false

(** [parse_qcheck_failures m] Extract the number of qcheck failures from a qcheck result string [m]. *)
let parse_qcheck_failures (msg : string) : int option =
  if   is_qcheck msg
  then Some (try int_of_string (Str.matched_group 1 msg)
             with e -> let () = print_endline "[harness] QCHECK PARSE ERROR in Harness.parse_num_failed. Make sure error message in assertions.ml matches qcheck pattern in harness.ml" in
                       raise e)
  else None

(** [parse_harness_result o e] Parse the error message [e] to determine whether the unit test
    passed, failed, or was a quickcheck. Return the score earned and appropriate error message. *)
let parse_harness_result (opts : options) (failure_msg : string option) : int * (string option) =
  begin match failure_msg with
    | None                  -> (1, None) (* Test passed! *)
    | Some line_that_failed ->           (* Test failed, or was a quickcheck *)
       begin match parse_qcheck_failures line_that_failed with
         | None          -> (0, failure_msg) (* Regular test, not a quickcheck. *)
         | Some num_fail -> (* Qcheck. Success if 100% of quickchecks passed. *)
           let score = opts.quickcheck_count - num_fail in
           let msg = if (score = opts.quickcheck_count) then None else failure_msg in
           (score, msg)
       end
  end

(** [harness_run_test ?v o d t] Run test [t] on the submission in dir [d]. Collect
    the results of each unit test. *)
let harness_run_test ?(verbose=false) opts ~dir (tf : test_file) : TestFileResult.t =
  let ()        = if verbose then Format.printf "[harness] Compiling test file '%s' on submission '%s'.\n%!" tf.absolute_path dir in
  let ()        = ignore (Sys.command (Format.sprintf "cp %s %s" tf.absolute_path dir)) in
  let results   =
    begin match Compile.compile ~quiet:true ~dir:dir tf.name with
      | 0 -> (* Compiled successfully, run tests *)
         let () = if verbose then Format.printf "[harness] Running tests '%s/%s'.\n%!" dir tf.name in
         let () = ignore (Test.test ~quiet:true ~output:opts.temporary_failures_file ~dir:dir tf.name) in
         let raw_lines = In_channel.read_lines (Format.sprintf "%s/%s" dir opts.temporary_failures_file) in
         UnittestSet.fold
           ~f:(fun acc unit_name ->
               let line_matching_test_that_failed_option = List.find raw_lines ~f:(fun line -> (unittest_name_of_line line) = unit_name) in
               let score, err_msg = parse_harness_result opts line_matching_test_that_failed_option in
               TestFileResult.add acc {unittest_name=unit_name; points_earned=score; error_message=err_msg})
           ~init:TestFileResult.empty
           tf.unit_tests
      | _ -> (* Compile error. Generate all-zero results for the no-compile. *)
         let () = if verbose then Format.printf "[harness] NO COMPILE '%s'.\n%!" dir in
         UnittestSet.fold
           ~f:(fun acc unit_name ->
               TestFileResult.add acc {unittest_name=unit_name; points_earned=0; error_message=(Some "NO COMPILE")})
           ~init:TestFileResult.empty
           tf.unit_tests
    end
  in
  let ()        = ignore (Sys.command (Format.sprintf "rm %s/%s.ml" dir tf.name)) in
  results

(** [harness_student ?v opts dir] Run all tests in the harness on student directory [dir].
    The list of results will have have element per test file. Sort results alphabetically
    by test file name. *)
let harness_student ?(verbose=false) (opts : options) (dir : string) : TestFileResultSet.t =
  TestFileSet.fold opts.test_suite
    ~f:(fun acc tf ->
         let r = harness_run_test ~verbose:verbose opts tf ~dir:dir in
         TestFileResultSet.add acc (tf.name, r))
    ~init:TestFileResultSet.empty

(** [write_comments o d tr] Create a markdown file summarizing the test results [tr] for
    directory [d]. These comments can be uploaded directly to CMS. *)
let write_comments (opts : options) (dir : string) (tr : TestFileResultSet.t) : unit =
  let netid = filename_of_path dir in
  let fname = Format.sprintf "%s/%s.md" opts.output_directory netid in
  let title = Format.sprintf "## Automated test results for '%s' ##" netid in
  let body  =
    TestFileResultSet.fold_right
      ~f:(fun (tname, results) acc ->
          let hd    = Format.sprintf "### %s ###\n" tname in
          let body  = string_of_test_results results in
          let ft    = "\n" in
          hd :: body :: ft :: acc)
      ~init:[]
      tr
  in
  Out_channel.write_lines fname (title::body)

(** [write_postscript o d tr] Create a postscript file summarizing the test results [tr] for
    directory [d]. Useful for on-paper grading. *)
let write_postscript (opts : options) (dir : string) (results : TestFileResultSet.t) : unit =
  let netid = filename_of_path dir in
  TestFileResultSet.iter
    ~f:(fun (test_name, results) ->
        (* collect data *)
        let fname = Format.sprintf "%s/%s-%s.ps" opts.output_directory netid test_name in
        let title = Format.sprintf "%s\t\t%s.ml" netid                 test_name in
        let src   = begin match String.rsplit2 ~on:'_' test_name with
                      | Some (module_name,_) ->
                         Format.sprintf "%s/%s.ml" dir module_name
                      | None                 ->
                         let () = Format.printf "[harness] WARNING: Could not find source for file '%s'. Unable to generate postscript." test_name in
                         "\n"
                    end
        in
        let body  = string_of_test_results results in
        (* write postscript *)
        let chn   = Postscript.init          fname title in
        let ()    = Postscript.write_code    chn src in
        let ()    = Postscript.write_results chn body in
        let ()    = Postscript.close         chn in
        ()
       )
    results

(** [post_harness ?v o d] Clean up the directory [d] after testing.
    Remove generated files/logs, clean. *)
let post_harness ?(verbose=false) (opts : options) ~dir (tr : TestFileResultSet.t) : unit =
  let () = if verbose then Format.printf "[harness] Aggregating results for '%s'.\n" dir in
  let () = write_comments opts dir tr in
  let () = if opts.postscript then write_postscript opts dir tr in
  let () = if verbose then Format.printf "[harness] Cleaning directory '%s'.\n" dir in
  let () = List.iter ~f:(fun tgt -> ignore (Clean.clean ~dir:dir tgt)) ["compile";"test"] in
  let () = if verbose then Format.printf "[harness] Cleaning complete!\n" in
  ()

(** [parse_results_from_list us cs] Given a collection of unit test names [us] and a (possibly longer) list
    of scores [scores], create test results matching the unit tests [us] and additionally return the
    remaining scores. *)
let parse_results_from_list (unit_tests : UnittestSet.t) (cols : string list) : TestFileResult.t * (string list) =
  UnittestSet.fold
    ~f:(fun (rs, cols_left) unit_name ->
        begin match cols_left with
          | []   -> failwith "parse error, not enough columns"
          | h::t ->
             let v   = try int_of_string h
                       with e -> let () = print_endline "[harness] ERROR parsing, column doesn't contain int" in
                                 raise e
             in
             let rs' = TestFileResult.add rs {unittest_name=unit_name; points_earned=v; error_message=None} in (* TODO better err msg? *)
             (rs', t)
        end)
    ~init:(TestFileResult.empty, cols)
    unit_tests

(** [harness ?v o submissions] Initialize a spreadsheet with unit test names as columns.
    Iterate through students, filling out the sheet. *)
let harness ?(verbose=false) (opts : options) (subs : string list) : unit =
  let () = if verbose then Format.printf "[harness] Initializing spreadsheet...\n" in
  (* Define spreadsheet *)
  (* 2014-08-01: Sorry, this is the ugliest part of this entire package. The trouble is that
     the module [HarnessSpreadsheet] is dependent on the options [opts]. *)
  let module HarnessSpreadsheet =
    Spreadsheet.Make(struct
      type row              = string * TestFileResultSet.t
      let compare_row r1 r2 = Pervasives.compare (fst r1) (fst r2) (* compare netids *)
      let row_of_string str =
        (* 2014-07-26: string SHOULD be netid, unit_test_scores. The scores SHOULD match the current suite. *)
        begin match String.split str ~on:',' with
          | []               -> failwith "parse error, empty row"
          | netid :: columns ->
             let results_by_test, cols_left =
               TestFileSet.fold
                 ~f:(fun (acc,cols) test ->
                    let rs, cols' = parse_results_from_list test.unit_tests cols in
                    let acc'      = TestFileResultSet.add acc (test.name, rs) in
                    let cols_next = Option.value ~default:[] (List.tl cols') in
                    (acc', cols_next) (* 2014-07-26: List.tl because we skip the totals columns. *)
                   )
                 ~init:(TestFileResultSet.empty,columns)
                 opts.test_suite
             in
             let () =
               begin match cols_left with
                 | []   -> ()
                 | _::_ -> Format.printf "[harness] WARNING unused columns '%s' in spreadsheet row.\n" (String.concat ~sep:"," cols_left)
               end
             in
             (netid, results_by_test)
        end
      let string_of_row row =
        let (netid, rs) = row in
        let string_of_int_list (ints : int list) : string =
          String.concat ~sep:"," (List.map ~f:string_of_int ints)
        in
        let all_scores = (* 2014-07-26: Could get and print the total here. *)
          TestFileResultSet.fold_right rs
          ~f:(fun (_,r) acc -> string_of_int_list (get_scores r) :: acc)
          ~init:[]
        in (* Trailing comma is for the last 'totals' column. *)
        Format.sprintf "%s,%s," netid (String.concat ~sep:",," all_scores)
      let title : string    =
        (* For each test file in the suite, concat all unit test names. Stick title (in uppercase) at the end. *)
        let unit_test_names : string list =
          TestFileSet.fold_right
            ~f:(fun test acc ->
                 let unittest_str = String.concat ~sep:"," (UnittestSet.to_list test.unit_tests) in
                 let title_str    = String.uppercase test.name in
                 (Format.sprintf "%s,%s" unittest_str title_str) :: acc)
            ~init:[]
            opts.test_suite
        in
        Format.sprintf "NetID,%s" (String.concat ~sep:"," unit_test_names)
    end)
  in
  let initial_sheet =
    begin match Sys.file_exists opts.output_spreadsheet with
      | `No | `Unknown -> HarnessSpreadsheet.create ()
      | `Yes           ->
         let () = if verbose then Format.printf "[harness] Reading existing spreadsheet '%s'\n" opts.output_spreadsheet in
         HarnessSpreadsheet.read opts.output_spreadsheet
    end
  in
  let () = if verbose then Format.printf "%s\n[harness] Running all tests...\n%!" (String.make 80 '*') in
  let final_sheet   =
    List.fold
      ~f:(fun sheet dir ->
         let ()          = pre_harness ~verbose:verbose opts dir in
         let netid       = filename_of_path dir in
         let all_results = harness_student ~verbose:verbose opts dir in
         let ()          = post_harness ~verbose:verbose opts ~dir:dir all_results in
         let row         = (netid, all_results) in
         HarnessSpreadsheet.add_row sheet ~row:row
         )
      ~init:initial_sheet
      subs
  in
  let () = if verbose then Format.printf "[harness] Testing complete, writing spreadsheet to '%s'...\n" opts.output_spreadsheet in
  let () = HarnessSpreadsheet.write final_sheet ~filename:opts.output_spreadsheet in
  let () = if verbose then Format.printf "[harness] All done!\n" in
  ()

(** [tests_of_directory d] Get the full filenames of tests from a directory [d].
    For example, if the directory "my_dir" has files "my_test.ml" and "notatest.ml",
    we will return a singleton list containing the string "mydir/my_test.ml". *)
let test_list_of_directory (dir : string) : string list =
  let test_names = filter_directory ~f:is_valid_test_file dir in
  List.map ~f:(fun test_name -> Format.sprintf "%s/%s" dir test_name) test_names

(** [get_unit_test_names ?v ~staging_dir test_file] Extract the names of all
    unit tests from the file [test_file] by compiling it in directory [staging_dir].
    Raise an error if the file [test_file] does not exist.
    Unit test names are extracted from the log file [cPA_OUNIT_LOGFILE], generated by running
    [pa_ounit] tests. *)
let get_unittest_names ?(verbose=false) ~staging_dir (test_abs_path : string) : UnittestSet.t =
  let ()        = if verbose then Format.printf "[harness] collecting unit test names for file '%s'...\n%!" test_abs_path in
  let test_name = filename_of_path test_abs_path in
  begin match Sys.file_exists test_abs_path with
    | `No | `Unknown ->
      let msg = Format.sprintf "Could not find test file '%s'." test_abs_path in
      raise (File_not_found msg)
    | `Yes           ->
      (* Copy test into staging dir, compile test, run to get names out. *)
      let ()  = if verbose then Format.printf "[harness] copying the test '%s' into directory '%s'...\n" test_abs_path staging_dir in
      let ()  = check_code (Sys.command (Format.sprintf "cp %s %s" test_abs_path staging_dir)) in
      let ()  = if verbose then Format.printf "[harness] running tests in file '%s/%s'...\n" staging_dir test_name in
      let ()  = check_code (Test.test ~quiet:true ~verbose:verbose ~compile:true ~dir:staging_dir test_name)  in
      let raw = In_channel.read_lines (Format.sprintf "%s/%s" staging_dir Cli_config.cPA_OUNIT_LOGFILE) in
      let ()  = List.iter ~f:(fun tgt -> ignore (Clean.clean ~dir:staging_dir tgt)) ["compile";"test"] in
      let ()  = if verbose then Format.printf "[harness] successfully ran tests and collected log file. Removing file '%s/%s'\n" staging_dir test_name in
      let ()  = check_code (Sys.command (Format.sprintf "rm %s/%s" staging_dir test_name)) in
      List.fold_left raw
        ~f:(fun acc line -> UnittestSet.add acc (unittest_name_of_line line))
        ~init:UnittestSet.empty
  end

(** [test_suite_of_list ?v ~staging_dir ts] Convert a list of relative paths to tests [ts] into a test suite.
    The test suite is a more convenient representation. *)
let test_file_set_of_list ?(verbose=false) ~staging_dir (tests : string list) : TestFileSet.t =
  List.fold_right
    ~f:(fun path suite ->
        let () = assert_file_exists path in (* hmmm, the error raised here may not be clear. *)
        let name = strip_suffix (filename_of_path path) in
        let t = {
          absolute_path = path;
          name          = name;
          unit_tests    = begin try get_unittest_names ~verbose:verbose ~staging_dir:staging_dir path
                                with e -> let () = Format.printf "[harness] ERROR: Could not compile files in directory '%s'\n%!" staging_dir in
                                          raise e
                          end
        } in
        TestFileSet.add suite t)
    ~init:TestFileSet.empty
    tests

(** [get_test_files cfg ~test_dir tests] Return a list of filenames -- the names of
    the test files that will constitute the harness. There are 3 stages of fallback.
    {ul
      {li {First, check the list of filenames [tests]. If this is non-empty, filter the
           invalid filenames and return.}
      {li {Second, check the optional directory [test_dir] for test files}
      {li {Finally, fall back to the directory specified in the config [cfg].}
    } *)
let get_test_files (cfg : Cli_config.t) ~test_dir (tests : string list) : string list =
  begin match tests with
    | _::_ ->
       let () = List.iter ~f:(fun x -> assert_file_exists x) tests in
       tests
    | []   ->
       let test_dir  = Option.value       test_dir ~default:cfg.harness.tests_directory in
       let ()        = assert_file_exists test_dir ~msg:"Could not find tests directory" in
       test_list_of_directory test_dir
  end

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
      +> flag ~aliases:["-v"]               "-verbose"     (no_arg)          ~doc:" Print debugging information."
      +> flag ~aliases:["-t"]               "-test"        (listed file)     ~doc:"FILE Use the unit tests in the module FILE."
      +> flag ~aliases:["-p"; "-ps"]        "-postscript"  (optional bool)   ~doc:" Generate postscript output."
      +> flag ~aliases:["-r";"-i";"-input"] "-release"     (optional file)   ~doc:"DIR Release directory. Used to get starter code and as a staging area."
      +> flag ~aliases:["-d"]               "-directory"   (optional file)   ~doc:"DIR Use all unit tests in all modules under directory DIR."
      +> flag ~aliases:["-o"]               "-output"      (optional string) ~doc:"DIR Set the output directory."
      +> flag ~aliases:["-s";"-sheet"]      "-spreadsheet" (optional string) ~doc:"FILE Location to write the spreadsheet."
      +> anon (sequence ("submission" %: string))
    )
    (fun v tests ps input test_dir output_dir sheet_location subs () ->
      let ()         = if v then Format.printf "[harness] Parsing options...\n%!" in
      let cfg        = Cli_config.init () in
      let input_dir  = Option.value       ~default:cfg.harness.input_directory input in
      let ()         = assert_file_exists ~msg:"Release directory missing"     input_dir in
      let test_files = get_test_files cfg ~test_dir:test_dir                   tests in
      let opts = {
        input_directory         = input_dir;
        output_directory        = Option.value output_dir     ~default:cfg.harness.output_directory;
        output_spreadsheet      = Option.value sheet_location ~default:cfg.harness.output_spreadsheet;
        postscript              = Option.value ps             ~default:cfg.harness.postscript;
        quickcheck_count        = 100; (* 2014-07-31: Very ugly hardcode. See 'assertions.ml'. *)
        temporary_failures_file = cfg.harness.temporary_failures_file;
        test_suite              = test_file_set_of_list ~verbose:v ~staging_dir:input_dir test_files;
      } in
      let () = ensure_dir (Format.sprintf "%s/_harness" Cli_config.cOUTPUT_DIRECTORY) in
      let () = ensure_dir opts.output_directory in
      if TestFileSet.is_empty opts.test_suite
      then Format.printf "[harness] Empty test suite, nothing to run (tests should end with suffix '_test.ml'). Bye now!\n"
      else harness ~verbose:v opts (at_expand subs)
    )
