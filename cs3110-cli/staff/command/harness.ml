open Core.Std
open Cli_constants
open Io_util
open Filepath_util

(* aka 'ordered list', used only to keep unit test names ordered within a test file. *)
(* TODO remove this *)
module StringSet = Set.Make(String)

type test_file = {
  absolute_path : string;      (* Exact path to the test file. Ends with the filename. *)
  name          : string;      (* Short name of the test file. *)
  unit_tests    : StringSet.t; (* Names of the unit tests within the file *)
}

(* For organizing test files. Keep tests alphabetized by filename. *)
module TestSuite = Set.Make(struct
  type t            = test_file
  let compare t1 t2 = Pervasives.compare t1.name t2.name
  let sexp_of_t _   = failwith "not implemented"
  let t_of_sexp _   = failwith "not implemented"
end)

(* The results of running one test file. It's a set to keep unit test names alphabetized. *)
module TestResults = Set.Make(struct
  type t                        = string * int * (string option) (* unittest_name , points_earned , error_message *)
  let compare (n1,_,_) (n2,_,_) = Pervasives.compare n1 n2
  let sexp_of_t _               = failwith "not implemented"
  let t_of_sexp _               = failwith "not implemented"
end)

(* TODO test results set? *)

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
    set-to-string conversion. *)
let string_of_test_results (results : TestResults.t) : string =
  String.concat ~sep:"\n"
    (TestResults.fold_right
       ~f:(fun (unit_name, _, err_msg) acc ->
           begin match err_msg with
             | None   -> success_message unit_name
             | Some e -> failure_message unit_name e
           end :: acc)
       ~init:[]
       results)

(** [get_scores tr] Get just the scores (in order) from the test results [t]. *)
let get_scores (tr : TestResults.t) : int list =
  TestResults.fold_right tr
    ~f:(fun (_,score,_) acc -> score :: acc)
    ~init:[]

(** [sort_results rs] Sort a list of pairs [rs] of (test_file_name, results) in alphabetical
    order of the test_file_names. *)
let sort_results (rs : (string * TestResults.t) list) : (string * TestResults.t) list =
  List.sort rs
    ~cmp:(fun (s1,_) (s2,_) -> Pervasives.compare s1 s2)

(** [sanitize_file f] Check if the file [f] contains the word TEST. Ask
    user to remove all occurrences or approve the file as-is. You'd want
    to approve if TEST was in a comment or part of a variable name like
    [cTESTING_MY_BOUNDARIES]. *)
let sanitize_file (fname : string) : unit =
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

(** [sanitize_directory d] Check all [.ml] files in directory [d]
    for unit tests. If any, ask the user to remove them before continuing. *)
let sanitize_directory (dir : string) : unit =
  let ml_files = filter_directory ~f:(String.is_suffix ~suffix:".ml")  dir in
  List.iter ~f:sanitize_file ml_files

(** [pre_harness o d] Prepare the directory [d] for testing. *)
let pre_harness (opts : options) (dir : string) : unit =
  let () = if opts.verbose then Format.printf "[harness] Preparing directory '%s' for testing.\n" dir in
  let () = if opts.verbose then Format.printf "[harness] Checking for unit tests in submission files...\n" in
  let () = sanitize_directory dir in
  let () = if opts.verbose then Format.printf "[harness] Copying release files...\n" in
  let () = ignore (soft_copy opts.release_directory dir) in
  let () = if opts.verbose then Format.printf "[harness] Preparation complete!\n" in
  ()

(** [is_qcheck m] Determine whether the message [m] was generated by running
    a quickcheck unit test. (2014-07-20 Those always raise exceptions.) *)
let is_qcheck (msg : string) : bool =
  let stripped = snd (rsplit (fst (rsplit msg '(')) 'A') in
  stripped = "ssertions.QCheck_result" (* very janky *)

(** [parse_num_failed m] Extract the number of qcheck failures from a qcheck result string [m]. *)
let parse_num_failed (msg : string) : int =
  (* Convert '...Assertions.Qcheck_result(9001,"heyheyhey")...' into 9001 *)
  int_of_string (fst (lsplit (snd (rsplit msg '(')) ','))

(** [parse_harness_result o e] Parse the error message [e] to determine whether the unit test
    passed, failed, or was a quickcheck. Return the score earned and appropriate error message. *)
let parse_harness_result opts (failure_msg : string option) : int * (string option) =
  begin match failure_msg with
    | Some e ->
       if is_qcheck e then
         (* Success if 100% of quickchecks passed. *)
         let score = opts.num_quickcheck - (parse_num_failed e) in
         let msg = if (score = opts.num_quickcheck) then None else Some e in
         (score, msg)
       else
         (0, Some e)
    | None   ->
       (1, None)
  end

(** [harness_run_test t d] Run test [t] on the submission in dir [d]. Collect
    the results of each unit test. *)
let harness_run_test opts ~dir (t : test_file) : TestResults.t =
  let ()        = if opts.verbose then Format.printf "[harness] running test file '%s' on submission '%s'.\n" t.absolute_path dir in
  let ()        = ignore (Sys.command (Format.sprintf "cp %s %s" t.absolute_path dir)) in
  let ()        = Process_util.check_code (Test.test ~quiet:true ~output:opts.fail_output ~dir:dir t.name) in
  let failures  = In_channel.read_lines (Format.sprintf "%s/%s" dir opts.fail_output) in
  StringSet.fold
    ~f:(fun acc unit_name ->
        let failure_msg = List.find failures ~f:(fun line -> (unittest_name_of_line line) = unit_name) in
        let score, err_msg = parse_harness_result opts failure_msg in
       TestResults.add acc (unit_name, score, err_msg))
    ~init:TestResults.empty
    t.unit_tests

(** [harness_student] Run all tests in the harness on student [dir].
    The list of results will have have element per test file. Sort results
    alphabetically by test file name. *)
let harness_student (opts : options) (dir : string) : (string * TestResults.t) list =
  let results =
    TestSuite.fold opts.test_suite
      ~f:(fun acc t -> (t.name, harness_run_test opts t ~dir:dir) :: acc)
      ~init:[]
  in
  sort_results results

(** [write_comments o d tr] Create a markdown file summarizing the test results [tr] for
    directory [d]. These comments can be uploaded directly to CMS. *)
let write_comments (opts : options) (dir : string) (tr : (string * TestResults.t) list) : unit =
  let netid = filename_of_path dir in
  let fname = Format.sprintf "%s/%s.md" opts.output_directory netid in
  let title = Format.sprintf "## Automated test results for '%s' ##" netid in
  let body  =
    List.fold_right
      ~f:(fun (test_file, results) acc ->
          let hd    = Format.sprintf "### %s ###\n" test_file in
          let body  = string_of_test_results results in
          hd :: body :: acc)
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
  let () = if opts.verbose then Format.printf "[harness] Aggregating results for '%s'.\n" dir in
  let () = write_comments opts dir tr in
  let () = if opts.postscript then write_postscript opts dir tr in
  let () = if opts.verbose then Format.printf "[harness] Cleaning directory '%s'.\n" dir in
  let () = List.iter ~f:(fun tgt -> Clean.clean ~dir:dir tgt) ["compile";"test"] in
  let () = if opts.verbose then Format.printf "[harness] Cleaning complete!\n" in
  ()

(** [parse_results_from_list us cs] Given a collection of unit test names [us] and a (possibly longer) list
    of scores [scores], create test results matching the unit tests [us] and additionally return the
    remaining scores. *)
let parse_results_from_list (unit_tests : StringSet.t) (cols : string list) : TestResults.t * (string list) =
  StringSet.fold
    ~f:(fun (rs, cols_left) unit_name ->
        begin match cols_left with
          | []   -> failwith "parse error, not enough columns"
          | h::t ->
             let v   = try int_of_string h with _ -> failwith "parse error, column doesn't contain int" in
             let rs' = TestResults.add rs (unit_name, v, None) in (* TODO better err msg? *)
             (rs', t)
        end)
    ~init:(TestResults.empty, cols)
    unit_tests

(** [harness o submissions] Initialize a spreadsheet with unit test names as columns.
    Iterate through students, filling out the sheet. *)
let harness (opts : options) (subs : string list) : unit =
  let () = if opts.verbose then Format.printf "[harness] Initializing spreadsheet...\n" in
  (* define spreadsheet *)
  let module HarnessSpreadsheet =
    (* TODO Set.add doesn't replace existing *)
    Spreadsheet.Make(struct
      type row              = string * ((string * TestResults.t) list) (* should be sorted by test file *)
      let compare_row r1 r2 = Pervasives.compare (fst r1) (fst r2)     (* compare netids *)
      let filename : string = opts.spreadsheet_location
      let row_of_string str =
        (* string SHOULD be netid, unit_test_scores *)
        begin match String.split str ~on:',' with
          | []               -> failwith "parse error, empty row"
          | netid :: columns ->
             let results_by_test, cols_left =
               TestSuite.fold
                 ~f:(fun (acc,cols) test ->
                    let pairs, cols' = parse_results_from_list test.unit_tests cols in
                    ((test.name, pairs) :: acc, cols')
                   )
                 ~init:([],columns)
                 opts.test_suite
             in
             let () =
               begin match cols_left with
                 | []   -> ()
                 | _::_ -> Format.printf "[harness] WARNING unused columns '%s' in spreadsheet row." (String.concat ~sep:"," cols_left)
               end
             in
             (netid, sort_results results_by_test)
        end
      let string_of_row r   =
        let (netid, results_by_test) = r in
        let string_of_int_list (ints : int list) : string =
          String.concat ~sep:"," (List.map ~f:string_of_int ints)
        in
        let all_scores = List.map ~f:(fun (_,r) -> string_of_int_list (get_scores r)) results_by_test in
        Format.sprintf "%s,%s" netid (String.concat ~sep:",," all_scores)
      let title : string    =
        (* For each test file in the suite, concat all unit test names *)
        let unit_test_names : string list =
          TestSuite.fold_right
            ~f:(fun test acc -> (String.concat ~sep:"," (StringSet.to_list test.unit_tests)) :: acc)
            ~init:[]
            opts.test_suite
        in
        Format.sprintf "NetID,%s" (String.concat ~sep:",," unit_test_names)
    end)
  in
  let () = if opts.verbose then Format.printf "[harness] Running all tests...\n" in
  let sheet =
    List.fold
      ~f:(fun sheet dir ->
         let ()          = pre_harness opts dir in
         let netid       = filename_of_path dir in
         let all_results = harness_student opts dir in
         let ()          = post_harness opts ~dir:dir all_results in
         let row         = (netid, all_results) in
         HarnessSpreadsheet.add_row sheet ~row:row
         )
      ~init:(HarnessSpreadsheet.create ())
      subs
  in
  HarnessSpreadsheet.write sheet

(** [is_valid_test_file fn] true if filename matches the expected format, false otherwise *)
let is_valid_test_file (fname : string) : bool =
  String.length fname <> 0 &&
  fname.[0] <> '.' &&
  is_suffix fname "_test.ml"

(** [tests_of_directory d] Get the full filenames of tests from a directory [d].
    For example, if the directory "my_dir" has files "my_test.ml" and "notatest.ml",
    we will return a singleton list containing the string "mydir/my_test.ml". *)
let test_list_of_directory ?(verbose=false) (dir : string) : string list =
  Core.Std.List.fold_right
    (Array.to_list (Sys.readdir dir))
    ~f:(fun fname acc ->
        if is_valid_test_file fname then
          let full_path = Format.sprintf "%s/%s" dir fname in
          full_path :: acc
        else
          let () = if verbose then Format.printf "[harness] WARNING: skipping invalid test file '%s/%s'.\n" dir fname in
          acc)
    ~init:[]

(** [get_unit_test_names d t] Extract the names of all unit tests from the
    file [test] by compiling it in directory [d]. Raise an error if the file
    [test] does not exist.
    Relies on the file 'inline_tests.log' automatically generated when running
    giving pa_ounit the [-log] option. *)
let get_unittest_names ?test_name ~staging_dir (test_abs_path : string) : StringSet.t =
  begin match Sys.file_exists test_abs_path with
    | `No | `Unknown ->
      let msg = Format.sprintf "Could not find test file '%s'." test_abs_path in
      raise (File_not_found msg)
    | `Yes           ->
      (* Copy test into dir, compile test, run to get names out. *)
      let ()  = ignore (Sys.command (Format.sprintf "cp %s %s" test_abs_path staging_dir)) in
      let nm  = Option.value test_name ~default:(strip_suffix (filename_of_path test_abs_path)) in
      let ()  = Process_util.check_code (Test.test ~quiet:true ~dir:staging_dir nm)  in
      let raw = In_channel.read_lines (Format.sprintf "%s/%s" staging_dir cTEST_OUTPUT) in
      let ()  = List.iter ~f:(fun tgt -> Clean.clean ~dir:staging_dir tgt) ["compile";"test"] in
      StringSet.of_list (List.map ~f:unittest_name_of_line raw)
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
          unit_tests    = get_unittest_names ~test_name:name ~staging_dir:staging_dir path;
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
      let tests_dir = Option.value test_dir ~default:cTESTS_DIR in
      let opts = {
        fail_output          = cFAIL_OUTPUT;
        num_quickcheck       = Option.value qc ~default:cNUM_QCHECK;
        output_directory     = Option.value output_dir ~default:cHARNESS_DIR;
        postscript           = ps;
        release_directory    = release_dir;
        spreadsheet_location = Option.value sheet_location ~default:cHARNESS_SHEET;
        test_suite           = test_suite_of_list ~staging_dir:release_dir (tests @ test_list_of_directory ~verbose:v tests_dir);
        verbose              = v;
      } in
      harness opts (at_expand subs))
