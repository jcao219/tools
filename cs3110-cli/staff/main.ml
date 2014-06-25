open Cli_constants
open Process_util
open Filepath_util

let help () =
  print_string "\
Usage: cs3110-staff COMMMAND [args]

  cs3110 clean                         Removes files created by 'cs3110 compile'.
                                        Add an argument to clean files generated
                                        by that command.
  cs3110 compile <file>                Compile file.ml.
  cs3110 cms <file>                    Convert spreadsheet [file] into a
                                        CMS-ready grades table.
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

let () =
  let () = config_env () in
  try
    match Array.to_list Sys.argv with
    | [ _; "help" ]  -> help ()
    | [ _; "clean" ] -> Clean.build ()
    | [ _; "clean"; "diff" ] -> Clean.diff ()
    | [ _; "clean"; "email" ] -> Clean.email ()
    | [ _; "clean"; "harness" ] -> Clean.harness ()
    | [ _; "clean"; "smoke" ] -> Clean.smoke ()
    | [ _; "clean"; "all" ] -> begin
      Clean.clean ["build";"cms";"diff";"email";"harness";"smoke"] ()
    end
    | [ _; "clean"; _ ] -> ()
    | [ _; "compile"; target ] -> check_code (Build.run (strip_suffix target))
    |  _ :: "diff" :: arg1 :: args ->
      if arg1.[0] = '@'
      then Diff.run (directories_of_list arg1)
      else Diff.run (arg1 :: args)
    | [ _; "cms"; sheet ] ->
      let () = assert_file_exists sheet in
      Cms.run sheet
    | [ _; "doc"; out_dir; src_dir] ->
      check_code (Doc.run ~src_dir:src_dir out_dir)
    | [ _; "doc"; out_dir] ->
      check_code (Doc.run out_dir)
    | [ _; "email" ] -> Email.run ()
    | _ :: "harness" :: arg1 :: args ->
      (* Make sure test dir exists *)
      let () = assert_file_exists cTESTS_DIR in
      (* Ensure output directories *)
      let () = ensure_dir cCMS_DIR in
      let () = ensure_dir cOUTPUT_DIR in
      (* convert to absolute path *)
      let abs_dir = absolute_path cTESTS_DIR in
      (* Run harness *)
      if arg1.[0] = '@'
      then Harness.run abs_dir (directories_of_list arg1)
      else Harness.run abs_dir (arg1 :: args)
    | _ :: "run" :: target :: args ->
        let target' = strip_suffix target in
        check_code (Run.run target' args)
    | _ :: "smoke" :: arg1 :: args ->
      if arg1.[0] = '@'
      then Smoke.run (directories_of_list arg1)
      else Smoke.run (arg1 :: args)
    | [ _; "stat"; sheet ] ->
      Stat.run sheet
    | [ _; "test"; target ] ->
        let target' = strip_suffix target in check_code (Test.test target')
    | _ ->
      let () = print_endline "Invalid arguments." in
      help ()
  with File_not_found filename ->
    Format.printf "Could not find the file %s.\n%!" filename
