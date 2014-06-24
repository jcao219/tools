open Cli_constants
open Filepath_util
  
let diff_tmp = ".diff"
let num_files_diffed = ref 0

let create_netid_map (directories : string list) =
  let netid_map = Hashtbl.create 27 in 
  let () = 
    List.iter (fun dir -> 
      Hashtbl.add netid_map (tag_of_path dir) dir
    ) (strip_trailing_slash_all directories) 
  in
  netid_map

(** [get_all_nocompiles ()] read the local directory of no compiles,
 * [cNOCOMPILE_DIR], return all filenames in a sorted array. *)
let get_all_nocompiles () : string array =
  let arr = Sys.readdir cNOCOMPILE_DIR in
  let () = Array.sort Pervasives.compare arr in
  arr

(** [diff ()] if any files under the [cNOCOMPILE_DIR] match a target,
 * perform a diff on the pre-and post submission. Record results in a csv file. *)
let run (directories : string list) : unit =
  let () = assert_file_exists cNOCOMPILE_DIR in
  let () = num_files_diffed := 0 in
  (* Space to store outputs *)
  (* maps netid -> dir, to make it easy to get new input from the 
  * name of a previous no-compile *)
  let netid_map = create_netid_map directories in
  let results_chn = open_out cDIFF_RESULTS in
  (* For each file in the nocompile folder, check if it has a match in [directories] *)
  let all_nocompiles = get_all_nocompiles () in
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
          let old_file = Format.sprintf "%s/%s/%s" cNOCOMPILE_DIR netid fname in
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
                incr num_files_diffed
              in
              !user_input
          end
        end 
      (* initial accumulator for the fold is 1 because of the guard on 0 
       * If [acc = 0], we stop doing diffs for the student. *)
      ) 1 (Sys.readdir (Format.sprintf "%s/%s" cNOCOMPILE_DIR netid)) in
      (* Save the results to the .csv *)
      output_string results_chn (Format.sprintf "%s,%d\n" netid result)
  ) all_nocompiles;
  (* Close up *)
  let () = ignore(Sys.command (Format.sprintf "rm -f %s" diff_tmp)) in
  let () = close_out results_chn in
  Format.printf "Finished inputting decisions for %d files. See '%s' for results\n%!" (!num_files_diffed) cDIFF_RESULTS
