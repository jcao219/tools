open Cli_constants
open Filepath_util

(** [stat fname] read in the test results from [fname], create 
 * a mapping of test names to #passes/#failures, print results
 * as a dump and as histograms *)
let run (sheet_name : string) : unit =
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
      let () = line := String.trim (input_line chn) in
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

