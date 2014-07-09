open Io_util
open Filepath_util

(* 
 * 2014-03-23 Note: this is currently brittle because scores are not directly
 * connected to column titles. We rely on correct ordering in [add_row]; there
 * is NO error checking. Not that this is a problem if the rest of the code
 * doesn't move, but it's a bit unsetting.
 *)

exception Invalid_spreadsheet of string

(* NetID, scores_by_test *)
type row = string * ((string * int list) list)

(* Keep spreadsheet data in a set, to preserve ordering on students *)
module S = Set.Make(struct 
  type t = row
  let compare (id1,_) (id2,_) = Pervasives.compare id1 id2
end)

type t = { 
  test_cases_by_file : (string * (string list)) list; 
  data : S.t; 
}

let cSEPARATOR = ","

(* [make_title s] Convert [s] into something that'd go on to CMS *)
let make_title s = String.capitalize (fst (lsplit s '_'))

(* [add_row t netId scores_by_test] ALMOST adds data directly to the sheet.
 * There is one preprocessing step: converted the bucketed [scores_by_test]
 * to scores and totals *)
let add_row t netId scores_by_test =
  (* Add padding to nocompiles *)
  let scores_by_test =
    List.fold_right (fun (test_name,scores) acc ->
      let test_title = make_title test_name in
      begin match scores with
        | [] -> (* No compile *)
          (* Find the number of test cases *)
          let cases = List.fold_left (fun acc (name, cases) ->
            if test_title = name then cases else acc) [] t.test_cases_by_file
          in
          (* Padding *)
          (test_title, (List.fold_left (fun acc _ -> 0 :: acc) [] cases)) :: acc
        | _::_ -> 
          (test_title, scores) :: acc
      end
    ) scores_by_test []
  in
  let v = (netId,scores_by_test) in
  { t with data = S.add v (S.remove v t.data) }

(* [init fname names_by_file] create a new spreadsheet named [fname]
 * columns are:
 * - one for each test case
 * - totals for each test file *)
let init names_by_file : t = 
  {
    test_cases_by_file = List.map (fun (fname,cases) -> make_title fname, cases) names_by_file;
    data = S.empty;
  }

(** [init_read_header s cs] helper for initialization. Step through [s]
 * by [cSEPARATOR], if capitalized, assume is test name. Otherwise assume is case *)
let rec init_read_header str acc_cases cases_by_name =
    (* Split the string *)
  let hd, tl = lsplit str cSEPARATOR.[0] in
    (* Do shit with [hd] *)
  let acc', cases' =
    if (Char.code 'A' <= Char.code hd.[0]) && (Char.code hd.[0] <= Char.code 'Z')
    then (* Uppercase, must be a test. *)
      begin match acc_cases with
        | [] -> (* Ignore this, probably NetId *)
          acc_cases, cases_by_name
        | _::_ ->
          [], (hd, (List.rev acc_cases)) :: cases_by_name
      end
    else (* Lowercase, it's a test case *)
      hd :: acc_cases, cases_by_name
  in
  (* Choose whether to recurse or end *)
  if not (String.contains tl cSEPARATOR.[0])
  then cases' (* [acc_cases] should really be empty here *)
  else init_read_header tl acc' cases'

(* Parse exisiting spreadsheet for column names and values *)
let init_from_file (fname : string) : t =
  (* Open channel *)
  let chn = open_in fname in
  (* Read titles into a test-cases-by-file *)
  let parsed_title = 
    try 
      List.rev (init_read_header (input_line chn) [] [])
    with End_of_file -> raise (Invalid_spreadsheet ("Error parsing spreadsheet. File is empty.\n"))
  in
  let parsed_lines =
    (* Fold over lines *)
    fold_in_channel (fun row_set line ->
      let netid, line = lsplit line cSEPARATOR.[0] in
      (* Fold over test cases within line *)
      let scores_by_test = 
        List.rev (snd (List.fold_left (fun (line,acc) (name,cases) ->
          (* Pull one score per case *)
          let line, scores =
            List.fold_left (fun (line, acc) _ ->
              let hd, tl = lsplit line cSEPARATOR.[0] in
              if hd = "" then
                (* Advance cursor *)
                tl, acc 
              else
                try tl, (int_of_string hd) :: acc with Failure "int_of_string" -> 
                  raise (Invalid_spreadsheet (Format.sprintf "Error parsing spreadsheet: cannot convert %s to int.\n" hd))
            ) (line,[]) cases
          in
          let line' = 
            let ln = String.length line in
            if ln < 2 then line else String.sub line 1 (ln-1)
          in
          line', ((name, List.rev scores) :: acc)
        ) (line,[]) parsed_title))
      in
      S.add (netid,scores_by_test) row_set
    ) S.empty chn
  in
  { 
    test_cases_by_file = parsed_title;
    data = parsed_lines
  }

(* Print the spreadsheet *)
let write t filename =
  let chn = open_out filename in
  (* Print header *)
  let () = output_string chn "NetID" in
  (* Print test case names. Add totals at end of section *)
  let () =
    List.iter (fun (test_name,cases) -> 
      let () = 
        List.iter (fun name ->
          output_string chn (cSEPARATOR ^ name)
        ) cases 
      in 
      output_string chn (cSEPARATOR ^ test_name)
    ) t.test_cases_by_file
  in
  let () = output_string chn (cSEPARATOR ^ "Total\n") in
  (* Print data *)
  let () = 
    S.iter (fun (id,scores_by_test) ->
      let () = output_string chn id in (* print name *)
      let () = (* print scores *)
        List.iter (fun (_,scores) ->
          let () = output_string chn cSEPARATOR in (* empty space for test totals *)
          let () = output_string chn (String.concat cSEPARATOR (List.map string_of_int scores)) in
          output_string chn cSEPARATOR (* empty space for test totals *)
        ) scores_by_test
      in
      output_string chn "\n" (* End line *)
    ) t.data
  in
  ignore (close_out chn)
