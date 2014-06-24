open Cli_constants
open Filepath_util
open Io_util

exception Invalid_spreadsheet of string

(* [get_columns s] Collect the capitalized columns from the string [s]. 
 * Ignore the CMS-specific [Totals] column. *)
let get_columns (s : string) : string list * int list =
  (* Split line by comma, fold over titles *)
  begin match Str.split (Str.regexp ",") s with
    | [] -> raise (Invalid_spreadsheet "Cannot generate grades table from empty sheet.")
    | "NetID"::tail ->
      let titles, indices, _ = 
        List.fold_left (fun (ts,is,i) title ->
          let t_code = Char.code title.[0] in
          if (Char.code 'A' <= t_code) && (t_code <= Char.code 'Z') && (title <> "Total")
          then (title :: ts, i::is, i+1)
          else (ts,is,i+1)
        ) (["NetID"],[],1) tail 
      in
      (List.rev ("Add Comments" :: titles)), List.rev indices
    | _::_ -> raise (Invalid_spreadsheet "First column MUST be 'NetID'")
  end

let get_comments (netid : string) : string =
  let fname = Format.sprintf "%s/%s.md" cOUTPUT_DIR netid in
  if (Sys.file_exists fname)
  then (",\"" ^ (String.concat " \n " (read_lines (open_in fname))) ^ "\"")
  else ","

(** [run f] Parse the spreadsheet [f].
 * Save the columns denoted with capital letters (but not the overall total)
 * Add comments using the files in [cOUTPUT_DIR]. *)
let run (fname : string) : unit =
  let in_chn = open_in fname in
  let out_chn = open_out cCMS_FINAL in
  (* Get titles from first line *)
  let columns, indices = get_columns (input_line in_chn) in
  Format.printf "INDICES = [%s]\n" (String.concat "; " (List.map string_of_int indices));
  (* Write titles to output *)
  let () = output_string out_chn (String.concat "," columns) in
  let () = output_string out_chn "\n" in
  (* Get netids and scores from remaining lines,
   * write these to the output *)
  fold_in_channel (fun _ line ->
    (* Iterate over entries in row. Track current index and title indices so-far-popped. *)
    let netid, tail = lsplit line ',' in
    let () = output_string out_chn netid in
    let _ =
      List.fold_left (fun (curr_i, ixs) score ->
        begin match ixs with
          | [] ->
            (curr_i+1, ixs)
          | i::xs when i = curr_i ->
            let () = output_string out_chn (","^score) in
            (curr_i+1, xs)
          | _::_ ->
            (curr_i+1, ixs)
        end
      ) (1, indices) (Str.split (Str.regexp ",") tail)
    in
    let () = output_string out_chn (get_comments netid) in
    output_string out_chn "\n"
  ) () in_chn 
