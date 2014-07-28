open Core.Std
open Cli_constants
open Filepath_util
open Io_util

exception Invalid_spreadsheet of string

module StringSet = Set.Make(String)
type options = {
  columns            : StringSet.t;
  delimeter          : char;
  input_spreadsheet  : string;
  output_spreadsheet : string;
  verbose            : bool;
}

(* (\* [get_columns s] Collect the capitalized columns from the string [s]. *)
(*  * Ignore the CMS-specific [Totals] column. *\) *)
(* let get_columns (s : string) : string list * int list = *)
(*   (\* Split line by comma, fold over titles *\) *)
(*   begin match Str.split (Str.regexp ",") s with *)
(*     | [] -> raise (Invalid_spreadsheet "Cannot generate grades table from empty sheet.") *)
(*     | "NetID"::tail -> *)
(*       let titles, indices, _ = *)
(*         List.fold_left (fun (ts,is,i) title -> *)
(*           let t_code = Char.code title.[0] in *)
(*           if (Char.code 'A' <= t_code) && (t_code <= Char.code 'Z') && (title <> "Total") *)
(*           then (title :: ts, i::is, i+1) *)
(*           else (ts,is,i+1) *)
(*         ) (["NetID"],[],1) tail *)
(*       in *)
(*       (List.rev ("Add Comments" :: titles)), List.rev indices *)
(*     | _::_ -> raise (Invalid_spreadsheet "First column MUST be 'NetID'") *)
(*   end *)

(* let get_comments (netid : string) : string = *)
(*   let fname = Format.sprintf "%s/%s.md" cOUTPUT_DIR netid in *)
(*   if (Sys.file_exists fname) *)
(*   then (",\"" ^ (String.concat " \n " (read_lines (open_in fname))) ^ "\"") *)
(*   else "," *)

(* (\** [run f] Parse the spreadsheet [f]. *)
(*  * Save the columns denoted with capital letters (but not the overall total) *)
(*  * Add comments using the files in [cOUTPUT_DIR]. *\) *)
(* let run (fname : string) : unit = *)
(*   let in_chn = open_in fname in *)
(*   let out_chn = open_out cCMS_FINAL in *)
(*   (\* Get titles from first line *\) *)
(*   let columns, indices = get_columns (input_line in_chn) in *)
(*   Format.printf "INDICES = [%s]\n" (String.concat "; " (List.map string_of_int indices)); *)
(*   (\* Write titles to output *\) *)
(*   let () = output_string out_chn (String.concat "," columns) in *)
(*   let () = output_string out_chn "\n" in *)
(*   (\* Get netids and scores from remaining lines, *)
(*    * write these to the output *\) *)
(*   fold_in_channel (fun _ line -> *)
(*     (\* Iterate over entries in row. Track current index and title indices so-far-popped. *\) *)
(*     let netid, tail = lsplit line ',' in *)
(*     let () = output_string out_chn netid in *)
(*     let _ = *)
(*       List.fold_left (fun (curr_i, ixs) score -> *)
(*         begin match ixs with *)
(*           | [] -> *)
(*             (curr_i+1, ixs) *)
(*           | i::xs when i = curr_i -> *)
(*             let () = output_string out_chn (","^score) in *)
(*             (curr_i+1, xs) *)
(*           | _::_ -> *)
(*             (curr_i+1, ixs) *)
(*         end *)
(*       ) (1, indices) (Str.split (Str.regexp ",") tail) *)
(*     in *)
(*     let () = output_string out_chn (get_comments netid) in *)
(*     output_string out_chn "\n" *)
(*   ) () in_chn *)

(** [get_titles_exn ~sep sheet] Return the first line from the file [sheet],
    split into a list on the character [~sep]. Raise an exception in the
    file [sheet] is empty. *)
let get_first_line (sheet : string) : string list =
  let input    = In_channel.create sheet in
  let line_opt = In_channel.input_line input in
  let ()       = In_channel.close sheet in
  begin match line_opt with
    | Some ln -> String.split ~on:sep ln
    | None    ->
       let msg = Format.sprintf "Empty spreadhseet '%s'." sheet in
       raise (Invalid_spreadsheet msg)
  end

(** [cms o sheet] Read the spreadsheet [sheet] and extract
    particular columns. Save these columns along with
    harness-generated comments in a new spreadsheet. *)
let cms (opts : options) (sheet : string) =
  (* TODO we're gonna want a spreadsheet functor. One or two?
          because we could parse input line-by line or read the
          whole file.
          I'm thinking we should work with the functor, and later improve
          that....
          AH, wait. Can't define an input functor. Okay, that makes things easy.
   *)
  failwith ""

(** [infer_columns ~sep sheet] Infer the special column names
    from the spreadsheet [sheet]. Prompt user to validate selection. *)
let infer_columns ~sep (sheet : string) : StringSet.t =
  let titles = get_titles_exn ~sep:sep sheet in
  List.fold_left (* Save capitalized titles *)
    ~f:(fun acc str ->
        if String.length str > 0 && ('A' <= str.[0] && str.[0] <= 'Z')
        then StringSet.add acc str
        else acc)
    ~init:StringSet.empty
    titles

(** [infer_delimeter f] Infer the delimeter string from a spreadsheet
    file by reading the extension. *)
let infer_delimeter (fname : string) : string =
  begin match get_extension fname with
    | Some "csv" -> ','
    | Some "tab" -> '\t'
    | Some _
    | None       -> ','
  end

(** [validate_columns ~sep ~sheet cols] Assert that each column name in [cols]
    actually appears as the title of a column in spreadsheet [~sheet]. *)
let validate_columns ~sep ~sheet (cols : string list) : StringSet.t =
  let titles = get_titles_exn ~sep:sep sheet in
  List.fold_left (* Filter invalid entries from 'cols'. *)
    ~f:(fun acc col ->
         if List.mem titles col
         then StringSet.add acc col
         else let () = Format.printf "[cms] WARNING: Ignoring invalid column name '%s'.\n" col in acc)
    ~init:StringSet.empty
    cols

let command =
  Command.basic
    ~summary:"Aggregate point totals and comments into a CMS-ready spreadsheet."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "This command parses an input spreadsheet, scraping the values from selected columns.";
      "These columns are copied to a new spreadsheet, along with any comments generated by";
      "the harness. Output is ready for immediate upload to CMS provided the column titles";
      "match the labeled assignment parts on the CMS assignment."
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose" no_arg            ~doc:" Print debugging information."
      +> flag ~aliases:["-i"] "-input"   (optional file)   ~doc:"DIR Specify the directory of harness-generated comments."
      +> flag ~aliases:["-s"] "-sep"     (optional string) ~doc:"STR Delimeter character (i.e. comma or tab) for the input sheet."
      +> flag ~aliases:["-o"] "-output"  (optional file)   ~doc:"FILE Write output to the file FILE."
      +> flag ~aliases:["-c"] "-column"  (listed string)   ~doc:"NAME Scrape the column titled NAME from the input spreadsheet."
      +> anon ("spreadsheet" %: string)
    )
    (fun v inp sep out cols sheet () ->
      let ()    = assert_file_exists ~msg:"Input spreadsheet does not exist! Bye now." sheet in
      let ()    = if v then Format.printf "[cms] Preparing to read spreadsheet '%s'.\n" sheet in
      let input = Option.value inp ~default:cHARNESS_SHEET in (* TODO remove constants *)
      let delim =  begin match sep with
                      | Some s -> Char.of_string s (* TODO error handling *)
                      | None   -> infer_delimeter sheet
                   end in
      let ()    = if v then Format.printf "[cms] Identified delimeter '%c'.\n" delim in
      let cols  = begin match cols with
                      | []   -> infer_columns    ~sep:delim input
                      | _::_ -> validate_columns ~sep:delim ~sheet:input cols
                  end in
      let ()    = if v then Format.printf "[cms] Target columns are [%s].\n" (String.concat ~sep:"; " (StringSet.to_list cols)) in
      let opts  = {
        columns            = cols;
        delimeter          = delim;
        input_spreadsheet  = input;
        output_spreadsheet = Option.value out ~default:"./cms_spreadsheet.csv";
        verbose            = v;
      } in
      cms opts sheet
    )
