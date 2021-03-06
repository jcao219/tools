open Core.Std
open Cli_util
open Process_util

type options = Cli_config.cms_command_options

(* used for pulling values from an input sheet *)
module LabeledRow = Set.Make(struct
  type t = string * string
  let compare (l1,_) (l2,_) = Pervasives.compare l1 l2
  let sexp_of_t _ = failwith "not implemented"
  let t_of_sexp _ = failwith "not implemented"
end)

type cms_row = string * LabeledRow.t * string

(** [get_titles_exn ~sep sheet] Return the first line from the file [sheet],
    split into a list on the character [~sep]. Raise an exception in the
    file [sheet] is empty. *)
let get_titles_exn ~sep (sheet : string) : string list =
  let in_chn   = In_channel.create sheet in
  let line_opt = In_channel.input_line in_chn in
  let ()       = In_channel.close in_chn in
  begin match line_opt with
    | Some ln -> String.split ~on:sep ln
    | None    ->
       let msg = Format.sprintf "Empty spreadhseet '%s'." sheet in
       raise (Spreadsheet.Invalid_spreadsheet msg)
  end

(** [parse_comments ?v o n] Convert the comments file for netid [n] into
    a spreadsheet-ready string. *)
let parse_comments ?(verbose=false) (opts : options) (netid : string) : string =
  let fname = Format.sprintf "%s/%s.md" opts.comments_directory netid in
  begin match Sys.file_exists fname with
    | `No | `Unknown ->
      let () = if verbose then Format.printf "[cms] Could not find comments for student '%s'.\n" netid in
      ""
    | `Yes           ->
      let lines = In_channel.read_lines fname in
      Format.sprintf "\"%s\"" (String.concat ~sep:" \n " lines)
  end

(** [parse_row_exn ?v opts ~titles line] Read the data values from a row [line] of the spreadsheet.
    Pull out the netid and filter the data columns and read the comments from an external file. *)
let parse_row_exn ?(verbose=false) (opts : options) ~titles (line : string) : cms_row =
  let ()       = if verbose then Format.printf "[cms] Reading line '%s'.\n" line in
  let data     = String.split ~on:opts.delimiter line in
  let ()       = if verbose then Format.printf "[cms] Matching values with titles.\n" in
  let ()       = if not ((List.length titles) = (List.length data))
                 then raise (Spreadsheet.Invalid_spreadsheet "Row does not match titles") in
  let lr       = List.fold2_exn titles data
                   ~f:(fun acc t d -> LabeledRow.add acc (t,d))
                   ~init:LabeledRow.empty
  in
  let ()       = if verbose then Format.printf "[cms] Extracting netid...\n" in
  let netid    = snd (LabeledRow.find_exn ~f:(fun (lbl,_) -> lbl = "NetID") lr) in
  let ()       = if verbose then Format.printf "[cms] Filtering columns...\n" in
  let scores   = LabeledRow.filter ~f:(fun (lbl,_) -> StringSet.mem opts.column_names lbl) lr in
  let ()       = if verbose then Format.printf "[cms] Reading comments...\n" in
  let comments = parse_comments opts netid in
  (netid, scores, comments)

(** [cms ?v o sheet] Read the spreadsheet [sheet] and extract
    particular columns. Save these columns along with
    harness-generated comments in a new spreadsheet. *)
let cms ?(verbose=false) (opts : options) (sheet : string) =
  let () = if verbose then Format.printf "[cms] Creating spreadsheet template...\n" in
  let module CmsSpreadsheet = Spreadsheet.Make(struct
      type row                          = cms_row
      let compare_row (l1,_,_) (l2,_,_) = Pervasives.compare l1 l2
      let row_of_string _               = failwith "CMS.row_of_string intentionally not implemented"
      let string_of_row (id, lrow, cs)  = let scores = LabeledRow.fold_right ~f:(fun (_,s) acc -> s::acc) ~init:[] lrow in
                                          String.concat ~sep:(Char.escaped opts.delimiter) ([id] @ scores @ [cs])
      let title                         = let names = StringSet.to_list opts.column_names in
                                          String.concat ~sep:(Char.escaped opts.delimiter) (["NetID"] @ names @ ["Add Comments"])
    end)
  in
  let ()        = if verbose then Format.printf "[cms] Parsing input file '%s'...\n" sheet in
  let in_chn    = In_channel.create sheet in
  let ()        = if verbose then Format.printf "[cms] Reading & saving column headers.\n" in
  let titles    = String.split ~on:opts.delimiter (Option.value_exn (In_channel.input_line in_chn)) in
  let ()        = if verbose then Format.printf "[cms] Reading & parsing spreadsheet body.\n" in
  let cms_sheet = In_channel.fold_lines in_chn
                    ~f:(fun acc ln -> CmsSpreadsheet.add_row acc ~row:(parse_row_exn ~verbose:verbose opts ~titles:titles ln))
                    ~init:(CmsSpreadsheet.create ())
  in
  let ()        = In_channel.close in_chn in
  let ()        = if verbose then Format.printf "[cms] Finished! Saving results to '%s'.\n" opts.output_spreadsheet in
  CmsSpreadsheet.write cms_sheet ~filename:opts.output_spreadsheet

(** [is_capitalized s] True if string [s] begins with a capital A-Z letter. *)
let is_capitalized (str : string) : bool =
  String.length str > 0 && ('A' <= str.[0] && str.[0] <= 'Z')

(** [infer_columns ~sep sheet] Infer the special column names
    from the spreadsheet [sheet]. *)
let infer_columns ~sep (sheet : string) : StringSet.t =
  let titles = get_titles_exn ~sep:sep sheet in
  List.fold_left (* Save capitalized titles *)
    ~f:(fun acc str ->
        if (is_capitalized str) && (str <> "NetID")
        then StringSet.add acc str
        else acc)
    ~init:StringSet.empty
    titles

(** [infer_delimiter f] Try inferring the delimiter string from
     a spreadsheet file by reading the file extension. *)
let infer_delimiter (fname : string) : char option =
  begin match get_extension fname with
    | Some "csv" -> Some ','
    | Some "tab" -> Some '\t'
    | Some _
    | None       -> None
  end

(** [get_delimiter cfg sheet sep_opt] Determine the delimiter for the spreadsheet file [sheet].
    First see if [sep_opt] was set at the command line. Fall back to checking the file extension,
    and finally the config file (which is, if nothing else, a comma). *)
let get_delimiter (cfg : Cli_config.t) (sheet : string) (sep_opt : string option) : char =
  begin match sep_opt with
    | Some s when String.length s = 1 -> (try Char.of_string s with _ -> failwith "Delimiter must be a single character.")
    | Some _ | None                   -> Option.value (infer_delimiter sheet) ~default:cfg.cms.delimiter
  end

(** [validate_columns ~sep ~sheet cols] Filter any column names in [cols] that
    do not actually appears as the title of a column in spreadsheet [~sheet]. *)
let validate_columns ~sep ~sheet (cols : StringSet.t) : StringSet.t =
  let title_set = StringSet.of_list (get_titles_exn ~sep:sep sheet) in
  (* Filter invalid entries from 'cols'. *)
  StringSet.fold cols
    ~f:(fun acc col ->
         if StringSet.mem title_set col
         then StringSet.add acc col
         else let () = Format.printf "[cms] WARNING: Ignoring invalid column name '%s'.\n" col in acc)
    ~init:StringSet.empty

(** [get_columns cfg delim sheet cols] Get the set of columns for the spreadsheet
    file [sheet]. First look for column names [cols] given on the command line,
    fall back to the config file and finally try parsing names from the spreadsheet
     itself. No matter what, ensure that acquired columns are in the actual sheet. *)
let get_columns (cfg : Cli_config.t) (delim : char) (sheet : string) (cols : string list) : StringSet.t =
  begin match cols with
    | _::_ ->
      validate_columns ~sep:delim ~sheet:sheet (StringSet.of_list cols)
    | []   ->
      if StringSet.is_empty cfg.cms.column_names
      then infer_columns ~sep:delim sheet
      else validate_columns ~sep:delim ~sheet:sheet cfg.cms.column_names
  end

let command =
  Command.basic
    ~summary:"Aggregate point totals and comments into a CMS-ready spreadsheet."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "Parses an input spreadsheet, scraping the values from selected columns..";
      "These columns are copied to a new spreadsheet, along with any comments generated by";
      "the harness. Output is ready for immediate upload to CMS provided the column titles";
      "match the labeled assignment parts on the CMS assignment."
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-v"] "-verbose"  no_arg           ~doc:" Print debugging information."
      +> flag ~aliases:["-i"] "-input"   (optional file)   ~doc:"DIR Specify the directory of harness-generated comments."
      +> flag ~aliases:["-s"] "-sep"     (optional string) ~doc:"STR Delimiter character (i.e. comma or tab) for the input sheet."
      +> flag ~aliases:["-o"] "-output"  (optional file)   ~doc:"FILE Write output to the file FILE."
      +> flag ~aliases:["-c"] "-column"  (listed string)   ~doc:"NAME Scrape the column titled NAME from the input spreadsheet."
      +> anon ("spreadsheet" %: string)
    )
    (fun v inp sep_opt out cols sheet () ->
      let cfg   = Cli_config.init () in
      let ()    = assert_file_exists ~msg:"Input spreadsheet does not exist! Bye now." sheet in
      let ()    = if v then Format.printf "[cms] Preparing to read spreadsheet '%s'.\n" sheet in
      let input = Option.value inp ~default:cfg.cms.comments_directory in
      let delim = get_delimiter cfg sheet sep_opt in
      let ()    = if v then Format.printf "[cms] Identified delimiter '%c'.\n" delim in
      let ()    = if not ("NetID" = List.hd_exn (get_titles_exn ~sep:delim sheet))
                  then raise (Spreadsheet.Invalid_spreadsheet "First column should be 'NetID'.") in
      let cols  = get_columns cfg delim sheet cols in
      let ()    = if v then Format.printf "[cms] Target columns are [%s].\n" (String.concat ~sep:"; " (StringSet.to_list cols)) in
      let opts  = ({
        column_names       = cols;
        comments_directory = input;
        delimiter          = delim;
        output_spreadsheet = Option.value out   ~default:cfg.cms.output_spreadsheet;
      } : options)
      in
      let () = assert_file_exists ~msg:"Comments directory does not exist. Bye now." opts.comments_directory in
      cms ~verbose:v opts sheet
    )
