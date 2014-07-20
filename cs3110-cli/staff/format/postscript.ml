type font = Normal | Header | Code
type t = Pervasives.out_channel

let string_of_font = function
  | Normal -> "Palatino-Roman10"
  | Header -> "Palatino-Bold10"
  | Code   -> "Courier-New10"

let close t : unit =
  Pervasives.flush t;
  ignore (Unix.close_process_out t)

(** [init f t] opens a stream to file [f] with title [t]
 * that pipes its input into a postscript-formatted file *)
let init (fname : string) (title : string) : out_channel =
  Unix.open_process_out (Format.sprintf "enscript --quiet -p %s -b '%s' -M Letter --fancy-header --escapes=\001 --no-formfeed" fname title)

(** [set_font c f] Resets the font of text output to channel [c] *)
let set_font t (font : font) : unit =
  output_string t (Format.sprintf "\n\001font{%s}" (string_of_font font))

let write t (msg : string) : unit =
  output_string t msg

let write_code t (src_fname : string) : unit =
  let () = set_font t Header in
  begin match Sys.file_exists src_fname with
    | true  ->
       let () = write t (Format.sprintf "Source code for file '%s':\n" src_fname) in
       let () = set_font t Code in
       List.iter ~f:(write t) (In_channel.read_lines src_fname)
    | false -> write t "SOURCE NOT FOUND\n"
  end

let write_results t (results_str : string) : unit =
  let () = set_font t Header in
  let () = write t "\nTest Results:\n" in
  let () = set_font t Normal in
  let () = write t results_str in
  ()
