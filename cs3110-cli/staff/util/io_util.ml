(** [fold_in_channel f acc c] Analog to List.fold_left, just for in_channels.
 * Calls [f] on built-up accumulator and each line of the file.
 * [f ( ... ( f acc line_1 ) ... ) line_n] *)
let rec fold_in_channel (f : 'a -> string -> 'a) (acc : 'a) (chn : in_channel) : 'a =
  try 
    fold_in_channel f (f acc (input_line chn)) chn
  with End_of_file -> 
    let _ = close_in chn in 
    acc

(** [read_lines c] reads all lines from channel [c] into a list *)
let read_lines (chn : in_channel) : string list =
  List.rev (fold_in_channel (fun acc s -> s::acc) [] chn)

(** [csv_of_file filename] reads all lines in [filename] and concatenates them
 * with commas *)
let csv_of_file (filename : string) : string =
  let chn = open_in filename in
  String.concat "," (read_lines chn)
