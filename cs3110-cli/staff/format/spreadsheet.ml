open Core.Std

exception Invalid_spreadsheet of string

module type SpecType = sig
  type row

  val compare_row : row -> row -> int
  val row_of_string : string -> row
  val string_of_row : row -> string
  val title : string
end

module Make =
  functor (Spec : SpecType) -> struct

    module RowSet = Set.Make(struct
      type t = Spec.row
      let compare        = Spec.compare_row
      let t_of_sexp sexp = Spec.row_of_string (Sexp.to_string sexp)
      let sexp_of_t r    = Sexp.of_string (Spec.string_of_row r)
    end)

    type t = RowSet.t

    let add_row (sheet : t) ~row =
      if RowSet.mem sheet row then
        let sheet' = RowSet.remove sheet row in
        RowSet.add sheet' row
      else
        RowSet.add sheet row

    let count_rows (sheet : t) : int =
      RowSet.length sheet

    let create () =
      RowSet.empty

    let read (fname : string) : t =
      let lines = (* skip the title *)
        List.map
          ~f:Spec.row_of_string
          (List.tl_exn (In_channel.read_lines fname))
      in
      RowSet.of_list lines

    let write ~filename (sheet : t) : unit =
      let rows =
        List.map
          ~f:Spec.string_of_row
          (RowSet.to_list sheet)
      in
      try Out_channel.write_lines filename (Spec.title :: rows)
      with Sys_error _ ->
        let msg = Format.sprintf "Could not write to file '%s'. Make sure the containing directory exists." filename in
        raise (Sys_error msg)

  end
