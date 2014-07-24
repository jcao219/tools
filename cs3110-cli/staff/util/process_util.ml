(** [return_code_of_exit_status st] Convert the process_status into an int *)
let return_code_of_exit_status (st : Unix.process_status) : int =
  match st with
    | Unix.WEXITED exit_code ->
      exit_code
    | Unix.WSIGNALED n ->
      let () = Format.printf "Sub-process killed (signal %d)\n%!" n in
      -1
    | Unix.WSTOPPED n ->
      let () = Format.printf "Sub-process stopped (signal %d)\n%!" n in
      -1

(** [run_process f args] Runs an executable and waits for termination, returning
    an integer exit code. If the process is killed or stopped,
    prints a warning & returns code -1. *)
let run_process (filename : string) (args : string list) : int =
  let open Unix in
  let pid = create_process filename (Array.of_list (filename :: args))
    stdin stdout stderr in
  let (_, exit_status) = waitpid [] pid in
  return_code_of_exit_status exit_status

(** [check_code c] exit uncleanly if [c] indicates an error *)
let check_code (return_code : int) : unit =
  if return_code <> 0 then begin
    let () = Format.printf "Error (exit code %d)\n%!" return_code in
    exit return_code
  end
