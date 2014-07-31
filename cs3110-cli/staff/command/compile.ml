open Cli_util
open Core.Std
open Process_util

type options = Cli_config.compile_command_options

(* Default compiler flags. These are Jane Street's settings.
   [-warn-error] says "treat the following warnings as errors".
   [+a] means "all warnings"
   [-4-33-34-39-40-41-42-43-44] means "except these"
   - [4]  : Fragile pattern matching (uses _)
   - [33] : Unused open statement
   - [34] : Unused type declaration
   - [39] : Unused rec flag
   - [40] : Constructor or label name used out of scope
   - [41] : Ambiguous constructor label or name
   - [42] : Disambiguated constructor label or name
   - [43] : Nonoptional label applied as optional
   - [44] : Open statement shadows an identifier
   Note that warnings 40-45 are for [ocamlopt] and not for [ocamlc].
   See more documentation here: http://caml.inria.fr/pub/docs/manual-ocaml/native.html *)
let default_compiler_flags = [
  "-warn-error";
  "+a-4-33-34-40-41-42-43-44";
]
let default_ocamlbuild_flags = [
  "-use-ocamlfind";
  "-no-links"
]

(** [assert_ocamlbuild_friendly_filepath path] Make sure the target is
    somewhere that [ocamlbuild] can find. To quote: "ocamlbuild must be invoked from the root of the project"
    {href {http://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html}} *)
let assert_ocamlbuild_friendly_filepath (path : string) : unit =
  let is_relative =
    try let _ = Str.search_forward (Str.regexp "\\.\\./") path 0 in true
    with Not_found -> false
  in
  let is_absolute = String.contains path '~' || path.[0] = '/' in
  if (is_absolute || is_relative)
  then raise (Invalid_filepath "Must call cs3110 from the project root. Absolute or relative paths are not allowed.")

(** [format_compiler_flags fs] prepend the string '-cflag' to each compiler
    flag for compliance with ocamlbuild *)
let format_compiler_flags (flags : string list) : string list =
  List.fold_right
    ~f:(fun f acc -> "-cflag" :: f :: acc)
    ~init:[]
    flags

(** [format_dependencies deps] Prepare the set of dependencies [deps]
    to be passed into the ocamlbuild command line. *)
let format_includes (includes : StringSet.t) : string list =
  begin match StringSet.to_list includes with
    | []  -> []
    | [x] -> ["-I"; x]
    | xs  -> ["-Is"; (String.concat ~sep:"," xs)]
  end

(** [format_libraries libs] Prepare the set of libraries [libs]
    to be passed in to the ocamlbuild command line. *)
let format_libraries (libs : StringSet.t) : string list =
  begin match StringSet.to_list libs with
    | [] -> ["-lib"; "assertions"]
    | xs -> ["-libs"; "assertions," ^ (String.concat ~sep:"," xs)]
  end

(** [format_ocamlbuild_flags ?mktop pkgs] Prepare the set of opam packages [pkgs]
    to be passed in to the ocamlbuild command line. Output differs if we are
    making an executable toplevel i.e. when [mktop] is true. *)
let format_ocamlbuild_flags ?(mktop=false) ?(thread=false) (pkgs : StringSet.t) : string list =
  if mktop then
    let pkgs = begin match StringSet.to_list pkgs with
                 | []  -> []
                 | [x] -> ["-pkg"; x]
                 | xs  -> ["-pkgs"; (String.concat ~sep:"," xs)]
               end
    in default_ocamlbuild_flags @ pkgs
  else
    let pkgs_str =
      begin match StringSet.to_list pkgs with
        | [] -> if thread then "thread" else ""
        | xs ->
           let pkgs = String.concat ~sep:", " (List.map ~f:(Format.sprintf "package(%s)") xs) in
           if thread then "thread, " ^ pkgs else pkgs
      end
    in
    default_ocamlbuild_flags @ [
      "-tag-line";
      "<*.ml{,i}> : syntax(camlp4o), " ^ pkgs_str;
      "-tag-line";
      "<*.d.byte> : "                  ^ pkgs_str;
      "-tag-line";
      "<*.native> : "                  ^ pkgs_str;
    ]

(** [get_target ?mktop main] Return the compilation target
    for module [main]. If [mktop] is true, prepare files to
    create a custom toplevel. *)
let get_target ?(mktop=false) (main : string) : string =
  let main = strip_suffix main in
  if mktop then (* Ensure that the .mltop file exists. *)
    let mltop   = Format.sprintf "%s.mltop" main in
    begin match Sys.file_exists mltop with
      | `Yes           -> Format.sprintf "%s.top" main
      | `No | `Unknown ->
        let prefix = "*  " in
        let border = String.make 80 '*' in
        let ()     = print_endline (String.concat ~sep:"\n" [
          border;
          Format.sprintf "%sERROR: Could not find file '%s'." prefix mltop;
          prefix ^ "A '.mltop' file is required to build a toplevel. This file should contain";
          prefix ^ "the names of all modules you want to include in the toplevel. For example,";
          prefix ^ Format.sprintf "the command 'echo '%s' > '%s' would create a minimal" main mltop;
          prefix ^ "valid '.mltop' file. If the module has dependencies, be sure to include";
          prefix ^ "those as well.";
          border
        ])
        in
        raise (File_not_found mltop)
    end
  else
    Format.sprintf "%s.d.byte" main

(** [compile ?q ?v ?m ?t ?d ?o main] compile [main] into a bytecode executable.
    Relies on ocamlbuild. *)
let compile ?(quiet=false) ?(verbose=false) ?(mktop=false) ?(thread=false) ?dir ?opts (main_module : string) : int =
  let opts      = (begin match opts with
                     | Some o -> o
                     | None   -> (Cli_config.init ()).compile
                   end : options)
  in
  let main      = ensure_ml main_module in
  let cwd       = Sys.getcwd () in
  let ()        = Sys.chdir (Option.value ~default:cwd dir) in
  let ()        = if verbose then Format.printf "[compile] Preparing to compile file '%s'\n" main in
  let target    = get_target ~mktop:mktop main in
  let ()        = if verbose then Format.printf "[compile] Target is '%s'\n" target in
  let deps      = format_includes opts.include_directories in
  let ()        = if verbose then Format.printf "[compile] Included directories are [%s]\n" (String.concat ~sep:"; " deps) in
  let libs      = format_libraries opts.ocaml_libraries in
  let ()        = if verbose then Format.printf "[compile] Linked libraries are [%s]\n"     (String.concat ~sep:"; " libs) in
  let cflags    = format_compiler_flags default_compiler_flags in (* 2014-07-30: ignores the config's compiler flags *)
  let ()        = if verbose then Format.printf "[compile] Compiler flags are [%s]\n"       (String.concat ~sep:"; " cflags) in
  let run_quiet = if quiet then ["-quiet"] else [] in
  let oflags    = format_ocamlbuild_flags ~mktop:mktop ~thread:thread opts.opam_packages in
  let ()        = if verbose then Format.printf "[compile] ocamlbuild flags are [%s]\n"     (String.concat ~sep:"; " oflags) in
  let command   = deps @ libs @ cflags @ run_quiet @ oflags @ [target] in
  (* 2014-07-23: Need to flush before ocamlbuild prints. *)
  let ()        = if verbose && (not quiet) then Format.printf "%!" in (* whitespace to combat ocamlbuild *)
  let exit_code = run_process "ocamlbuild" command in
  let ()        = Sys.chdir cwd in
  let ()        = if verbose && (not quiet) then Format.printf "\n" in (* more required whitespace for ocambuild *)
  let ()        = if verbose && (exit_code = 0) then Format.printf "[compile] Compilation succeeded!\n" in
  exit_code

let command =
  Command.basic
    ~summary:"Compiles into a bytecode executable. Relies on ocamlbuild."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The compile command is a wrapper for the ocamlbuild tool. It takes the";
      "input file, resolves dependencies, and compiles to a bytecode";
      "executable file. The object files produced during compilation are placed";
      "in a directory [_build], which will be created (if not already present)";
      "in the current working directory.";
      "You may pass a file in the current directory or a file in a sub-directory.";
      "";
      "Additionally, use this command for creating a custom OCaml toplevel with";
      "the '-mktop' option. Custom toplevels are '.top' files. To use one, change";
      "into the '_build' directory and execute it."
    ])
    Command.Spec.(
      empty
      +> flag ~aliases:["-q"]             "-quiet"    no_arg            ~doc:" Compile quietly."
      +> flag ~aliases:["-v"]             "-verbose"  no_arg            ~doc:" Print debugging information (about compiler options, etc.)."
      +> flag ~aliases:["-top"; "-mktop"] "-toplevel" no_arg            ~doc:" Create a custom toplevel (.top file) inside the '_build' directory instead of an executable."
      +> flag ~aliases:["-t"]             "-thread"   no_arg            ~doc:" Compile with threading libraries."
      +> flag ~aliases:["-I"]             "-include"  (listed file)     ~doc:"DIR Search the directory DIR recursively for dependencies."
      +> flag ~aliases:["-p"; "-pkg"]     "-package"  (listed string)   ~doc:"PKG Include the OPAM package PKG."
      +> flag ~aliases:["-l"; "-lib"]     "-library"  (listed string)   ~doc:"LIB Include the OCaml library LIB."
      +> anon ("target" %: file)
    )
    (fun q v mktop thread includes pkgs libs target () ->
      let ()   = assert_ocamlbuild_friendly_filepath target in
      let cfg  = Cli_config.init () in
      let opts = ({
        include_directories = begin match includes with
                                | []   -> cfg.compile.include_directories
                                | _::_ -> StringSet.of_list includes
                              end;
        opam_packages       = begin match pkgs with
                                | []   -> cfg.compile.opam_packages
                                | _::_ -> StringSet.of_list pkgs
                              end;
        ocaml_libraries     = begin match libs with
                                | []   -> cfg.compile.ocaml_libraries
                                | _::_ -> StringSet.of_list libs
                              end;
      } : options) in
      check_code (compile ~quiet:q ~verbose:v ~mktop:mktop ~thread:thread ~opts:opts target)
    )
