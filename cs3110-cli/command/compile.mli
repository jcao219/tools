val compile : ?quiet:bool -> ?verbose:bool -> ?mktop:bool -> ?menhir:bool -> ?dir:string -> ?opts:Cli_config.compile_command_options -> string -> int
val command : Core.Std.Command.t
