#!/bin/sh
# Generates inspiration.ml from .inspirations.fortune

awk '
BEGIN { RS="\f" }
{
	print "(* THIS FILE IS AUTO-GENERATED. Any manual changes will be overwritten. *)\n"
	print "open Core.Std"
	print "open Cli_util"
	print "open Process_util\n"
	print "let inspirations = [|";
	n = split($0, inspirations, /%\n/);
	for (i = 1; i <= n; i++) {
		gsub(/"/, "\\\"", inspirations[i])
		gsub(/\n/, "\\n", inspirations[i])
		print "\"" inspirations[i] "\";";
	}
	print "|]\n";
	print "let print_inspiration () ="
	print "  let _ = Random.self_init () in"
	print "  let rand_idx = Random.int (Array.length inspirations) in"
	print "  print_string inspirations.(rand_idx)\n"
	print "let command ="
	print "  Command.basic"
	print "  ~summary:\"Gives a healthy dose of inspiration to the weary CS 3110 champion\" "
	print "  ~readme:(fun () -> \"Gives a healthy does of inspiration to the weary CS 3110 champion\")"
	print "  Command.Spec.("
	print "    empty"
	print "  )"
	print "  (fun () -> print_inspiration ())"
}
' < inspirations.fortune > inspiration.ml

echo "val print_inspiration : unit -> unit" >> inspiration.mli
echo "val command : Core.Std.Command.t" >> inspiration.mli