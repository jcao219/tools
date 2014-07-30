#!/bin/sh

awk '
BEGIN { RS="\f" }
{
	print "(* THIS FILE IS AUTO-GENERATED. Any manual changes will be overwritten. *)"
	print ""
	print "let inspirations = [|";
	n = split($0, inspirations, /%\n/);
	for (i = 1; i <= n; i++) {
		ocamlstr = inspirations[i];
		gsub(/"/, "'"'"'", ocamlstr)
		gsub(/\n/, "\n", ocamlstr)
		print "\"" ocamlstr "\";";
	}
	print "|]";
	print ""
	print "let print_inspiration () ="
	print "  let _ = Random.self_init () in"
	print "  let rand_idx = Random.int (Array.length inspirations) in"
	print "  print_string inspirations.(rand_idx)"
}
' < .inspirations.fortune > inspiration.ml
