#!/bin/sh

gawk '
BEGIN { RS="\0" }
{
	print "let inspirations = [|";
	n = split($0, inspirations, /%\n/);
	for (i = 1; i <= n; i++) {
		ocamlstr = gensub(/"/, "\\\\\"", "g", inspirations[i]);
		ocamlstr = gensub(/\n/, "\\\\n", "g", ocamlstr);
		print "\"" ocamlstr "\";";
	}
	print "|]";
	print ""
	print "let print_inspiration () ="
	print "  let _ = Random.self_init () in"
	print "  let rand_idx = Random.int (Array.length inspirations) in"
	print "  print_string inspirations.(rand_idx)"
}
' < inspirations.fortune > inspiration.ml
