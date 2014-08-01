Util
====

Shared utilities for the command-line interface.

The module `cli_utils` contains a bunch of helper functions.
These are documented in `cli_oconfig.mli`.

The module `cli_config` contains the datatypes, defaults, constants shared thoughout the harness, along with parsing tools for the config files.

Config Files
------------

The config files can be stored in the home directory (indicated by the environment variable `HOME`) and/or the directory the cli executable is run in.
These (dot)files are always called `.cs3110` and contain a bunch of equalities, for example:

```
compile.include_directories = dir1, dir2, dir3
compile.ocaml_libraries = str
email.admins = professor@cs.cornell.edu, gradstudent@cornell.edu
```

The left side of each is an option from the record `Cli_config.t`.
These are written exactly as you'd access them in OCaml.
To set the directory that `diff` scrapes no-compiles from, use the option `diff.nocompile_directory`.

The right side is up to you.
Make sure the string has the right "type" otherwise the parser will throw some kind of error.
Lists can be separated by commas, semicolons, or whitespace; unfortunately they have to be on a single line.
