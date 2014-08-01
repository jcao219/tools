cs3110-cli
==========

Command line tools for cs3110.
The solution for compiling, testing, and running your code. 
Commands are:

* `cs3110 compile <filename>` compile the target file into a bytecode executable
* `cs3110 run <filename> [args]` Run the executable created by `cs3110 compile <filename>` with the supplied arguments
* `cs3110 test <filename>` Execute the `pa_ounit` tests within the file
* `cs3110 clean` remove the files generated through compilation
* `cs3110 help` print a summary of the available commands
* `cs3110 inspiration` provides an inspirational quote from past cs3110 staff and professors

Requirements
============

* [GNU Make](http://www.gnu.org/software/make/)
* [OCaml 4.01.0](http://caml.inria.fr/ocaml/release.en.html)
* [OPAM](http://opam.ocamlpro.com/)
* [ocamlfind](http://opam.ocamlpro.com/pkg/ocamlfind/1.4.0/)
* [oUnit](http://opam.ocamlpro.com/pkg/ounit/2.0.0/) 
* [pa_ounit](http://opam.ocamlpro.com/pkg/pa_ounit/109.36.00/)
* [qcheck](http://opam.ocamlpro.com/pkg/qcheck/qcheck.0.2/)

Once you have installed OCaml and OPAM, you should be able to download the remaining dependencies by executing `opam install <package-name>`, where `<package-name>` is one of `ocamlfind`, `ounit`, or `pa_ounit`.
Detailed instructions for installing Make, OCaml, and OPAM are included below, in the FAQ.

Installation
============

Download the script and the dependencies.
Execute `make install`.
This creates the supporting assertions library, the `cs3110` executable, and links it to `/usr/local/lib`.

Note that linking is convenient, but optional.
You can alternatively call the `cs3110` executable with the absolute path or extend your `PATH` environment variable to include the containing directory.

Removal
=======

Run `make uninstall`. 
If you modified your path earlier, you should delete that portion of your shell init file. 

You may delete the `cs3110-cli` directory too, but that's arguably overkill.

Usage
=====

The following is a more detailed overview of the interesting commands.

compile
-------

`cs3110 compile file.ml` compiles `file.ml` into an executable `file.d.byte`.
At it's most basic (if `file.ml` has no dependencies outside the standard library), this is nearly a synonym for `ocamlc -o file.d.byte file.ml`.
(The flag `-o file.d.byte` tells the compiler to name the generated file `file.d.byte` instead of the default `a.out`.)
But if there were two files, `file_a.ml` and `file_b.ml` such that `file_b` accessed the contents of `file_a`, then `ocaml -o file.d.btye file_b.btye` would fail.
`cs3110 compile file_b.ml`, besides being less typing, uses [ocamlbuild](http://caml.inria.fr/pub/docs/manual-ocaml-400/manual032.html) to discover the dependency on `file_a` and then compiles both files.

Additionally, this command puts all generated files into a folder named `_build`.
This way the compiled object and interfaces `file.cmo` and `file.cmi` are kept separate from the source code.
The executable `file.d.byte` is also stored in this folder.

For larger projects with a more complex file hierarchy, `cs3110 compile` relies on a file `.depend` to list all the sub-directories that need to be included during compilation.
This is because ocamlbuild does not traverse sub-directories by default.
It must be told explicitly where to look.
At any rate, we will supply a `.depend` file for all projects in this course, so you should not need to worry about this.

Similarly, the `.lib` file tells `ocamlbuild` which non-standard library files to use.
Provided these files exist on the `PATH`, they will be made available to compilation targets.
Again, we will supply the necessary `.lib` files for all projects.

run
---

`cs3110 run file.ml arg1` executes the bytecode executable `_build/file.d.byte` with argument `arg1`.
This is exactly the same as running `./_build/file.d.byte arg1`.
It just executes the file.
The benefit to using the `cs3110` script is that you no longer need to type the `_build` every time and that it is nice to have one command that supplies all the tools for the course.

test
----

The above documentation for `compile` said that `cs3110 compile file.ml` was nearly identical to `ocamlc -o file.d.byte file.ml` for a dependency-free `file.ml`.
The first difference is that the output from `cs3110 compile` goes into the `_build` folder.
The second difference is that `cs3110 compile` supports the syntax extensions defined by `pa_ounit`.
This makes [unit testing](http://en.wikipedia.org/wiki/Unit_testing) much easier. 

As decribed in the project's [documentation](https://github.com/janestreet/pa_ounit/blob/master/readme.md), `pa_ounit` introduces three new syntactic constructs to support inline unit testing.

    TEST name? = <boolean expr> (* true means ok, false or exn means broken *)
    TEST_UNIT name? = <unit expr> (* () means ok, exn means broken *)
    TEST_MODULE name? = <module expr> (* to group TESTs (to share some setup for instance) *)

You may write expressions of this form directly into your OCaml modules (.ml files).
`cs3110 compile` understands how to handle these constructs, and `cs3110 test` will execute each of the unit tests as they are encountered during an execution of the bytecode executable.
`ocamlc` does not support this syntax and will raise an error if used to compile a file with these inline tests.
(For this reason, we recommend keeping tests separate from source code. Tests for `file.ml` should be placed inside a separate file `file_test.ml` such that `file.ml` could be compiled without linking the `pa_ounit` library.)

We strongly urge you to write unit tests early and often to check the correctness of your code.
In fact, this functionality is the primary reason we supply the `cs3110` script.
Quickly compiling a module and running tests will prevent serious or hard-to-identify bugs from appearing during development, and keeping a comprehensive test suite handy gives assurance that later changes to the codebase did not break the implementation.
Writing unit tests is guaranteed to improve your grade in cs3110.

Assertions
==========

This testing framework comes with a simple library, `assertions.ml`, that includes functions designed to augment the standard `assert` statement.
When running the `cs3110` executable, your programs may reference `Assertions` as they would `List` or `String` or any other standard OCaml library.
Use of this library is optional, but we document it here.

Many of these commands print their arguments on failure.
This is done by the functions in `serializer.ml`.
They look at the internal representation of OCaml values and attempt to print a string representation.

Unfortunately, the printer is not entirely accurate.
It executes at runtime and thus does not have enough information to generate a perfect representation.
For one, it is not clear how to print user-defined types.
But simple values like `0`, `false`, and `[]` all print to the same string, `0`.

The OCaml Toplevel is able to discern and appropriately print these values because it can use AST information to print.
Our serializer cannot, so keep this in mind when reading its error messages.

assert_true
-----------
Almost the same as OCaml's `assert` statement.
Accepts a boolean argument and returns unit if it is true and raises an exception otherwise.

assert_false
------------
The opposite of `assert_true`.
Raises an exception if the argument is `false` and returns unit otherwise.

assert_greater
--------------
Checks whether its first argument is greater (using `(>)`) than its second argument.
If true, returns unit.
If false, prints its arguments with a helpful error mesage.

assert_less
-----------
The opposite of `assert_greater`.
Checks whether the first argument is less (using `(<)`) than the second.
Raises a helpful exception if not and return unit if so.

assert_equal
------------
Checks whether its arguments are equal using `=`.
If so, returns unit.
If not, raises and exception, printing its arguments.

assert_almost_equal
-------------------
Compares two floating point numbers for equality with 4-digit precision.
Returns unit if they are the same within this epsilon and raises an exception otherwise.

assert_not_equal
----------------
Checks whether its arguments are unequal using `<>`.
Returns unit if true and raises an exception otherwise.

assert_is
---------
Checks whether its arguments are the same object.
Similar to `assert_equal`, but uses `==` for equality.

assert_is_not
-------------
Opposite of `assert_is`.

assert_raises
-------------
At a high level, this function checks whether executing a function raises an exception.
Requires 3 arguments.

The first is an option, indicating which exception should be raised.
`None` means "raise any exception".
This is useful for catching functions that are supposed to `failwith` an arbitrary argument, or when the exception raised does not matter.
`Some Sys_error` means "catch `Sys_error` exceptions, returning unit if one is raised, and raise an error if execution completes without exception or any other exception is raised".

The second is a function of type `'a ->'b` and the third is a value of type `'a`.
The function is applied to the argument inside the body of `assert_raises`.

If you want to use a function that takes multiple arguments, partially apply it until it matches the form `'a -> 'b`.
For example, consider `List.find 0 [1;2]`, which should raise the exception `Not_found`.
One should call `assert_raises (Some Not_found) (List.find 0) ([1;2])` to test this behavior.

timeout
-------
Not an assertion per se, but still useful.
Checks whether executing a function finishes within a set amount of time.
Requires 3 arguments.

First is an integer, the timeout in seconds.
Second is a function of type `'a -> 'b` and third is a value of type `'a`.

Uses system alarms to time the the current process, then applies the second and third arguments as the timer diminishes.
Returns unit if the function terminates in time and raises `Timeout` otherwise.

FAQ
===

Q. How do I install GNU Make?
-----------------------------
Make should be installed by default in most Linux distributions. 
However, osx does not include it.
Either install the Apple Developer Tools through XCode or use the [osx-gcc-installer](https://github.com/kennethreitz/osx-gcc-installer)

Q. How do I install OCaml and OPAM?
--------------------------
On osx, you can use brew directly. `brew update; brew install ocaml opam`.

On linux, you will likely need to add a new repository to your package manager.
Using a Debian-based distribution, try:
```
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install ocaml opam
```

Now you have OCaml fully installed, and should be able to execute `ocaml -version` and see the output `The OCaml toplevel, version 4.01.0`.
OPAM is partially installed.
To finish, you need to run:
```
opam init
eval `opam config env`
```
The first command initializes OPAM on your machine and asks to add a line to your shell init file.
The second command exports a few environment variables for OPAM.
These lines should be added to your shell init file as well.
You may do this manually, or with the command `opam config env >> ~/<your-shell-init-file>` (the `>>` operator adds the output of the left hand side argument to the end of the right hand side file).

Q. Command not found: add-apt-repository
----------------------------------------
On Ubuntu, the `python-software-properties` package is a dependency for `add-apt-repository`.
Run the following command and try to add your repository again.
```
apt-get install python-software-properties
```

Q. Cannot link OPAM via brew
----------------------------

If `brew link opam` fails, you may need to edit the permissions on your `/usr/bin` folder.
Try running `chown -R $USER:admin /usr/local`.

Q. Cannot install ocamlfind through OPAM. m4 not in PATH
--------------------------------------------------------
You likely need to install `m4`.
Use your package manager, for example `apt-get install m4`.

Q. ocamlfind cannot find package "oUnit"
----------------------------------------

First, make certain `oUnit` was installed through OPAM.
Execute `opam search ounit`.
You should see output like:

    Available packages for system:
    alcotest         --  Alcotest is a lightweight and colourful test framework
    ounit         2.0.0  Unit testing framework loosely based on HUnit. It is similar to JUnit, and other XUnit testing frameworks
    pa_ounit  109.53.02  Syntax extension for oUnit
    qtest            --  Inline unit test extractor

The key points here are that ounit and pa_ounit have up-to-date version numbers.

If this information is correct, try reinstalling ocamlfind.

    opam remove ocamlfind
    opam install ocamlfind

If this fails, there may be an issue with case sensitivity on the folder named 'oUnit'.
<!-- This was an issue on bkc39's mac. Changing it to 'ounit' fixed him but broke blg59's arch -->
Another possibility is that oUnit was installed in a location where ocamlfind cannot locate it.
Either way, you should seek help in office hours or on Piazza. 

Q. What is a package manager?
-----------------------------
A package manager is software that manages the installation and maintenance of other software on your computer.
OPAM is one example of a package manager.
It lets you search for, install, and update other programs easily.

`aptitude`, which supplies `apt-get`, is the default on Ubuntu.
Arch has `pacman`, Fedora has `yum`, and so on.
OSX does not come with a default package manager, but we highly recommend [Homebrew](http://brew.sh/).
Brew is very easy to install: just enter the command at the bottom of their website.

Q. What is a shell init file?
-----------------------------
A shell init file is the first thing run when starting a command line session. 
Before the prompt appears to accept commands, init files are run to configure variables and set preferences.
The ones we reference in this readme are located in the home folder of your computer (a.k.a ~).
If you are using the [bash](http://www.gnu.org/software/bash/) shell, which is available by default on most Unix systems, your shell init file is called `~/.bashrc`, that is, the file called `.bashrc` which is located in the home folder `~/`.

The `.` at the end of the file signals that it should be hidden on the computer, so it will not appear when you `ls` in the home folder, nor when you use a graphical tool like Finder to view the contents of the home folder.
Use `ls -a` or configure your explorer to show hidden files.

Q. Could not run `make install`. Making the link failed with 'Permission denied'.
---------------------------------------------------------------------------------

You did not have permission to edit the file named by the variable `LINK` at the top of the Makefile.
Either change this variable to a different file or re-run `make install` with root permissions.

Q. Could not run `make install`. Making the link failed with 'No such file or directory'.
-----------------------------------------------------------------------------------------

The file named by the variable `LINK` (defined at the top of the Makefile) does not exist on your machine.
You can either make the file or re-define `LINK` to one that exists.
Ask on Piazza or come to office hours for help.

Q. What is root? What is "sudo"? What are root permissions? What is the "superuser"?
------------------------------------------------------------------------------------

On unix-like computers (OSX and all Linux distributions, including the cs3110 virtual machine), many operations are restricted.
Ordinary users may not alter files or directories in certain locations of the computer.
This is for security reasons.
Recklessly altering certain files may be dangerous to your computer.

The "root" or "superuser" is a special user who may edit all files on the computer, even the protected ones.
However, a user sometimes wants to perform actions the computer thinks are dangerous.
For example, only root can install new software.
So there is a mechanism for a user to obtain root permissions; that is, to edit protected files.
This is done by prepending "sudo" to the desired command.
"sudo" asks for permission to perform a restricted action.

As an example, `apt-get` is the ubuntu package manager.
It helps install new software and manage existing software.
To install ocaml, a user would type:
    $ apt-get install ocaml
However, this is a restricted operation.
Only the superuser may install new software.
Thus one more often uses this command to install ocaml:
    $ sudo apt-get install ocaml
Which means "execute the command `apt-get install ocaml` with root permissions.

Credits
=======

We thank [Arjun Guha](https://github.com/arjunguha/cs691f) at Brown University, [Jane Street](https://github.com/janestreet/pa_ounit), and [OUnit](https://github.com/warrenharris/ounit).
