LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

.PHONY: assertions

all: compile

assertions:
	ocamlfind ocamlc -package qcheck -I assertions -I include -I util -a -o assertions/assertions.cma include/serializer.mli util/cli_constants.ml assertions/assertions.mli include/serializer.ml assertions/assertions.ml
	mv assertions/assertions.cmi assertions/assertions.cmo assertions/assertions.cma ${STD_LIB}
	rm include/serializer.cmi include/serializer.cmo

compile:
	ocamlc -o cs3110-staff -I util -I format -I command str.cma unix.cma util/filepath_util.mli format/postscript.mli format/grades_table.mli command/cms.mli command/harness.mli command/smoke.mli util/cli_constants.ml util/io_util.ml util/filepath_util.ml util/process_util.ml format/postscript.ml format/grades_table.ml command/build.ml command/clean.ml command/cms.ml command/doc.ml command/diff.ml command/email.ml command/test.ml command/harness.ml command/run.ml command/smoke.ml command/stat.ml cs3110_cli.ml

clean:
	corebuild -clean
	rm -rf _cms

install: assertions compile link clean

uninstall:
	rm cs3110-staff
	rm ${STD_LIB}/assertions.cmi
	rm ${STD_LIB}/assertions.cmo
	rm ${STD_LIB}/assertions.cma

link:
	ln -sf `pwd`/cs3110-staff $(LINK)cs3110-staff

unlink:
	rm $(LINK)cs3110-staff
