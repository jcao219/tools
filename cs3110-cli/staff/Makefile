LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

all: compile clean

assertions:
	ocamlfind ocamlc -package qcheck -I util -a -o assertions.cma serializer.mli util/constants.ml assertions.mli serializer.ml assertions.ml
	mv assertions.cmi assertions.cmo assertions.cma ${STD_LIB}
	rm serializer.cmi serializer.cmo

compile:
	ocamlc -o cs3110-staff -I ./util -I ./format -I ./command str.cma unix.cma util/filepath_util.mli format/postscript.mli format/grades_table.mli command/cms.mli command/harness.mli command/smoke.mli util/constants.ml util/io_util.ml util/filepath_util.ml util/process_util.ml format/postscript.ml format/grades_table.ml command/build.ml command/clean.ml command/cms.ml command/doc.ml command/diff.ml command/email.ml command/test.ml command/harness.ml command/run.ml command/smoke.ml command/stat.ml cs3110_cli.ml

clean:
	find . -name '*.cm*' | xargs rm -f 

install: assertions compile link clean

uninstall:
	rm cs3110-staff
	rm ${STD_LIB}/assertions.cmi
	rm ${STD_LIB}/assertions.cmo
	rm ${STD_LIB}/assertions.cma

link:
	ln -s `pwd`/cs3110-staff $(LINK)

unlink:
	rm $(LINK)
