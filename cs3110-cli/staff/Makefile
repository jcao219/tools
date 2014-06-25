LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

all: compile

assertions:
	ocamlfind ocamlc -package qcheck -I util -a -o assertions.cma serializer.mli util/cli_constants.ml assertions.mli serializer.ml assertions.ml
	mv assertions.cmi assertions.cmo assertions.cma ${STD_LIB}
	rm serializer.cmi serializer.cmo

compile:
	corebuild -Is util,assertions,format,include,command main.native
	mv main.native cs3110-staff

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
