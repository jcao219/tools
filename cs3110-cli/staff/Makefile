LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

SRC=constants.mli constants.ml \
iOUtil.mli iOUtil.ml \
processUtil.mli processUtil.ml \
filepathUtil.mli filepathUtil.ml \
postscript.mli postscript.ml

all: compile clean

assertions:
	ocamlfind ocamlc -package qcheck -a -o assertions.cma serializer.mli assertions.mli serializer.ml assertions.ml
	mv assertions.cmi assertions.cmo assertions.cma ${STD_LIB}
	rm serializer.cmi serializer.cmo

compile:
	ocamlc -o cs3110-staff unix.cma $(SRC) cs3110_cli.mli cs3110_cli.ml

clean:
	rm -f *.cm*

install: assertions compile clean

uninstall:
	rm cs3110-staff
	rm ${STD_LIB}/assertions.cmi
	rm ${STD_LIB}/assertions.cmo
	rm ${STD_LIB}/assertions.cma

link:
	ln -s `pwd`/cs3110-staff $(LINK)

unlink:
	rm $(LINK)
