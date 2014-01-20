LINK=/usr/local/bin/cs3110
STD_LIB=`ocamlc -where`

all: compile clean

assertions:
	ocamlc -a -o assertions.cma serializer.mli assertions.mli serializer.ml assertions.ml
	mv assertions.cmi assertions.cmo assertions.cma ${STD_LIB}
	rm serializer.cmi serializer.cmo

compile:
	ocamlc -o cs3110 unix.cma cs3110_cli.mli cs3110_cli.ml

clean:
	rm cs3110_cli.cmi
	rm cs3110_cli.cmo

install: assertions compile clean

uninstall:
	rm cs3110
	rm ${STD_LIB}/assertions.cmi
	rm ${STD_LIB}/assertions.cmo
	rm ${STD_LIB}/assertions.cma

link:
	ln -s `pwd`/cs3110 $(LINK)

unlink:
	rm $(LINK)
