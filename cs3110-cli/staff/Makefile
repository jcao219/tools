LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

.PHONY: assertions

all: compile

assertions:
	corebuild -package qcheck -Is assertions,include,util assertions.cma
	mv _build/assertions/assertions.cmi _build/assertions/assertions.cmo _build/assertions/assertions.cma ${STD_LIB}

compile:
	corebuild -Is command,format,include,util -libs str main.native
	mv main.native cs3110-staff

clean:
	corebuild -clean

install: assertions compile link clean

uninstall: unlink
	rm cs3110-staff
	rm ${STD_LIB}/assertions.cmi
	rm ${STD_LIB}/assertions.cmo
	rm ${STD_LIB}/assertions.cma

link:
	ln -sf `pwd`/cs3110-staff $(LINK)cs3110-staff

unlink:
	rm $(LINK)cs3110-staff
