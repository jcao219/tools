LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

# without the .PHONY, make thinks that these targets are built because folders of the same name exist
.PHONY: assertions inspiration

all: inspiration compile

assertions: unassertions
	corebuild -package qcheck -Is assertions,util assertions.cma
	mv _build/assertions/assertions.cmi _build/assertions/assertions.cmo _build/assertions/assertions.cma ${STD_LIB}

compile:
	corebuild -lib str -Is util,format,command cli_main.native
	mv cli_main.native cs3110

clean:
	corebuild -clean
	rm command/inspiration.*

inspiration: inspiration/.inspirations.fortune
	cd inspiration; ./make_inspiration_ml.sh; cd ..
	mv inspiration/inspiration.ml command/inspiration.ml
	mv inspiration/inspiration.mli command/inspiration.mli

install: assertions compile link 

unassertions:
	-rm ${STD_LIB}/assertions.cmi
	-rm ${STD_LIB}/assertions.cmo
	-rm ${STD_LIB}/assertions.cma

uninstall: unlink unassertions
	rm cs3110

link:
	ln -sf `pwd`/cs3110 $(LINK)cs3110

unlink:
	rm $(LINK)cs3110
