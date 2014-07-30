LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

# without the .PHONY, make thinks that these targets are built because folders of the same name exist
.PHONY: assertions inspiration

all: compile

assertions: unassertions
	corebuild -package qcheck -Is assertions,include,util assertions.cma
	mv _build/assertions/assertions.cmi _build/assertions/assertions.cmo _build/assertions/assertions.cma ${STD_LIB}

compile:
	corebuild -lib str -Is util,format,command main.native
	mv main.native cs3110-staff

clean:
	corebuild -clean

inspiration: inspiration/.inspirations.fortune
	cd inspiration; ./make_inspiration_ml.sh; cd ..
	mv inspiration/inspiration.ml command/inspiration.ml

install: assertions compile link clean

unassertions:
	-rm ${STD_LIB}/assertions.cmi
	-rm ${STD_LIB}/assertions.cmo
	-rm ${STD_LIB}/assertions.cma

uninstall: unlink unassertions
	rm cs3110-staff

link:
	ln -sf `pwd`/cs3110-staff $(LINK)cs3110-staff

unlink:
	rm $(LINK)cs3110-staff
