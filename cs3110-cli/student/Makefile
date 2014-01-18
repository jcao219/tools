LINK=/usr/local/bin/cs3110

all: compile clean

compile:
	ocamlc -o cs3110 unix.cma cs3110_cli.ml

clean:
	rm cs3110_cli.cmi
	rm cs3110_cli.cmo

uninstall:
	rm cs3110

link:
	ln -s `pwd`/cs3110 $(LINK)

unlink:
	rm $(LINK)
