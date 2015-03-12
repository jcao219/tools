install: /usr/local/share/texmf/latex/cs3110

/usr/local/share/texmf/latex/cs3110: tex/latex/cs3110
	sudo rm -rf $@
	sudo ln -s $(shell pwd)/$< $(dir $@)
	sudo mktexlsr

