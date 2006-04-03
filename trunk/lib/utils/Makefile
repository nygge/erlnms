include ../../support/subdir.mk

include ./vsn.mk

APPNAME=utils
DOC_OPTS=[{def,{vsn,"$(UTILS_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) ../utils/ebin -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
