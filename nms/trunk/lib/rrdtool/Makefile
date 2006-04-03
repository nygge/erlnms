SUBDIRS=src test/src

include ../../support/subdir.mk

include ./vsn.mk

APPNAME=rrdtool
DOC_OPTS=[{def,{vsn,"$(RRDTOOL_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) ../utils/ebin -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
