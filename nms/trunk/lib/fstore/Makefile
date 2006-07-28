SUBDIRS=src 

include ../../support/subdir.mk

include ./vsn.mk

APPNAME=fstore
DOC_OPTS=[{def,{vsn,"$(FSORE_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) ../utils/ebin -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
