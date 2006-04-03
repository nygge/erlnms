SUBDIRS=src
include ../../support/subdir.mk

include ./vsn.mk

APPNAME=threshold
DOC_OPTS=[{def,{vsn,"$(THRESHOLD_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) ../utils/ebin -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
