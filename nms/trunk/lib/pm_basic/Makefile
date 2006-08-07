SUBDIRS=src test/src

include ../../support/subdir.mk

include ./vsn.mk

APPNAME=pm_basic
DOC_OPTS=[{def,{vsn,"$(PM_BASIC_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) ../utils/ebin -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
