# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

#include ../common.mk
EBIN = ../ebin
INCLUDE = ../include 
DOCDIR = ../doc

ERL = erl
ERLC = erlc
#ETAGS = etags
EMULATOR = beam

EDOC_OPTS = [{title,"Utility library"}]

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

EXTERNAL_HEADERS = 

INTERNAL_HEADERS = 

ERL_FILES = $(wildcard *.erl)
HRL_FILES = $(EXTERNAL_HEADERS:%=$(INCLUDE)/%.hrl) $(INTERNAL_HEADERS:%=$(INCLUDE)/%.hrl)

DOC_TARGET_FILES = $(EDOC_MODULES:%=$(DOCDIR)/%.html)
TARGET_FILES = $(patsubst %.erl,$(EBIN)/%.$(EMULATOR),$(ERL_FILES))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I $(INCLUDE)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

doc:	
	$(ERL) -noshell $(EDOC_PATHS) -run edoc_run application \
		"'$(APPNAME)'" '".."' '$(EDOC_OPTS)' -s erlang halt

opt build: $(TARGET_FILES) 

clean:
	$(RM) $(TARGET_FILES)

realclean: clean
	$(RM) $(DOC_TARGET_FILES)
	$(RM) $(DOCDIR)/edoc-info
	$(RM) $(DOCDIR)/index.html
	$(RM) $(DOCDIR)/modules-frame.html
	$(RM) $(DOCDIR)/overview-summary.html
	$(RM) $(DOCDIR)/packages-frame.html
	$(RM) $(DOCDIR)/stylesheet.css


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(EBIN)/%.$(EMULATOR):	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPNAME)-$(VSN)

release_src:
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin


# ----------------------------------------------------
# .hrl dependencies
# ----------------------------------------------------
# $(EBIN)/base_syntax.beam:../include/smpp_globals.hrl
