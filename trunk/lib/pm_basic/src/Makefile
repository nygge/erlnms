include ../vsn.mk

VSN=$(PM_BASIC_VSN)

include ../../../support/include.mk

all: $(ERL_OBJECTS) $(ERL_DOCUMENTS) ../ebin/pm_basic.app

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/pm_config.beam: ../include/pm_store.hrl
$(EBIN_DIR)/pm_store.beam: ../include/pm_store.hrl