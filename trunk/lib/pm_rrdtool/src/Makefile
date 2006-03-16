include ../vsn.mk

VSN=$(PM_RRDTOOL_VSN)

include ../../../support/include.mk

ERLC_FLAGS += -I ../../pm_basic/include

all: $(ERL_OBJECTS) $(ERL_DOCUMENTS) ../ebin/pm_rrdtool.app

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/pm_rrd_access.beam: ../../pm_basic/include/pm_store.hrl

$(EBIN_DIR)/pm_rrd_config.beam: ../include/pm_rrdtool.hrl