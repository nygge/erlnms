include ../vsn.mk

VSN=$(PM_RRDTOOL_VSN)

include ../../../support/include.mk

ERLC_FLAGS += -I ../../pm_basic/include -I ../../rrdtool/include

#all: $(ERL_OBJECTS) $(ERL_DOCUMENTS) ../ebin/pm_rrdtool.app
all: $(ERL_OBJECTS) ../ebin/pm_rrdtool.app

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/pm_rrd_access.beam: ../include/pm_rrdtool.hrl ../../pm_basic/include/pm_store.hrl ../../rrdtool/include/rrdtool.hrl

$(EBIN_DIR)/pm_rrd_config.beam: ../include/pm_rrdtool.hrl