include ../vsn.mk

VSN=$(RRDTOOL_VSN)

include ../../../support/include.mk

all: $(ERL_OBJECTS) ../ebin/rrdtool.app

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/rrd_create.beam: ../include/rrdtool.hrl
$(EBIN_DIR)/rrd_info.beam: ../include/rrdtool.hrl
$(EBIN_DIR)/rrd_xport.beam: ../include/rrdtool.hrl

$(EBIN_DIR)/rrdtool.beam: ../include/rrdtool.hrl