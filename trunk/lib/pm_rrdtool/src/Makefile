include ../vsn.mk

VSN=$(PM_RRDTOOL_VSN)

include ../../../support/include.mk

ERLC_FLAGS += -I ../../pm_basic/include

all: $(ERL_OBJECTS) $(ERL_DOCUMENTS) ../ebin/pm_rrdtool.app

clean:
	-rm $(ERL_OBJECTS)

