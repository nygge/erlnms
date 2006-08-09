include ../vsn.mk

VSN=$(THRESHOLD_VSN)

include ../../../support/include.mk

ERLC_FLAGS += -I ../../pm_basic/include

all: $(ERL_OBJECTS) ../ebin/threshold.app

clean:
	-rm $(ERL_OBJECTS)

