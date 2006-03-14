include ../../../../support/include.mk

ERLC_FLAGS += -I ../../include

all: $(ERL_OBJECTS)

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/os_poll.beam: ../../include/rrdtool.hrl
$(EBIN_DIR)/rrd_lib_test.beam: ../../include/rrdtool.hrl

