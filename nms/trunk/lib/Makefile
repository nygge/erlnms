LIBS := \
	utils \
	rrdtool \
	pm_basic \
	pm_rrdtool \
	threshold \
	fm_basic \
	fm_clients \
	nls \
	rate \
	nms \
	reltool \
#	pm_rdbms \

all: $(LIBS)

docs:
	@for dir in $(LIBS); do \
	  if [ ! -f $@/SKIP ]; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done

clean:
	@for dir in $(LIBS); do \
	  if [ ! -f $@/SKIP ]; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done

$(LIBS):
	@if [ ! -f $@/SKIP ]; then \
	  (cd $@; $(MAKE) all) \
	fi

# Dependencies between programs.
#errdtool
#pm_basic
#pm_rdbms
#pm_rrdtool: errdtool pm_basic
#threshold
#utils


.PHONY: $(LIBS)

