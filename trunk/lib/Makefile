LIBS := \
	errdtool \
#	pm_basic \
#	pm_rdbms \
#	pm_rrdtool \
#	threshold \
#	utils

all: $(LIBS)

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

