LIBS := \
	ast_man \
	fast_agi \
	pm_basic \
	pm_rrdtool \
	reltool \
	rrdtool \
	threshold \
	utils
#	pm_rdbms \

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

