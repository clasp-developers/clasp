
# Do nothing


PUMP = $(CLASP_HOME)/src/common/pump.py


./include/generated/%.h : %.pmp
	$(PYTHON2) $(PUMP) $< $@

