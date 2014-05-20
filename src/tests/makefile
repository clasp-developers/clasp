include	../../makefile.inc


export TEST_DEST_DIR=$(INSTALLDIR)/tests


gt:
	python gimbalTest.py out.xml

et:
	echo $(PYTHONPATH)
	python eulerTest.py out.xml

gl:
	python ../cando/glView.py out.xml &

subClassTree sct:
	@echo do nothing

sa subAll:
	@echo Install test for now do nothing
#	-rm -rf $(TEST_DEST_DIR)
#	-mkdir -p $(TEST_DEST_DIR)
#	ln -f * $(TEST_DEST_DIR)
#	rm $(TEST_DEST_DIR)/makefile
#	make osSpecific_subAll


subInstall:
	@echo Install test for now do nothing

subParsers:
	@echo No parsers


subClean:
	-rm -f *.pyc


antechamber:
	antechamber -c gas -du n -i trimer.mol2 -fi mol2 -o _trimerAc.mol2 -fo mol2


ifeq ($(CANDO_OS),linux)
osSpecific_subAll:
	cp $(C_meister_OMMON_INSTALLDIR)/* $(TEST_DEST_DIR)


endif

ifeq ($(CANDO_OS),windows)

SRC=$(CANDO_HOME)/targets/windows/

osSpecific_subAll:
	# copy libraries
	(export DEST=$(TEST_DEST_DIR); make copyBoostLibraries )
	(export DEST=$(TEST_DEST_DIR); make copyExpatLibraries )
	(export DEST=$(TEST_DEST_DIR); make copyWindowsLibraries )
#	cp $(C_meister_OMMON_INSTALLDIR)/* $(TEST_DEST_DIR)
	# link source files
	ln -f ../common/*.py $(TEST_DEST_DIR)

endif


TESTS_SOURCE=testPlug.py testAggregate.py testBuilderDatabase.py

TESTS_OUTPUT=$(TESTS_SOURCE:.py=.cmp)
TESTS_DIFF=$(TESTS_SOURCE:.py=.diff)

checkPoint:
	-rm $(TESTS_OUTPUT)
	make $(TESTS_OUTPUT)

test:
	-rm *.diff
	make $(TESTS_DIFF)
	cat *.diff


%.cmp: %.py
	python $< >$@

%.diff: %.py
	echo Results of test: $@ >$@
	python $< | diff - $(<:.py=.cmp) >>$@


