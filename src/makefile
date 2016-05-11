

#
# Targets are subAll, subInstall, subClean
#
sa subAll subInstall subClean subClassTree sct subBundle sb subMeta sm sad:

SRC-SUB-DIRS = gctools core cffi clbind asttooling llvmo serveEvent sockets

sub-prebuild:
	make -C gctools/ $@
	make -C core/ $@
	make -C clbind/ $@
	make -C llvmo/ $@
	make -C asttooling/ $@
	make -C serveEvent/ $@
	make -C sockets/ $@




tags:
	$(CTAGS_BIN) -e -R .



allTags at:
	echo no ctags -R


cleanBins:
	-find . -name 'bin' -exec rm -rf {} \;

mps-submodule:
	git submodule add -b dev/2014-08-18/non-incremental https://github.com/Ravenbrook/mps-temporary mps


pump:
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py core/applyToActivationFrame.pmp core/include/generated/applyToActivationFrame.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py core/wrappers_functoids.pmp core/include/generated/wrappers_functoids.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py core/wrappers_methoids.pmp core/include/generated/wrappers_methoids.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py core/external_wrappers_indirect_methoids.pmp core/include/generated/external_wrappers_indirect_methoids.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py clbind/clbind_constructor_functoids.pmp clbind/include/generated/clbind_constructor_functoids.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py clbind/clbind_functoids.pmp clbind/include/generated/clbind_functoids.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py clbind/clbind_methoids.pmp clbind/include/generated/clbind_methoids.h
	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py clbind/clbind_static_members.pmp clbind/include/generated/clbind_static_members.h
#	$(PYTHON2) $(CLASP_HOME)/src/common/pump.py clbind/constructors.pmp clbind/include/generated/constructors.h

