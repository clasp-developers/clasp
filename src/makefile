

#
# Targets are subAll, subInstall, subClean
#
sa subAll subInstall subClean subClassTree sct subBundle sb subMeta sm sad:

SRC-SUB-DIRS = gctools core cffi clbind asttooling llvmo serveEvent sockets

sub-prebuild:
	make -C gctools/ $@
	make -C core/ $@
	make -C clbind/ $@
	make -C asttooling/ $@
	make -C llvmo/ $@
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


