

#
# Targets are subAll, subInstall, subClean
#
sa subAll subInstall subClean subClassTree sct subBundle sb subMeta sm sad:

SRC-SUB-DIRS = gctools core cffi clbind asttooling llvmo serveEvent sockets

sub-prebuild:
	$(foreach var,$(SRC-SUB-DIRS), make -C $(var)/ $@; )




tags:
	$(CTAGS_BIN) -e -R .



allTags at:
	echo no ctags -R


cleanBins:
	-find . -name 'bin' -exec rm -rf {} \;

mps-submodule:
	git submodule add -b dev/2014-08-18/non-incremental https://github.com/Ravenbrook/mps-temporary mps


