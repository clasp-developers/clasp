

include	$(CLASP_HOME)/makefile.inc

#
# Targets are subAll, subInstall, subClean
#
sa subAll subInstall subClean subClassTree sct subBundle sb subMeta sm sad:
	make -C core/ $@
	make -C cffi/ $@
	make -C llvmo/ $@
	make -C lisp/ $@
	make -C candoLisp/ $@
#	make -C sys/ $@
	make -C addon/ $@
	make -C units/ $@
	make -C chem/ $@
#	make -C numerics/ $@
	make -C kinematics/ $@
#	make -C openmmPackage $@
	make -C packageManager $@
	make -C cando/ $@
	make -C tools/ $@
	make -C python/ $@
#	make -C databases/ $@
#	make -C mwxCando/ $@



sal:
	make -C core/ sa
	make -C candoLisp/ sa

tags:
	$(CTAGS_BIN) -e -R .



allTags at:
	echo no ctags -R
