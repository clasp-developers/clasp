


include	$(CANDO_HOME)/makefile.inc


sad subAllDebug:
	rm -f apps/[a-z]*
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg -j $(COMPILE_PROCESSORS)
	make subBundle

sa subAll:
	rm -f apps/[a-z]*
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg install_opt -j $(COMPILE_PROCESSORS)
	make subBundle

sm subMeta:
	echo Nothing

san:
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg install_opt -j $(COMPILE_PROCESSORS) -a -n >_bjam.txt

sb subBundle:
	install -d $(CANDO_APP_BIN_DIR)
	install -c apps/[a-z]* $(CANDO_APP_BIN_DIR)
bjd:
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single debug -j $(COMPILE_PROCESSORS)

relink:
	rm apps/[a-z]*
	make sa

subClean sc:
	rm -rf bin/*

totalClean:
	rm -rf bin install_release
