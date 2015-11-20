include	$(CLASP_HOME)/makefile.inc


INC_NEEDS = 
NEEDS = initClasses.inc $(INC_NEEDS)



sad subAllDebug: $(NEEDS)
	make scrape
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single -j $(COMPILE_PROCESSORS) install_dbg
	make subBundle

sa subAll: $(NEEDS)
	make scrape
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single -j $(COMPILE_PROCESSORS) install_dbg install_opt
	make subBundle

san: $(NEEDS)
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single release install -j $(COMPILE_PROCESSORS) -a -n >_bjam.txt

sb subBundle:
	cp libs/[a-z]* $(CANDO_APP_LIB_DIR)
bjd:
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single debug -j $(COMPILE_PROCESSORS)

subMeta sm:
	make mi
	make cscrape

initClasses.inc : *.h
	make mi


relink:
	rm libs/[a-z]*
	make sa

subClean sc:
	rm -rf bin/*

totalClean:
	rm -rf bin install_release



sub-prebuild:
	make scrape-all
