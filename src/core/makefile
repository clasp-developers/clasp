
include	$(CLASP_HOME)/makefile.inc


subAll sa: $(NEEDS)
	make scrape
	$(BJAM) -j $(COMPILE_PROCESSORS) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg install_opt
	make sb

# clos_accessors.cc
subAllDebug sad: $(NEEDS)
	make scrape
	$(BJAM) -j $(COMPILE_PROCESSORS) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg
	make sb

subBundle sb:
	install -d $(CANDO_APP_LIB_DIR)
	install -c libs/lib* $(CANDO_APP_LIB_DIR)





#
# run bjam and print all the commands it would execute but don't evaluate them
#
san: $(NEEDS)
	$(BJAM) -j $(COMPILE_PROCESSORS) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg install_opt -a -n >_bjam.txt

commit:
	svn commit -m "commit"


subDox:
	make msmarts_Parser.dox


subClean:
	rm -rf bin/*
	rm -f *.tab.*
	rm -f lex.*

export CT=/usr/local/bin/ctags
tags: 	$(SOURCES) $(HEADERS)
	echo $(CT)
	echo *.cc *.cpp *.h
	$(CT) --language-force=c++ *.cc *.cpp *.h


userDocsView: 
	make userDocs
	open -n userDocs.pdf


editDocs.dvi:	scrapedEditDocs.tex editDocs.tex docCommands.tex
	latex editDocs.tex
	latex editDocs.tex
	latex editDocs.tex
	makeindex editDocs
	latex editDocs.tex

editDocs.pdf: editDocs.dvi
	dvipdf editDocs.dvi


userDocs.dvi:	scrapedDocs.tex userDocs.tex docCommands.tex
	latex userDocs.tex
	latex userDocs.tex
	latex userDocs.tex
	makeindex userDocs
	latex userDocs.tex

userDocs.pdf: userDocs.dvi
	dvipdf userDocs.dvi

scrapedDocs.tex: $(SOURCES) msmarts_Parser.yy $(COMMON_DIR)/scrapeDocs.py *.cc
	python $(COMMON_DIR)/scrapeDocs.py scrapedDocs.tex scrapedEditDocs.tex *.cc msmarts_Parser.yy

scrapedEditDocs.tex: $(SOURCES) msmarts_Parser.yy $(COMMON_DIR)/scrapeDocs.py *.cc
	python $(COMMON_DIR)/scrapeDocs.py scrapedDocs.tex scrapedEditDocs.tex *.cc msmarts_Parser.yy


PUMP_OUTPUTS :=  include/generated/applyToActivationFrame.h \
	include/generated/wrappers_functoids.h \
	include/generated/wrappers_methoids.h \
	include/generated/external_wrappers_indirect_methoids.h

#
# When the pump files change - make this target
#
pump: $(PUMP_OUTPUTS)


