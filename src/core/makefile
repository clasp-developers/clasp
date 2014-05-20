
include	$(CANDO_HOME)/makefile.inc

M4_NEEDS = af_function_templates.h af_method_templates.h af_external_method_templates.h \
	initClasses.inc predefinedSymbols.inc \
	symbols_scraped.inc symbol_apply.inc

#	 class_method_templates.h external_class_method_templates.h


NEEDS = $(M4_NEEDS)

ECL = ecl


subParsers: $(PARSER_NEEDS)
	echo Nothing




FLEX=flex

sal:
	(cd ..; make sal)

#clos_accessors.cc

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

initClasses.inc: *.h
	make mi

m4: $(M4_NEEDS)
	echo Made $(M4_NEEDS)

af_function_templates.h aft: generate_af_function_templates.lisp prepare_af_method_templates.lisp
	$(ECL) -shell generate_af_function_templates.lisp

af_method_templates.h amt: generate_af_method_templates.lisp prepare_af_method_templates.lisp
	$(ECL) -shell generate_af_method_templates.lisp

af_external_method_templates.h aemt: generate_af_external_method_templates.lisp prepare_af_method_templates.lisp
	$(ECL) -shell generate_af_external_method_templates.lisp

#class_method_templates.h cmt: generate_class_method_templates.lisp
#	$(ECL) -shell generate_class_method_templates.lisp

#external_class_method_templates.h ecmt: generate_external_class_method_templates.lisp
#	$(ECL) -shell generate_external_class_method_templates.lisp

symbol_apply.inc: generate_symbol_apply.lisp
	$(ECL) -shell generate_symbol_apply.lisp



# Build all the meta-programming stuff
subMeta sm:
	make cscrape
	make mi
	make af_function_templates.h
	make af_method_templates.h
	make af_external_method_templates.h

	make symbol_apply.inc
	make clos
	make m4

#
# Build clos accessors
#

#clos_accessors.cc clos: clos_accessors.lisp
#	$(ECL) -shell clos_accessors.lisp


userDocs: userDocs.pdf
	install -c userDocs.pdf $(CANDO_HOME)/docs

editDocs: editDocs.pdf

docs:	userDocs.pdf editDocs.pdf

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

lex:
	make lex.lisp.cc

msmarts_Parser.dox:

bisonHelp:
	"$(BISON)" -h

winParsers:
	(cmd.exe /C bison -d -v -p lisp lispParse.yy)


moeForceField.tab.cc: moeForceField.yy
	$(BISON) -v `cygpath -a moeForceField.yy`

moeForceField.tab.hh: moeForceField.tab.cc



#
# Common parser generators
#
#


_lisp_PureLexer.cc : lisp_PureLexer.ll
	$(FLEX) -Plisp --nounistd --header-file=_lisp_PureLexer.h  -o_lisp_PureLexer.cc lisp_PureLexer.ll    

_lisp_Parser.cc : lisp_Parser.yy
	$(BISON) -p lisp -v --defines=_lisp_Parser.h -d -o _lisp_Parser.cc lisp_Parser.yy

_msmarts_Parser.cc : msmarts_Parser.yy
	echo MAKE SURE BISON IS UP TO DATE VERSION MUST BE higher than 2.5
	$(BISON) --version
	$(BISON) -p msmarts -v -d -o _msmarts_Parser.cc msmarts_Parser.yy

_gaff_Parser.cc : gaff_Parser.yy
	$(BISON) -p gaff -v -t -d -o _gaff_Parser.cc gaff_Parser.yy



