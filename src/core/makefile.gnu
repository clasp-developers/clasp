#
#
#
#
#  Important targets:
#
#
#
#	make initPython - searches through all source files and identifies class initialization functions
#
#

bjam:
	bjam


include	$(CANDO_HOME)/makefile.inc

printenv:
	echo BISON=$(BISON)
	echo CANDO_RESOURCES_BIN_DIR=$(CANDO_RESOURCES_BIN_DIR)
	echo CANDO_RESOURCES_LIB_DIR=$(CANDO_RESOURCES_LIB_DIR)
	@echo LIB is "$(LIB)"


export LIBRARY_NEEDS = 



include	makefile.files

OBJ_DIR = $(shell pwd)/obj

#
# mbbTargets is defined in makefile.inc for each system
# 
subAll sa:
	make -j $(COMPILE_PROCESSORS) -f makefile.gnu subTargets
	make subBundle


objs:
	make -j 2 $(OBJECTS)


subInstall:
	@echo Install mbbCore, for now do nothing

subClassTree sct:
	make classTree

subParsers: lispParse.tab.cc gaffParse.tab.cc
	make lispParse.tab.cc
	make lex
	make gaffParse.tab.cc 
	make msmartsParse.tab.cc

subDox:
	make msmartsParse.dox


subClean:
	rm -f *.o *.d.* *.d *.p *.obj *.exe
	rm -f *.tab.*
	rm -f lex.*

tags: 	$(SOURCES) $(HEADERS)
	echo $(CTAGS)
	$(CTAGS) --language-force=c++ $(SOURCES) $(HEADERS)

m4Templates.h: m4Templates.m4
	m4 m4Templates.m4 >m4Templates.h

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

scrapedDocs.tex: $(SOURCES) main.cc candoMpi.cc msmartsParse.yy $(COMMON_DIR)/scrapeDocs.py
	python $(COMMON_DIR)/scrapeDocs.py scrapedDocs.tex scrapedEditDocs.tex *.cc msmartsParse.yy

scrapedEditDocs.tex: $(SOURCES) main.cc candoMpi.cc msmartsParse.yy $(COMMON_DIR)/scrapeDocs.py
	python $(COMMON_DIR)/scrapeDocs.py scrapedDocs.tex scrapedEditDocs.tex *.cc msmartsParse.yy

lex:
	make lex.lisp.cc

msmartsParse.dox:

bisonHelp:
	"$(BISON)" -h
	
winParsers:
	(cmd.exe /C bison -d -v -p lisp lispParse.yy)


moeForceField.tab.cc: moeForceField.yy
	$(BISON) -v `cygpath -a moeForceField.yy`

moeForceField.tab.hh: moeForceField.tab.cc

lex.lisp.cc : lispLex.ll
	flex --nounistd -P lisp -o lex.lisp.cc lispLex.ll

lispParse.tab.cc : lispParse.yy
	$(BISON) -d -v -p lisp lispParse.yy
	cp lispParse.tab.hh lispParse_obj.hpp

msmartsParse.tab.cc : msmartsParse.yy
	$(BISON) -v -p smarts msmartsParse.yy

gaffParse.tab.cc : gaffParse.yy
	$(BISON) -v -t -p gaff gaffParse.yy


include	makefile.files
OBJECTS = $(SOURCES:.cc=.o)
MPI_ENABLED_OBJECTS = $(MPI_ENABLED_SOURCES:.cc=.o)
MPI_ENABLED_DEPENDS = $(MPI_ENABLED_SOURCES:.cc=.d)
MPI_DISABLED_OBJECTS = $(MPI_DISABLED_SOURCES:.cc=.o)
MPI_DISABLED_DEPENDS = $(MPI_DISABLED_SOURCES:.cc=.d)
MWX_OBJECTS=$(MWX_SOURCES:.cpp=.o)
#DEPENDENTS = $(SOURCES:.cc=.d) main.d

LIB_OBJECTS=$(OBJECTS) $(MPI_DISABLED_OBJECTS)
CANDO_OBJECTS=$(OBJECTS) $(MPI_DISABLED_OBJECTS)
CANDO_MPI_OBJECTS=$(OBJECTS) $(MPI_ENABLED_OBJECTS)
CANDO_VIEW_OBJECTS=$(MWX_OBJECTS) $(OBJECTS) $(MPI_DISABLED_OBJECTS)


mainMpi.o: main.cc

candoMpiEnabled.o: candoMpi.cc

candoMpiDisabled.o: candoMpi.cc

######################################################
######################################################
######################################################
######################################################
#
#
# Describe how to make object files based on the compiler
#
#



######################################################
#
# GNU GNU GNU GNU GNU GNU GNU GNU GNU GNU GNU GNU
#
ifeq ($(CANDO_COMPILER),gnu)


export CandCXXFLAGS=default
ifeq ($(COMPILE_TYPE),optasm)
    export CandCXXFLAGS=-O3 -S -pipe -fPIC $(DEFINES)
endif
ifeq ($(COMPILE_TYPE),optimize)
    export CandCXXFLAGS=-O3 -pipe -fPIC $(DEFINES)
endif
ifeq ($(COMPILE_TYPE),optimize_debug)
    export CandCXXFLAGS=-g -ggdb -O3 -pipe -fPIC -DSOURCE_DEBUG $(DEFINES)
endif
ifeq ($(COMPILE_TYPE),optimize_debug_warn)
    export CandCXXFLAGS=-Wall -Wno-unused-variable -g -ggdb -O3 -pipe -fPIC -DSOURCE_DEBUG $(DEFINES)
endif
ifeq ($(COMPILE_TYPE),preprocessOnly)
    export CandCXXFLAGS=-E -pipe -fPIC $(DEFINES)
endif
ifeq ($(COMPILE_TYPE),debug)
    export CandCXXFLAGS=-Wall -Wno-unused-variable -g -ggdb -pipe -fPIC -DSOURCE_DEBUG $(DEFINES)
endif
export CPPFLAGS=$(CandCXXFLAGS)
export CXXFLAGS=$(CandCXXFLAGS) $(ARCH)
export CFLAGS=$(CandCXXFLAGS)

CFLAGS	=	-Wextra -pipe -g $(DEFINES)
INCPATH	=	$(EXTERNAL_INCLUDES) -I$(SOURCE_DIR)
LINK	=	g++
LFLAGS	+=	$(LINKING_FLAGS)
LIBS	=	$(EXTERNAL_LIBS)
AR	=	ar cqs
RANLIB	=	

TAR	=	tar -cf
GZIP	=	gzip -9f



TARGETSO=	$(TARGET).so
TARGETDYLIB=	$(TARGET).dylib
TARGETDLL=	$(TARGET).dll


TARGETD	=	$(TARGET).1.0.0
TARGET0	=	$(TARGET).1.0.0
TARGET1	=	$(TARGET).1
TARGET2	=	$(TARGET).1.0

####### Implicit rules

.SUFFIXES: .cc .c

.cc.o:
	$(CC) -c $(CXXFLAGS) $(INCPATH) -o $@ $<

.cc.p:
	$(CC) -E -c $(CXXFLAGS) $(INCPATH) -o $@ $<

.cpp.o : 
	$(CC) -D__WXMAC__ -DSOURCE_DEBUG -DEXPAT -Ddarwin -g -ggdb \
		$(EXTERNAL_INCLUDES)\
		$(ARCH) \
		-I/usr/local/lib/wx/include/mac-ansi-release-static-2.8 \
		-I/usr/local/include/wx-2.8 \
		-I/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers/ \
		-c -o $@ $<
%.d:	%.cpp
	set -e; rm -f $@; \
	$(CC) -MM -D__WXMAC__ -DSOURCE_DEBUG -DEXPAT -Ddarwin -g -ggdb \
		$(EXTERNAL_INCLUDES)\
		-I/usr/local/lib/wx/include/mac-ansi-release-static-2.8 \
		-I/usr/local/include/wx-2.8 \
		-I/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers/ \
		-c -o $@ $<
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.cpp.p:
	$(CC) -E -D__WXMAC__ -DSOURCE_DEBUG -DEXPAT -Ddarwin -g -ggdb \
		$(EXTERNAL_INCLUDES)\
		-I/usr/local/lib/wx/include/mac-ansi-release-static-2.8 \
		-I/usr/local/include/wx-2.8 \
		-I/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers/ \
		-c -o $@ $<

.c.o:
	$(CC) -c $(CFLAGS) $(INCPATH) -o $@ $<



%.d:	%.cc
	set -e; rm -f $@; \
	$(CC) -MM $(CPPFLAGS) $(INCPATH) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$




-include	$(SOURCES:.cc=.d)
-include	$(MWX_SOURCES:.cpp=.d)
-include	$(MPI_ENABLED_SOURCES:.cc=.d)
-include	$(MPI_DISABLED_SOURCES:.cc=.d)

endif



######################################################
#
# WINDOWS VisualC
#
ifeq ($(CANDO_COMPILER),visualc)

#export DEFINES=-DUSEBOOSTPYTHON -DEXPAT -D$(CANDO_OS) -DSOURCE_DEBUG
export DEFINES=-DEXPAT -DXML_STATIC -D$(CANDO_OS) -DSOURCE_DEBUG

    ifeq ($(COMPILE_TYPE),optimize)
        export CandCXXFLAGS=/EHsc /WL /GX /GR /O2 /G7 $(DEFINES) -DWIN32
    endif
    ifeq ($(COMPILE_TYPE),optimize_debug)
        export CandCXXFLAGS=/EHsc /Zi /DEBUG /nologo /WL /GX /GR /O2 /G7 $(DEFINES) -DWIN32
    endif
    ifeq ($(COMPILE_TYPE),optimize_debug_warn)
        export CandCXXFLAGS=/EHsc /Zi /DEBUG /nologo /WL /GX /GR /O2 /G7 $(DEFINES) -DWIN32
    endif
    ifeq ($(COMPILE_TYPE),preprocessOnly)
        export CandCXXFLAGS=/EHsc /P /nologo /WL /GX /GR /O2 /G7 $(DEFINES) -DWIN32
    endif
    ifeq ($(COMPILE_TYPE),debug)
#        export CandCXXFLAGS=/nologo /WL /GX /GR /Zi /DEBUG $(DEFINES) -DWIN32
        export CandCXXFLAGS=/EHsc /GR /Zi /DEBUG $(DEFINES) -DWIN32
    endif

export CXXFLAGS=$(CandCXXFLAGS)
export CFLAGS=$(CandCXXFLAGS)


CC	=	cl
CXX	=	cl
LINK	=	link.exe
GNUCXX	=	g++


.cc.o:
	$(CXX) /c /nologo /TP \
		/MD \
		$(CXXFLAGS)\
		"/I$(WIN_BOOST_INCLUDES)" \
		"/I$(WIN_EXPAT_INCLUDES)" \
		"/I$(WIN_PYTHON_INCLUDES)" \
		-Fo$@ $< 


.cpp.o:
	$(CXX) /c /nologo /TP \
		/MD \
		$(CXXFLAGS) \
		/D__WXMSW__ \
		"/I$(WIN_BOOST_INCLUDES)"\
		"/Ic:\wxWidgets-2.8.10\include" \
		"/Ic:\wxWidgets-2.8.10\lib\vc_lib\msw" \
		/Fo$@ \
		$< 

%.d:	%.cpp
	set -e; rm -f $@; \
	$(GNUCXX) -MM \
		$(CXXFLAGS) \
		$(INCPATH) \
		"-I$(shell cygpath -u $(WIN_BOOST_INCLUDES))" \
		"-I$(shell cygpath -u 'c:\wxWidgets-2.8.10\include')" \
		"-I$(shell cygpath -u 'c:\wxWidgets-2.8.10\lib\vc_lib\msw')" \
	$< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$



subTargets: cando candoView 
#subTargets: cando candoView  candoMpi


libmbbCxx.dll: $(OBJECTS)
	$(LINK) /DLL $(OBJECTS) /Out:libmbbCxx.dll \
		"/LIBPATH:$(WIN_LIB_DIR)"\
		libexpat.lib

#libmbbCxx.lib: $(OBJECTS)
#	lib /def:libmbbCxx.lib /verbose $(OBJECTS)

cando: $(OBJECTS) main.o
	$(LINK) $(OBJECTS) main.o \
		/verbose:lib \
		/SUBSYSTEM:console\
		"/LIBPATH:$(WIN_LIB_DIR)"\
		/NODEFAULTLIB:libcmt.lib \
		libexpatMT.lib\
		/OUT:cando.exe

#candoMpi: $(OBJECTS) mainMpi.o
#	$(LINK) $(OBJECTS) mainMpi.o \
#		/verbose:lib \
#		/SUBSYSTEM:console\
#		"/LIBPATH:$(WIN_LIB_DIR)"\
#		/NODEFAULTLIB:libcmt.lib \
#		libexpatMT.lib\
#		/OUT:cando.exe

candoView: $(CANDO_VIEW_OBJECTS)
	link $(CANDO_VIEW_OBJECTS) \
		/SUBSYSTEM:windows \
		"/LIBPATH:$(WIN_LIB_DIR)"\
		/NODEFAULTLIB:libcmt.lib \
		/verbose:lib \
		libexpatMT.lib \
		wxmsw28_gl.lib\
		opengl32.lib\
		glu32.lib\
		wxbase28.lib\
		wxmsw28_core.lib\
		wxtiff.lib\
		wxjpeg.lib\
		wxpng.lib\
		wxzlib.lib\
		wxregex.lib\
		wxexpat.lib\
		kernel32.lib\
		user32.lib\
		gdi32.lib\
		comdlg32.lib\
		winspool.lib\
		winmm.lib\
		shell32.lib\
		comctl32.lib\
		ole32.lib\
		oleaut32.lib\
		uuid.lib\
		rpcrt4.lib\
		advapi32.lib\
		wsock32.lib\
		odbc32.lib\
		/OUT:candoView.exe
	



#
#		/LIBPATH:"c:\Python26\libs" \
#		/LIBPATH:"c:\Expat201\Bin" \
#		libexpat.lib


subBundle sb:
	install -d $(CANDO_APP_BIN_DIR)
	install -d $(CANDO_APP_LIB_DIR)
	install	-c cando.exe $(CANDO_APP_BIN_DIR)
	install	-c candoView.exe $(CANDO_APP_BIN_DIR)
	install	-c libmbbCxx.dll $(CANDO_APP_BIN_DIR)









clhelp:
	cl -help

linkhelp:
	link /link

endif






######################################################
######################################################
######################################################
######################################################
#
#
# Describe how to make targets based on the target system
#
#

######################################################
#
# Linux
#
# Link only cando and candoMpi
# 
# Doesn't use boost::python
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),linux)

export DEFINES= -DEXPAT -D$(CANDO_OS)

#subTargets: libmbbCxx.dylib libmbbCxx.so cando candoView candoMpi
subTargets: cando candoView candoMpi

subBundle sb: $(MBB_TARGETS)
	install -c cando $(CANDO_APP_BIN_DIR)
	install -c candoMpi $(CANDO_APP_BIN_DIR)
	
cando: main.o main.d $(CANDO_OBJECTS) $(MPI_DISABLED_DEPENDS)
	$(LINK) main.o $(CANDO_OBJECTS) \
		-L$(CANDO_APP_LIB_DIR) \
		-lboost_filesystem\
		-lboost_program_options\
		-lexpat \
		-Wl,-rpath,@executable_path/../Resources/lib\
		-o $@

candoMpi: mainMpi.o mainMpi.d main.d $(CANDO_MPI_OBJECTS) $(MPI_ENABLED_DEPENDS)
	$(LINK) mainMpi.o $(CANDO_MPI_OBJECTS) \
		`mpicxx -showme:link` \
		-L$(CANDO_APP_LIB_DIR) \
		-lboost_filesystem-$(BOOST_VERSION) \
		-lboost_program_options-$(BOOST_VERSION) \
		-lexpat \
		-Wl,-rpath,@executable_path/../Resources/lib\
		-o $@
endif





######################################################
#
# OS X /darwin
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),darwin)
# ---------------  Above inserted from makefile.gnu --------------------
# ---------------  Below from makefile.inc --------------------
#
# Compile with multiple architectures
# 
#
#export ARCH=-arch i386
export ARCH=
export DEFINES=-DUSEBOOSTPYTHON -DEXPAT -D$(CANDO_OS)

oldWay_library: $(LIBRARY_DIR) $(OBJECTS)
	install -d obj
	make --warn-undefined-variables -j $(COMPILE_PROCESSORS) -C $(OBJ_DIR) -f $(COMMON_DIR)/makefile.gnu libraries
	install -d $(LIBRARY_DIR)
	install -c $(OBJ_DIR)/lib* $(LIBRARY_DIR)

libmbbCxx.dylib: $(LIB_OBJECTS) $(MPI_DISABLED_DEPENDS)
	@echo DEVELOPMENT_ENVIRONMENT = $(DEVELOPMENT_ENVIRONMENT)
	@echo EXTERNAL_LIBS = $(EXTERNAL_LIBS)
	@echo MPI_DISABLED_OBJECTS = $(MPI_DISABLED_OBJECTS)
	$(LINK) -dynamiclib -current_version 0.0.01 \
		-install_name ./$@ \
                $(LIB_OBJECTS) \
		-L$(CANDO_APP_LIB_DIR) \
		-undefined dynamic_lookup \
		-lboost_filesystem \
		-lboost_program_options \
		-lboost_python \
		-lexpat \
    		-L/System/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/config -lpython2.5\
		-Wl,-rpath,@executable_path/../Resources/lib\
		-install_name @rpath/libmbbCxx.dylib \
		-o $@

#		$(ARCH) \

libmbbCxx.so: mainBoostPython.o libmbbCxx.dylib 
	$(LINK) -bundle mainBoostPython.o -L$(LIBRARY_DIR) \
		-L./ -lmbbCxx $(EXTERNAL_LIBS) \
		-undefined dynamic_lookup \
		-Wl,-rpath,@executable_path/../lib\
		-o $@

#		$(ARCH) \
#		-undefined dynamic_lookup

cando: main.o main.d $(CANDO_OBJECTS) $(MPI_DISABLED_DEPENDS)
	$(LINK) main.o $(CANDO_OBJECTS) \
		$(ARCH) \
		-undefined dynamic_lookup \
		-L$(CANDO_APP_LIB_DIR) \
		-lboost_filesystem \
		-lboost_program_options \
		-lboost_python \
		-lexpat \
    		-L/System/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/config -lpython2.5\
		-Wl,-rpath,@executable_path/../Resources/lib\
		-o $@

#		$(ARCH) \


candoMpi: mainMpi.o mainMpi.d main.d $(CANDO_MPI_OBJECTS) $(MPI_ENABLED_DEPENDS)
	$(LINK) mainMpi.o $(CANDO_MPI_OBJECTS) \
		$(ARCH) \
		`mpicxx -showme:link` \
		-L$(CANDO_APP_LIB_DIR) \
		-undefined dynamic_lookup \
		-lboost_filesystem \
		-lboost_program_options \
		-lboost_python \
		-lboost_serialization \
		-lboost_mpi \
		-lexpat \
    		-L/System/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/config -lpython2.5\
		-Wl,-rpath,@executable_path/../Resources/lib\
		-o $@

#		$(ARCH) \


export WXLIBS = -lwx_base_carbon-2.8
export WXLIBS += -lwx_base_carbon_net-2.8
export WXLIBS += -lwx_base_carbon_xml-2.8
export WXLIBS += -lwx_mac_adv-2.8
export WXLIBS += -lwx_mac_aui-2.8
export WXLIBS += -lwx_mac_core-2.8
export WXLIBS += -lwx_mac_gl-2.8
export WXLIBS += -lwx_mac_html-2.8
export WXLIBS += -lwx_mac_qa-2.8
export WXLIBS += -lwx_mac_richtext-2.8
export WXLIBS += -lwx_mac_xrc-2.8
export WXLIBS += -lwxjpeg-2.8
export WXLIBS += -lwxpng-2.8
export WXLIBS += -lwxtiff-2.8
export WXLIBS += -liconv

#
# Compile with iarch i386 only because wxWidgets doesnt support 64 bit
#
candoView: $(CANDO_VIEW_OBJECTS)
	g++ -L$(CANDO_APP_BIN_DIR) -arch i386\
		$(CANDO_VIEW_OBJECTS)\
		-undefined dynamic_lookup \
		-L/usr/local/lib $(WXLIBS) \
		-L$(CANDO_APP_LIB_DIR) \
		-lboost_filesystem \
		-lboost_program_options \
		-lboost_python \
		-lexpat \
    		-L/System/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/config -lpython2.5\
		-framework IOKit \
		-framework Carbon  \
		-framework Cocoa \
		-framework System \
		-framework QuickTime \
		-framework OpenGL \
		-framework Glut \
		-framework AGL \
		-Wl,-rpath,@executable_path/../Resources/lib\
		-o $@



#subTargets: libmbbCxx.dylib libmbbCxx.so cando candoView candoMpi
subTargets: cando candoView candoMpi

subBundle sb: $(MBB_TARGETS)
	install -c libmbbCxx.dylib $(CANDO_APP_LIB_DIR)
	install -c libmbbCxx.so $(CANDO_APP_LIB_DIR)
	install -c cando $(CANDO_APP_BIN_DIR)
	install -c candoView $(CANDO_APP_BIN_DIR)
	install -c candoMpi $(CANDO_APP_BIN_DIR)
	

endif


