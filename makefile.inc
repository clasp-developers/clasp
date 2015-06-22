#############################################
#
#
# This depends on the following enviroment variables
# being defined in your shell login script
# DEVELOPMENT_ENVIRONMENT = (darwin|)
# CANDO_LOG = _dbg.dbg
# CANDO_HOME = ___root_of_cando_tree___
# LD_LIBRARY_PATH
# -OSX- DYLD_LIBRARY_PATH
# PYTHONPATH to lib and modules
# PATH to include cando python
#
# NOTE NOTE NOTE NOTE NOTE
# External libraries are defined in makefileExternals.inc
# this file is included below

PUMP = $(CLASP_HOME)/src/common/pump.py

default:
	make sa



# cleanup first
cscrape:
	-rm symbols_scraped.inc
	touch symbols_scraped.inc
	make scrape

prebuild:
	make -C $(CLASP_HOME) sub-prebuild




######################################################################
###
###  Scraping
###
scrape-symbols:
	echo Scraping symbols in `pwd`
	(export PYTHONPATH=$(CLASP_HOME)/src/common:$(PYTHONPATH); python $(CLASP_HOME)/src/common/symbolScraper.py include/generated/symbols_scraped_inc.h include/generated/symbols_scraped_inc.h configure_symbol_scrape.py include/*.h *.cc)

scrape-classes:
	echo Scraping classes in `pwd`
	install -d ./include/generated
	(export PYTHONPATH=`pwd`:$(PYTHONPATH); python $(CLASP_HOME)/src/common/registerClasses.py include/generated/initClasses_inc.h include/*Package.h include/*.h *.h *.cc 2>registerClasses.log)


######################################################################
###
###  Scraping in subdirectories
###


sub-scrape-symbols:
	-make scrape-symbols

sub-scrape-classes:
	make scrape-classes


./include/generated/%.h : %.pmp
	$(PYTHON2) $(PUMP) $< $@




#
# I want everything about the build defined here
# 

#
# BOOST_VERSION is defined in the startup script development.xxxx
#
#
export BOOST_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/boost_$(BOOST_VERSION)


export BISON=bison

#
# Variables defining CANDO modules
#
export mbb_ROOT = mbb
export mbbCxx_ROOT = $(mbb_ROOT)Cxx
export mbb_MODULE_DIR = $(MODULE_DIR)/$(mbb_ROOT)
export mbb_SOURCE_DIR = $(SOURCE_DIR)/$(mbb_ROOT)

export chemInfo_ROOT = chemInfo
export chemInfo_MODULE_DIR = $(MODULE_DIR)/$(mbb_ROOT)/$(chemInfo_ROOT)


#####################
#
# Common directory names
#
#
#

#
# COMPILE_TYPE can be one of preprocessOnly|debug|optimize|optimize_debug|optasm   (in future add "profile")
#export COMPILE_TYPE=preprocessOnly
#export COMPILE_TYPE=debug
#export COMPILE_TYPE=optimize_debug
export COMPILE_TYPE=optimize_debug_warn
#export COMPILE_TYPE=optimize
#export COMPILE_TYPE=optasm	# dump assembly code

export	CDEBUG_FULL=1

#export USE_BOINC=1



###################################################
#
# Environment variables that are common to all DEVELOPMENT_ENVIRONMENTS
#
#
#export CANDO_INSTALLDIR=$(INSTALLDIR)/candoEdit
# namespace info
export mbbCxx_INCLUDE=$(CANDO_HOME)/src/mbb/cxx

#export BUILDTOP = $(CANDO_HOME)/build
export SOURCE_DIR = $(CANDO_HOME)/src
export EXTERNALS_DIR = $(CANDO_HOME)/externals
export COMMON_DIR = $(SOURCE_DIR)/common
#export OS_BUILDTOP = $(BUILDTOP)/$(CANDO_OS)
#export MODULE_DIR = $(OS_BUILDTOP)/modules
export LIBRARY_DIR = $(OS_BUILDTOP)/lib
#export BIN_DIR = $(OS_BUILDTOP)/bin


export EXTERNALS_SOURCE_DIR = $(EXTERNALS_DIR)/src
export BJAM_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/boost-jam-$(BJAM_VERSION)/
export PYTHON_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/Python-2.6.1
export EXPAT_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/expat-2.0.1
export READLINE_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/readline-6.2
export ZLIB_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/zlib-1.2.6
export EZ_SETUP_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/ez_setup_py
export PYOPENGL_SOURCE_DIR = $(EXTERNALS_SOURCE_DIR)/PyOpenGL-3.0.0b1


export SHARED_FLAGS=-shared
#include $(EXTERNALS_DIR)/makefileExternals.inc


#
# Put libraries in LIBRARY_DIR and point PYTHONPATH there
#


export buildDatabase_SOURCE_DIR = $(SOURCE_DIR)/buildDatabase
export buildDatabase_MODULE_DIR = $(MODULE_DIR)/buildDatabase


export DATABASE_DIR = $(CANDO_HOME)/data
export core_DATABASE_DIR = $(DATABASE_DIR)/core



export AWK=gawk
export BISON=bison
###########################################################
#
# Unix on kraken supercomputer
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),linux)
    export CANDO_OS = linux
    export CANDO_COMPILER=gcc
    export AWK = awk
    export CTAGS = /usr/local/bin/ctags


bundleDirectories:
	install -d $(CANDO_APP_BIN_DIR)
	install -d $(CANDO_APP_LIB_DIR)
	install	-d $(CANDO_APP_RESOURCES_DIR)
	install	-d $(CANDO_APP_RESOURCES_MODULE_DIR)
	install	-d $(CANDO_APP_RESOURCES_BIN_DIR)
	install	-d $(CANDO_APP_RESOURCES_CSC_DIR)
	install	-d $(CANDO_APP_RESOURCES_DATABASE_DIR)
#	install	-d $(CANDO_APP_EXAMPLES_DIR)

    #    export BOOST_TOOLSET=pathscale
    #    export BOOST_TOOLSET=gcc
    #    export BOOST_TOOLSET=pgi
    export BOOST_TOOLSET=gcc
    export BOOST_INCLUDE_DIR=$(EXTERNALS_DIR)/include/boost-$(BOOST_VERSION)/
    export PYTHON_INCLUDE_DIR=/usr/include/python2.6
    export EXTERNAL_INCLUDES = -I/usr/include
    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include
    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include/boost-$(BOOST_VERSION)
    export EXTERNAL_INCLUDES += -I/System/Library/Frameworks/Python.framework/Versions/2.6/include/python2.6

    EXTERNAL_LIBS      = -L$(CANDO_APP_BIN_DIR)
    EXTERNAL_LIBS	+= -lboost_python
    EXTERNAL_LIBS	+= -lboost_filesystem
    EXTERNAL_LIBS	+= -lboost_program_options
    EXTERNAL_LIBS	+= -lexpat
    EXTERNAL_LIBS	+= -L/System/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/config -lpython2.6
    export SHARED_FLAGS = -bundle -flat_namespace -undefined suppress
    export PYTHON_VERSION=python26
    export PYTHON_EMBED_TOP=$(HOME)/Development/mbb_Python-2.6.1
    export mbbCxx_TARGET += staticLib
    export smarts_TARGET += staticLib
    export forceField_TARGETS += staticLib
    export AWK = awk
    export BJAM_EXE_LOCAL_DIR = bin.macosxx86_64
    export BJAM_EXE_DIR = $(BJAM_SOURCE_DIR)/$(BJAM_EXE_LOCAL_DIR)
endif


###########################################################
#
# Unix on kraken supercomputer
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),kraken)
    export CANDO_OS = kraken
    export CANDO_COMPILER=gcc
    export AWK = awk
    export CTAGS = /usr/local/bin/ctags


bundleDirectories:
	install -d $(CANDO_APP_BIN_DIR)
	install -d $(CANDO_APP_LIB_DIR)
	install	-d $(CANDO_RESOURCES_DIR)
	install	-d $(CANDO_RESOURCES_MODULE_DIR)
	install	-d $(CANDO_RESOURCES_BIN_DIR)
	install	-d $(CANDO_RESOURCES_CSC_DIR)
	install	-d $(CANDO_RESOURCES_DATABASE_DIR)
	install	-d $(CANDO_APP_EXAMPLES_DIR)

    #    export BOOST_TOOLSET=pathscale
    #    export BOOST_TOOLSET=gcc
    #    export BOOST_TOOLSET=pgi
    export BOOST_TOOLSET=gcc
    export BOOST_INCLUDE_DIR=$(EXTERNALS_DIR)/include/boost-$(BOOST_VERSION)/
    export PYTHON_INCLUDE_DIR=/usr/include/python2.6
    export EXTERNAL_INCLUDES = -I/usr/include
    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include
    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include/boost-$(BOOST_VERSION)
    export EXTERNAL_INCLUDES += -I/System/Library/Frameworks/Python.framework/Versions/2.6/include/python2.6

    EXTERNAL_LIBS      = -L$(CANDO_APP_BIN_DIR)
    EXTERNAL_LIBS	+= -lboost_python
    EXTERNAL_LIBS	+= -lboost_filesystem
    EXTERNAL_LIBS	+= -lboost_program_options
    EXTERNAL_LIBS	+= -lexpat
    EXTERNAL_LIBS	+= -L/System/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/config -lpython2.6
    export SHARED_FLAGS = -bundle -flat_namespace -undefined suppress
    export PYTHON_VERSION=python26
    export PYTHON_EMBED_TOP=$(HOME)/Development/mbb_Python-2.6.1
    export mbbCxx_TARGET += staticLib
    export smarts_TARGET += staticLib
    export forceField_TARGETS += staticLib
    export AWK = awk
    export BJAM_EXE_LOCAL_DIR = bin.macosxx86_64
    export BJAM_EXE_DIR = $(BJAM_SOURCE_DIR)/$(BJAM_EXE_LOCAL_DIR)
endif





###########################################################
#
# MacOS X using clang
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),darwinClang)
    export CANDO_COMPILER=clang
    export AWK = awk
    export CTAGS = /usr/local/bin/ctags

    export CANDO_APP_ROOT = $(OS_BUILDTOP)/cando.app
    export CANDO_CONTENTS_DIR = $(CANDO_APP_ROOT)/Contents
    export CANDO_APP_BIN_DIR = $(CANDO_CONTENTS_DIR)/MacOS
    export CANDO_APP_EXAMPLES_DIR = $(CANDO_RESOURCES_DIR)/examples
    export CANDO_APP_LIB_DIR = $(CANDO_RESOURCES_DIR)/lib
    export CANDO_RESOURCES_DIR = $(CANDO_CONTENTS_DIR)/Resources
    export CANDO_RESOURCES_BIN_DIR = $(CANDO_CONTENTS_DIR)/Resources/bin
    export CANDO_RESOURCES_CSC_DIR = $(CANDO_CONTENTS_DIR)/Resources/csc
    export CANDO_RESOURCES_MODULE_DIR = $(CANDO_RESOURCES_DIR)/modules
    export CANDO_RESOURCES_DATABASE_DIR = $(CANDO_RESOURCES_DIR)/databases

bundleDirectories:
	install -d $(CANDO_CONTENTS_DIR)
	install -d $(CANDO_APP_BIN_DIR)
	install -d $(CANDO_APP_LIB_DIR)
	install	-d $(CANDO_RESOURCES_DIR)
	install	-d $(CANDO_RESOURCES_MODULE_DIR)
	install	-d $(CANDO_RESOURCES_BIN_DIR)
	install	-d $(CANDO_RESOURCES_CSC_DIR)
	install	-d $(CANDO_RESOURCES_DATABASE_DIR)
	install	-d $(CANDO_APP_EXAMPLES_DIR)

    export BOOST_TOOLSET=darwin
#    export BOOST_INCLUDE_DIR=$(EXTERNALS_DIR)/include/boost-$(BOOST_VERSION)/
#    export PYTHON_INCLUDE_DIR=/usr/include/python2.6
#    export EXTERNAL_INCLUDES = -I/usr/include
#    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include
#    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include/boost-$(BOOST_VERSION)
#    export EXTERNAL_INCLUDES += -I/System/Library/Frameworks/Python.framework/Versions/2.6/include/python2.6

#     EXTERNAL_LIBS      = -L$(CANDO_APP_BIN_DIR)
#     EXTERNAL_LIBS	+= -lboost_python
#     EXTERNAL_LIBS	+= -lboost_filesystem
#     EXTERNAL_LIBS	+= -lboost_program_options
#     EXTERNAL_LIBS	+= -lexpat
#     EXTERNAL_LIBS	+= -L/System/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/config -lpython2.6
#    export SHARED_FLAGS = -bundle -flat_namespace -undefined suppress
#    export PYTHON_VERSION=python26
#    export PYTHON_EMBED_TOP=$(HOME)/Development/mbb_Python-2.6.1
#    export mbbCxx_TARGET += staticLib
#    export smarts_TARGET += staticLib
#    export forceField_TARGETS += staticLib
#    export AWK = awk
#    export BJAM_EXE_LOCAL_DIR = bin.macosxx86_64
#    export BJAM_EXE_DIR = $(BJAM_SOURCE_DIR)/$(BJAM_EXE_LOCAL_DIR)

endif



###########################################################
#
# MacOS X
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),darwin)
    export CANDO_OS = darwin
    export CANDO_COMPILER=gnu
    export AWK = awk
    export CTAGS = /usr/local/bin/ctags

    export CANDO_APP_ROOT = $(OS_BUILDTOP)/cando.app
    export CANDO_CONTENTS_DIR = $(CANDO_APP_ROOT)/Contents
    export CANDO_APP_BIN_DIR = $(CANDO_CONTENTS_DIR)/MacOS
    export CANDO_APP_EXAMPLES_DIR = $(CANDO_RESOURCES_DIR)/examples
    export CANDO_APP_LIB_DIR = $(CANDO_RESOURCES_DIR)/lib
    export CANDO_RESOURCES_DIR = $(CANDO_CONTENTS_DIR)/Resources
    export CANDO_RESOURCES_BIN_DIR = $(CANDO_CONTENTS_DIR)/Resources/bin
    export CANDO_RESOURCES_CSC_DIR = $(CANDO_CONTENTS_DIR)/Resources/csc
    export CANDO_RESOURCES_MODULE_DIR = $(CANDO_RESOURCES_DIR)/modules
    export CANDO_RESOURCES_DATABASE_DIR = $(CANDO_RESOURCES_DIR)/databases

bundleDirectories:
	install -d $(CANDO_CONTENTS_DIR)
	install -d $(CANDO_APP_BIN_DIR)
	install -d $(CANDO_APP_LIB_DIR)
	install	-d $(CANDO_RESOURCES_DIR)
	install	-d $(CANDO_RESOURCES_MODULE_DIR)
	install	-d $(CANDO_RESOURCES_BIN_DIR)
	install	-d $(CANDO_RESOURCES_CSC_DIR)
	install	-d $(CANDO_RESOURCES_DATABASE_DIR)
	install	-d $(CANDO_APP_EXAMPLES_DIR)

    export BOOST_TOOLSET=darwin
    export BOOST_INCLUDE_DIR=$(EXTERNALS_DIR)/include/boost-$(BOOST_VERSION)/
    export PYTHON_INCLUDE_DIR=/usr/include/python2.6
    export EXTERNAL_INCLUDES = -I/usr/include
    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include
    export EXTERNAL_INCLUDES += -I$(OS_BUILDTOP)/include/boost-$(BOOST_VERSION)
    export EXTERNAL_INCLUDES += -I/System/Library/Frameworks/Python.framework/Versions/2.6/include/python2.6

    EXTERNAL_LIBS      = -L$(CANDO_APP_BIN_DIR)
    EXTERNAL_LIBS	+= -lboost_python
    EXTERNAL_LIBS	+= -lboost_filesystem
    EXTERNAL_LIBS	+= -lboost_program_options
    EXTERNAL_LIBS	+= -lexpat
    EXTERNAL_LIBS	+= -L/System/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/config -lpython2.6
    export SHARED_FLAGS = -bundle -flat_namespace -undefined suppress
    export PYTHON_VERSION=python26
    export PYTHON_EMBED_TOP=$(HOME)/Development/mbb_Python-2.6.1
    export mbbCxx_TARGET += staticLib
    export smarts_TARGET += staticLib
    export forceField_TARGETS += staticLib
    export AWK = awk
    export BJAM_EXE_LOCAL_DIR = bin.macosxx86_64
    export BJAM_EXE_DIR = $(BJAM_SOURCE_DIR)/$(BJAM_EXE_LOCAL_DIR)

endif





###########################################################
#
# Windows
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),Windows_NT)
    export CANDO_APP_ROOT = $(OS_BUILDTOP)/cando
    export CANDO_OS = Windows_NT
    export CANDO_COMPILER=visualc
    export AWK = awk
    export CTAGS = /usr/local/bin/ctags

    export EXPAT_LIB_DIR=/cygdrive/c/Expat201/Bin

    export BOOST_TOOLSET=vc7
    export BOOST_BUILD_OPTIONS=""
    export BJAM=$(EXTERNALS_DIR)/src/boost-jam-$(BJAM_VERSION)/bin.ntx86/bjam.exe

    export BOOST_INCLUDES=DELETE_ME
    export WIN_BOOST_INCLUDES=$(shell cygpath -w $(EXTERNALS_DIR)/include/boost-$(BOOST_VERSION))
    export WIN_EXPAT_INCLUDES=$(shell cygpath -w "/cygdrive/c/Expat201/Source/lib")
    export WIN_PYTHON_INCLUDES=$(shell cygpath -w "/cygdrive/c/Python26/include")
    export WIN_PYTHON_LIB_DIR=$(shell cygpath -w "/cygdrive/c/Python26/libs")
    export WIN_WX_INCLUDES=$(shell cygpath -w "/cygdrive/c/wxWidgets-2.8.10/include")




# Lumping together includes into EXTERNAL_INCLUDES causes lots of
# problems when passed to "cl" so pass them separately

    export LIB_DIR = $(OS_BUILDTOP)/lib
    export WIN_LIB_DIR = $(shell cygpath -w $(LIB_DIR))
    export WIN_BOOST_LIB_DIR = $(WIN_LIB_DIR)
    export EXTERNAL_LIBS = -L$(LIBRARY_DIR)/lib 
    export EXTERNAL_LIBS+= -L/usr/local/lib
    export EXTERNAL_LIBS+= -lboost_python
    export EXTERNAL_LIBS+= -lboost_filesystem
    export EXTERNAL_LIBS+= -lexpat
    export EXTERNAL_LIBS+= -L$(PYTHON_LIB_DIR) -lpython26


mbbTargets: libmbbCxx.dll cando

    export CANDO_APP_ROOT = $(OS_BUILDTOP)/cando2
    export CANDO_CONTENTS_DIR = $(CANDO_APP_ROOT)
    export CANDO_APP_BIN_DIR = $(CANDO_CONTENTS_DIR)/bin
    export CANDO_APP_LIB_DIR = $(CANDO_APP_BIN_DIR)
    export CANDO_RESOURCES_DIR = $(CANDO_CONTENTS_DIR)/Resources
    export CANDO_RESOURCES_MODULE_DIR = $(CANDO_RESOURCES_DIR)/modules
    export CANDO_RESOURCES_BIN_DIR = $(CANDO_RESOURCES_DIR)/bin
    export CANDO_RESOURCES_CSC_DIR = $(CANDO_RESOURCES_DIR)/csc
    export CANDO_RESOURCES_DATABASE_DIR = $(CANDO_RESOURCES_DIR)/databases
    export CANDO_APP_EXAMPLES_DIR = $(CANDO_RESOURCES_DIR)/examples

bundle:
	-rm -f $(CANDO_APP_BIN_DIR)/*.lib
	-rm -f $(CANDO_APP_BIN_DIR)/*.dll
	install -d $(CANDO_CONTENTS_DIR)
	install -d $(CANDO_APP_BIN_DIR)
	install -d $(CANDO_APP_LIB_DIR)
	install	-d $(CANDO_RESOURCES_DIR)
	install	-d $(CANDO_RESOURCES_MODULE_DIR)
	install	-d $(CANDO_RESOURCES_BIN_DIR)
	install	-d $(CANDO_RESOURCES_DATABASE_DIR)
	install	-d $(CANDO_APP_EXAMPLES_DIR)
	install -d $(LIB_DIR)
	make -C externals subBundle
	make -C src subBundle

endif








export CANDO_OS
export CANDO_COMPILER
export EXTERNAL_INCLUDES
export EXTERNAL_LIBS
export CYGWIN_EXTERNAL_INCLUDES




current:
	make -C $(CANDO_HOME) subAll


loginScript:
	@echo '#'
	@echo '#'
	@echo '#'
	@echo '# Cando login script'
	@echo '#'
	@echo 'export DEVELOPMENT_ENVIRONMENT=(darwin|linux|opteron|Windows_NT)'
	@echo 'export CANDO_HOME=<Cando_home_directory>'
	@echo 'export CANDO_LOG=_dbg.dbg'
	@echo 'export COMPILE_PROCESSORS=<_number_of_processors_for_compilations_>'
	@echo 'export LD_LIBRARY_PATH=$$CANDO_HOME/externals/lib'
	@echo 'export DYLD_LIBRARY_PATH=$$CANDO_HOME/externals/lib'
	@echo 'export PYTHONPATH=$$CANDO_HOME/build/$$DEVELOPMENT_ENVIRONMENT/lib'
	@echo 'export PYTHONPATH=$$CANDO_HOME/build/$$DEVELOPMENT_ENVIRONMENT/modules'
	@echo '#'
	@echo '#'
	@echo '#'
	@echo '#'

ifndef DEVELOPMENT_ENVIRONMENT
	echo "You must define DEVELOPMENT_ENVIRONMENT"
	echo Type:  \"make loginScript\" to see how to set up your login
	exit 1
endif

ifndef CANDO_HOME
	echo "You must define CANDO_HOME"
	exit 1
endif


mbb:
	make -C $(mbb_SOURCE_DIR) subAll

gimbalModel gm gim:
	make -C $(gimbalModel_SOURCE_DIR) subAll


$(mbb_MODULE_DIR): $(MODULE_DIR)
	-install -d $(mbb_MODULE_DIR)
	touch $(mbb_MODULE_DIR)

$(buildDatabase_MODULE_DIR): $(MODULE_DIR)
	install -d $(buildDatabase_MODULE_DIR)
	touch $(buildDatabase_MODULE_DIR)

$(LIBRARY_DIR): $(OS_BUILDTOP)
	-install -d  $(LIBRARY_DIR)

$(MODULE_DIR): $(OS_BUILDTOP)
	-install -d  $(MODULE_DIR)

$(OS_BUILDTOP): $(BUILDTOP)
	-install -d  $(OS_BUILDTOP)


$(DATABASE_DIR):
	-install -d  $(DATABASE_DIR)

$(core_DATABASE_DIR):
	-install -d  $(core_DATABASE_DIR)


directories:
	make 


dox:
	make -C $(CLASP_HOME)/docs dox 2>doxygen.err
	@echo !
	@echo !
	@echo ! Wrote doxygen error file to: doxygen.err
	@echo !
	@echo ! In vim use: cfile doxygen.err
	@echo !


parsers:
	make -C $(CANDO_HOME) subParsers


copyPythonLibraries:
	echo Nothing
#	cp $(WIN_PYTHON_ROOT)/

copyWindowsLibraries:
	cp /cygdrive/c/Windows/system32/msvcp71.dll $(DEST)
	cp /cygdrive/c/Windows/system32/msvcr71.dll $(DEST)
	cp /cygdrive/c/Windows/system/glut32.dll $(DEST)




############################################################
#
#
# Dont change below here
#
#



#
#
# Microsoft windows stuff
#
#
#

export VSINSTALLDIR=C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE
export VCINSTALLDIR=C:\Program Files\Microsoft Visual Studio .NET 2003
export FrameworkDir=C:\WINDOWS\Microsoft.NET\Framework
export FrameworkVersion=v1.1.4322
export FrameworkSDKDir=C:\Program Files\Microsoft Visual Studio .NET 2003\SDK\v1.1



export uVSINSTALLDIR=/cygdrive/c/Program Files/Microsoft Visual Studio .NET 2003/Common7/IDE
export uVCINSTALLDIR=/cygdrive/c/Program Files/Microsoft Visual Studio .NET 2003
export uFrameworkDir=/cygdrive/c/WINDOWS/Microsoft.NET/Framework
export uFrameworkVersion=v.1.1.4322
export uFrameworkSDKDir=/cygdrive/c/Program Files/Microsoft Visual Studio .NET 2003/SDK/v1.1


#@rem
#@rem Root of Visual Studio ide installed files.
#@rem
export DevEnvDir=$(VSINSTALLDIR)
export uDevEnvDir=$(uVSINSTALLDIR)


#
#@rem
#@rem Root of Visual C++ installed files.
#@rem
export MSVCDir=$(VCINSTALLDIR)\Vc7
export uMSVCDir=$(uVCINSTALLDIR)/Vc7



PATH:=$(uDevEnvDir):$(uMSVCDir)/bin:$(uVCINSTALLDIR)/Common7/Tools:$(uVCINSTALLDIR)/Common7/Tools/bin/prerelease:$(uVCINSTALLDIR)/Common7/Tools/bin:$(uFrameworkSDKDir)/bin:$(uFrameworkDir)/$(uFrameworkVersion):$(PATH)

export INCLUDE=$(MSVCDir)\atlmfc\include;$(MSVCDir)\INCLUDE;$(MSVCDir)\PlatformSDK\include\prerelease;$(MSVCDir)\PlatformSDK\include;$(FrameworkSDKDir)\include


export LIB=$(VCINSTALLDIR)\Common7\IDE;$(MSVCDir)\atlmfc\lib;$(MSVCDir)\LIB;$(MSVCDir)\PlatformSDK\lib\prerelease;$(MSVCDir)\PlatformSDK\lib;$(FrameworkSDKDir)\lib

INTEL_COMPILER80=C:\Program Files\Intel\Fortran\Compiler80
INTEL_SHARED=C:\Program Files\Common Files\Intel\Shared Files
uINTEL_COMPILER80=/cygdrive/c/Program Files/Intel/Fortran/Compiler80
uINTEL_SHARED=/cygdrive/c/Program Files/Common Files/Intel/Shared Files

INTEL_LICENSE_FILE=C:\Program Files\Common Files\Intel\Licenses

export PATH:=$(uINTEL_COMPILER80)/Ia32/Bin:$(uINTEL_SHARED)/Ia32/Bin:$(PATH)

export LIB+=$(INTEL_COMPILER80)\Ia32\Lib;$(INTEL_SHARED)\Ia32\Lib;

export INCLUDE:=$(INTEL_COMPILER80)\Ia32\Include;$(INCLUDE)

export APPROVE_TEST=$(COMMON_DIR)/approveTest


install:
	echo Do nothing

clean:
	make -C $(CANDO_HOME) subClean

#
# Searches for python module initializers
#
ct classTree mi metaInfo:
	python $(COMMON_DIR)/registerClasses.py initClasses.inc initScripting.inc *Package.h otherPackageClasses.h *.h *.cc *.inc > registerClasses.log

runce:
	make runCandoEdit

runced:
	make runCandoEditD

runCandoEdit:
	cd $(INSTALLDIR);\
	$(PYTHON_ROOT)/$(PYTHON) "$(shell cygpath -w $(OS_BUILDTOP)/candoEdit/candoEdit.py)"

runCandoEditD:
	cd $(INSTALLDIR);\
	$(PYTHON_ROOT)/$(PYTHON) "$(shell cygpath -w $(OS_BUILDTOP)/candoEdit/candoEdit.py)" -d

runGlView:
	cd $(INSTALLDIR);\
	$(PYTHON_ROOT)/$(PYTHON) "$(shell cygpath -w $(OS_BUILDTOP)/candoEdit/glView.py)" -d

#tags:
#	echo do nothing
#	cd $(CANDO_HOME)/src;\
#	make srcTags



updateDatabases:
	scp -C $(CANDO_MASTER_HOST):Development/cando/data/* $(CANDO_APP_RESOURCES_DATABASE_DIR)/
