include local.config

export CLASP_HOME = $(shell pwd)

export GIT_COMMIT := $(shell git describe --match='' --always || echo "unknown-commit")
export CLASP_VERSION := $(shell git describe --always || echo "unknown-version")

export CLASP_INTERNAL_BUILD_TARGET_DIR = $(shell pwd)/build/clasp
export EXTERNALS_BUILD_TARGET_DIR = $(EXTERNALS_SOURCE_DIR)/build


export LIBATOMIC_OPS_SOURCE_DIR = $(CLASP_HOME)/src/boehm/libatomic_ops
export BOEHM_SOURCE_DIR = $(CLASP_HOME)/src/boehm/bdwgc
export BOOST_BUILD_SOURCE_DIR = $(CLASP_HOME)/boost_build_v2
export BOOST_BUILD_INSTALL = $(BOOST_BUILD_SOURCE_DIR)

#export BOOST_BUILD_SOURCE_DIR = $(CLASP_HOME)/boost_build_v2
#export BOOST_BUILD_INSTALL = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/boost_build_v2


export BJAM = $(BOOST_BUILD_INSTALL)/bin/bjam --ignore-site-config --user-config= -q
export BUILD = build
export CLASP_APP_RESOURCES_DIR = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources
export CLASP_APP_RESOURCES_LIB_COMMON_DIR = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common

export PS1 := $(shell printf 'CLASP-ENV>>[\\u@\\h \\W]$ ')

ifeq ($(TARGET_OS),linux)
  export DEVEMACS = emacs -nw ./
else
  export DEVEMACS = open -n -a emacs ./
endif


#
# If the local.config doesn't define PYTHON2 then provide a default
#
ifeq ($(PYTHON2),)
	export PYTHON2 = /usr/bin/python
endif

ifeq ($(TARGET_OS),linux)
  export EXECUTABLE_DIR=bin
endif
ifeq ($(TARGET_OS),darwin)
  export EXECUTABLE_DIR=MacOS
endif

ifneq ($(EXTERNALS_BUILD_TARGET_DIR),)
	PATH := $(EXTERNALS_BUILD_TARGET_DIR)/release/bin:$(EXTERNALS_BUILD_TARGET_DIR)/common/bin:$(PATH)
	export PATH
endif

export BINDIR = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/$(EXECUTABLE_DIR)

export PATH := $(CLASP_HOME)/src/common:$(BOOST_BUILD_INSTALL)/bin:$(PATH)
export PATH := $(BINDIR):$(PATH)


ifneq ($(CXXFLAGS),)
  export USE_CXXFLAGS := cxxflags=$(CXXFLAGS)
endif


ifeq ($(WHAT),)
	WHAT = bundle debug release boehm mps
endif

all:
	@echo Dumping local.config
	cat local.config
	make submodules
	make asdf
	make boostbuildv2-build
	make boehm
	(cd src/main; $(BUILD) -j$(PJOBS) link=static program=clasp gc=boehm release dist )
	(cd src/main; $(BUILD) -j$(PJOBS) link=static program=clasp gc=mps release dist )
	time make -C src/main bclasp-boehm
	time make -C src/main cclasp-boehm
	time make -c src/main cclasp-boehm-addons
	time make -C src/main link-cclasp-mps
	time make -C src/main link-cclasp-mps-addons
	make executable-symlinks

boot:
	cat local.config
	make submodules
	make asdf
	make boostbuildv2-build
	make boehm
	(cd src/main; $(BUILD) -j$(PJOBS) link=static program=clasp gc=boehmdc release dist )
	time make -C src/main bclasp-boehmdc
	time make -C src/main bclasp-boehmdc-addons

clasp-libraries:
	(cd src/gctools; $(BJAM) link=static program=clasp gc=boehmdc threading=single gctools install-lib)

devbuild:
	(cd src/main; $(BUILD) -j$(PJOBS) link=static program=clasp gc=boehmdc release dist )

$(BINDIR)/clasp_boehm_o : $(BINDIR)/release/boehm/clasp
	@ln -s $(BINDIR)/release/boehm/clasp $(BINDIR)/clasp_boehm_o

$(BINDIR)/clasp_mps_o : $(BINDIR)/release/boehm/clasp
	echo $< $>

executable-symlinks:
	@if [ -e $(BINDIR)/release/boehm/clasp -a \( ! -e $(BINDIR)/clasp_boehm_o \) ]; then ln -s $(BINDIR)/release/boehm/clasp $(BINDIR)/clasp_boehm_o; fi
	@if [ -e $(BINDIR)/release/mps/clasp -a \( ! -e $(BINDIR)/clasp_mps_o \) ]; then ln -s $(BINDIR)/release/mps/clasp $(BINDIR)/clasp_mps_o; fi
	@if [ -e $(BINDIR)/release/boehmdc/clasp -a \( ! -e $(BINDIR)/clasp_boehmdc_o \) ]; then ln -s $(BINDIR)/release/boehmdc/clasp $(BINDIR)/clasp_boehmdc_o; fi
	@if [ -e $(BINDIR)/debug/boehm/clasp -a \( ! -e $(BINDIR)/clasp_boehm_d \) ]; then ln -s $(BINDIR)/debug/boehm/clasp $(BINDIR)/clasp_boehm_d; fi
	@if [ -e $(BINDIR)/debug/mps/clasp -a \( ! -e $(BINDIR)/clasp_mps_d \) ]; then ln -s $(BINDIR)/debug/mps/clasp $(BINDIR)/clasp_mps_d; fi
	@if [ -e $(BINDIR)/debug/boehmdc/clasp -a \( ! -e $(BINDIR)/clasp_boehmdc_d \) ]; then ln -s $(BINDIR)/debug/boehmdc/clasp $(BINDIR)/clasp_boehmdc_d; fi



libatomic-setup:
	-(cd $(LIBATOMIC_OPS_SOURCE_DIR); autoreconf -vif)
	-(cd $(LIBATOMIC_OPS_SOURCE_DIR); automake --add-missing )
	install -d $(CLASP_APP_RESOURCES_LIB_COMMON_DIR);
	(cd $(LIBATOMIC_OPS_SOURCE_DIR); \
		export ALL_INTERIOR_PTRS=1; \
		CFLAGS="-DUSE_MMAP -g" \
		./configure --enable-shared=no --enable-static=yes --enable-handle-fork --enable-cplusplus --prefix=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR);)

libatomic-compile:
	(cd $(LIBATOMIC_OPS_SOURCE_DIR); make -j$(PJOBS) | tee _libatomic_ops.log)

libatomic-install:
	(cd $(LIBATOMIC_OPS_SOURCE_DIR); make -j$(PJOBS) install | tee _libatomic_ops_install.log)

boehm-setup:
	-(cd $(BOEHM_SOURCE_DIR); autoreconf -vif)
	-(cd $(BOEHM_SOURCE_DIR); automake --add-missing )
	(cd $(BOEHM_SOURCE_DIR); \
		export ALL_INTERIOR_PTRS=1; \
		CFLAGS="-DUSE_MMAP -g" \
		PKG_CONFIG_PATH=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR)/lib/pkgconfig/ \
		./configure --enable-shared=no --enable-static=yes --enable-handle-fork --enable-cplusplus --prefix=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR);)

boehm-build:
	make boehm-compile
	make boehm-install

boehm-compile:
	(cd $(BOEHM_SOURCE_DIR); make -j$(PJOBS) | tee _boehm.log)

boehm-install:
	(cd $(BOEHM_SOURCE_DIR); make -j$(PJOBS) install | tee _boehm_install.log)


boehm:
	make libatomic-setup
	make libatomic-compile
	make libatomic-install
	make boehm-setup
	make boehm-compile
	make boehm-install

boehm-clean:
	install -d $(BOEHM_SOURCE_DIR)
	-(cd $(BOEHM_SOURCE_DIR); make clean )


cclasp-mps:
	(cd src/main; make cclasp-mps)

bclasp-only:
	@echo Dumping local.config
	cat local.config
	make submodules
	make asdf
	make boostbuildv2-build
	make -C src/main scrape-all
	$(BJAM) /internals/lisp//bundle
	make clasp-boehm
	make -C src/main bclasp-boehm-addons

sub-prebuild:
	make -C src/ $@

fix-scraping:
	for d in src/*/; do cd "$$d"; export PYTHONPATH="$$PWD:$$PYTHONPATH"; python ../../src/common/symbolScraper.py symbols_scraped.inc *.h *.cc *.scrape.inc; cd ../..; done

fix-scraping2:
	-(cd src/asttooling; bjam meta)
	-(cd src/cffi; bjam meta)
	-(cd src/clbind; bjam meta)
	-(cd src/core; bjam meta)
	-(cd src/gctools; bjam meta)
	-(cd src/llvmo; bjam meta)
	-(cd src/main; bjam meta)
	-(cd src/mpip; bjam meta)
	-(cd src/mps; bjam meta)
	-(cd src/serveEvent; bjam meta)
	-(cd src/sockets; bjam meta)

pump:
	(cd src/core; make pump)
	(cd src/clbind; make pump)

submodules:
	make submodules-boehm
	make submodules-mps

submodules-boehm:
	-git submodule update --init src/boehm/libatomic_ops
	-git submodule update --init src/boehm/bdwgc
	-git submodule update --init src/lisp/kernel/contrib/sicl
	-git submodule update --init src/lisp/modules/asdf
#	-(cd src/lisp/modules/asdf; git checkout master; git pull origin master)

submodules-mps:
	-git submodule update --init src/mps

asdf:
	(cd src/lisp/modules/asdf; make)

only-boehm:
	make submodules-boehm
	make boostbuildv2-build
	make clasp-boehm

boehm-build-mps-interface:
	make submodules
	make boostbuildv2-build
	make clasp-boehm
	(cd src/main; make mps-interface)


#
# Tell ASDF where to find the SICL/Code/Cleavir systems - the final // means search subdirs
#
#export CL_SOURCE_REGISTRY = $(shell echo `pwd`/src/lisp/kernel/contrib/sicl/Code/Cleavir//):$(shell echo `pwd`/src/lisp/kernel/contrib/slime//)

#
# When developing, set the CLASP_LISP_SOURCE_DIR environment variable
# to tell clasp to use the development source directly rather than the
# stuff in the clasp build target directory.  This saves us the trouble of
# constantly having to copy the lisp sources to the target directory.
export DEV_CLASP_LISP_SOURCE_DIR := $(shell echo `pwd`/src/lisp)

devemacs:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	@echo EXTERNALS_BUILD_TARGET_DIR = $(EXTERNALS_BUILD_TARGET_DIR)
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) $(DEVEMACS))

devemacs_no_clasp_lisp_source_dir:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	@echo EXTERNALS_BUILD_TARGET_DIR = $(EXTERNALS_BUILD_TARGET_DIR)
	$(DEVEMACS)

devshell:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	@echo EXTERNALS_BUILD_TARGET_DIR = $(EXTERNALS_BUILD_TARGET_DIR)
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) bash)


devshell-telemetry:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	@echo EXTERNALS_BUILD_TARGET_DIR = $(EXTERNALS_BUILD_TARGET_DIR)
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR); export CLASP_MPS_CONFIG="32 32 16 80 32 80"; export CLASP_TELEMETRY_FILE=/tmp/clasp.tel; export CLASP_TELEMETRY_MASK=3; bash)


testing:
	which clang++

clasp-mps-cpp:
	$(BUILD) -j$(PJOBS) gc=mps link=static program=clasp release src/main//dist

clasp-boehm-cpp:
	$(BUILD) -j$(PJOBS) gc=boehm link=static program=clasp release src/main//dist

clasp-mps:
	make clasp-mps-cpp
	(cd src/main; make mps)

# Compile the CL sources for min-mps: and full-mps
cl-mps:
	(cd src/main; make mps)

# Compile the CL sources for min-mps: using the existing min-mps: - FAST
cl-min-mps-recompile:
	(cd src/main; make min-mps-recompile)

# Compile the CL sources for full-mps:
cl-full-mps:
	(cd src/main; make full-mps)


clasp-boehm:
	make clasp-boehm-cpp
	$(BJAM) everything gc=boehm link=static program=clasp release
	(cd src/main; make boehm)

cclasp-boehm:
	(cd src/main; make cclasp-boehm)

cclasp-boehm-addons:
	(cd src/main; make cclasp-boehm-addons)


# Compile the CL sources for min-boehm: and full-boehm
cl-boehm:
	(cd src/main; make boehm)

# Compile the CL sources for min-boehm: using the existing min-boehm: - FAST
cl-min-boehm-recompile:
	(cd src/main; make min-boehm-recompile)

# Compile the CL sources for full-boehm:
cl-full-boehm:
	(cd src/main; make full-boehm)


boostbuildv2-build:
	(cd $(BOOST_BUILD_SOURCE_DIR); export BOOST_BUILD_PATH=`pwd`; ./bootstrap.sh; ./b2 toolset=clang install --prefix=$(BOOST_BUILD_INSTALL) --ignore-site-config)

compile-commands:
	(cd src/main; make compile-commands)


clean:
	git submodule sync
	(cd src/main; rm -rf bin bundle)
	(cd src/core; rm -rf bin bundle)
	(cd src/gctools; rm -rf bin bundle)
	(cd src/llvmo; rm -rf bin bundle)
	(cd src/asttooling; rm -rf bin bundle)
	(cd src/cffi; rm -rf bin bundle)
	(cd src/clbind; rm -rf bin bundle)
	(cd src/sockets; rm -rf bin bundle)
	(cd src/serveEvent; rm -rf bin bundle)
ifneq ($(CLASP_INTERNAL_BUILD_TARGET_DIR),)
	install -d $(CLASP_INTERNAL_BUILD_TARGET_DIR)
	-(find $(CLASP_INTERNAL_BUILD_TARGET_DIR) -type f -print0 | xargs -0 rm -f)
endif

setup-cleavir:
	clasp_boehm_o -f bclasp -l src/lisp/kernel/cleavir/setup-cclasp-build.lisp -e "(core:quit)"

pull-sicl-master:
	(cd src/lisp/kernel/contrib/sicl; git pull origin master)
	make setup-cleavir

mps-submodule:
	git submodule add -b dev/2014-08-18/non-incremental  https://github.com/Ravenbrook/mps-temporary ./src/mps


asdf-submodule:
	git submodule add --name updatedAsdf https://github.com/drmeister/asdf.git ./src/lisp/kernel/asdf
