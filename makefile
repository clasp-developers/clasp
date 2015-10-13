# -*- Mode: GNUmakefile -*-
# Cleaned up by Shinmera October 13, 2015

export CLASP_HOME := $(or $(wildcard $(CLASP_HOME)),\
                          $(shell pwd))

include $(wildcard $(CLASP_HOME)/local.config)

export PJOBS ?= 1

export TARGET_OS ?= $(shell uname)
export TARGET_OS := $(or $(filter $(TARGET_OS), Linux),\
                         $(filter $(TARGET_OS), Darwin),\
                         $(error Invalid TARGET_OS: $(TARGET_OS)))

export ADDRESS-MODEL ?= 64
export ADDRESS-MODEL := $(or $(filter $(ADDRESS-MODEL), 64),\
                             $(error Invalid ADDRESS-MODEL: $(ADDRESS-MODEL)))

export LINK ?= shared
export LINK := $(or $(filter $(LINK), shared),\
                    $(filter $(LINK), static),\
                    $(error Invalid LINK: $(LINK))


export VARIANT ?= release
export VARIANT := $(or $(filter $(VARIANT), debug),\
                       $(filter $(VARIANT), release),\
                       $(error Invalid VARIANT: $(VARIANT)))

export TOOLSET ?= $(or $(and $(filter $(TARGET_OS),Linux), clang-linux),\
                       $(and $(filter $(TARGET_OS),Darwin), clang-darwin))
export TOOLSET := $(or $(filter $(TOOLSET), clang-linux),\
                       $(filter $(TOOLSET), clang-darwin),\
                       $(error Invalid TOOLSET: $(TOOLSET)))

export PYTHON2 := $(or $(wildcard $(PYTHON2)),\
                       $(wildcard /usr/bin/python2.7),\
                       $(wildcard /usr/bin/python2),\
                       $(wildcard /usr/bin/python),\
                       $(error Could not find python.))

export EXECUTABLE_DIR ?= $(or $(and $(filter $(TARGET_OS),Linux), bin),\
                              $(and $(filter $(TARGET_OS),Darwin), MacOS))

export DEVEMACS ?= $(or $(and $(filter $(TARGET_OS),Linux), emacs -nw ./),\
                        $(and $(filter $(TARGET_OS),Darwin), open -n -a emacs ./))

export LLVM_CONFIG := $(or $(wildcard $(LLVM_CONFIG)),\
                           $(wildcard /usr/bin/llvm-config),\
                           $(wildcard /usr/bin/llvm-config*),\
                           $(error Could not find llvm-config.))

export GIT_COMMIT ?= $(shell git rev-parse --short HEAD || echo "unknown-commit")
export CLASP_VERSION ?= $(shell git describe --always || echo "unknown-version")

export LLVM_CONFIG_DEBUG ?= $(LLVM_CONFIG)
export LLVM_CONFIG_RELEASE ?= $(LLVM_CONFIG)

export LLVM_BIN_DIR ?= $(shell $(LLVM_CONFIG_RELEASE) --bindir)

export CLASP_INTERNAL_BUILD_TARGET_DIR ?= $(shell pwd)/build/clasp

export LIBATOMIC_OPS_SOURCE_DIR ?= $(CLASP_HOME)/src/boehm/libatomic_ops
export BOEHM_SOURCE_DIR ?= $(CLASP_HOME)/src/boehm/bdwgc
export BOOST_BUILD_SOURCE_DIR ?= $(CLASP_HOME)/tools/boost_build
export BOOST_BUILD_INSTALL ?= $(BOOST_BUILD_SOURCE_DIR)

export BUILD ?= $(CLASP_HOME)/src/common/build
export BJAM ?= $(BOOST_BUILD_INSTALL)/bin/bjam --ignore-site-config --user-config= -q
export CLASP_APP_EXECS ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/execs
export CLASP_APP_RESOURCES_DIR ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources
export CLASP_APP_RESOURCES_LIB_COMMON_DIR ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common

export CLASP_DEBUG_LLVM_LIB_DIR ?= $(shell $(LLVM_CONFIG_DEBUG) --libdir | tr -d '\n')
export CLASP_RELEASE_LLVM_LIB_DIR ?= $(shell $(LLVM_CONFIG_RELEASE) --libdir | tr -d '\n')

export CLASP_DEBUG_CXXFLAGS += -I$(shell $(LLVM_CONFIG_DEBUG) --includedir)
export CLASP_DEBUG_LINKFLAGS += -L$(CLASP_DEBUG_LLVM_LIB_DIR)
export CLASP_DEBUG_LINKFLAGS += $(shell $(LLVM_CONFIG_DEBUG) --libs)
export CLASP_DEBUG_LINKFLAGS += $(shell $(LLVM_CONFIG_DEBUG) --system-libs)
export CLASP_RELEASE_CXXFLAGS += -I$(shell $(LLVM_CONFIG_RELEASE) --includedir)
export CLASP_RELEASE_LINKFLAGS += -L$(CLASP_RELEASE_LLVM_LIB_DIR)
export CLASP_RELEASE_LINKFLAGS += $(shell $(LLVM_CONFIG_RELEASE) --libs)
export CLASP_RELEASE_LINKFLAGS += $(shell $(LLVM_CONFIG_RELEASE) --system-libs)

export BINDIR ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/$(EXECUTABLE_DIR)
export EXECS ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/execs/

export EXTRA_PATH += :$(LLVM_BIN_DIR)
export EXTRA_PATH += :$(CLASP_HOME)/src/common
export EXTRA_PATH += :$(BOOST_BUILD_INSTALL)/bin
export EXTRA_PATH += :$(BINDIR)
export PATH := $(EXTRA_PATH):$(PATH)

ifneq ($(CXXFLAGS),)
  export USE_CXXFLAGS := cxxflags=$(CXXFLAGS)
endif

define varprint
	@echo -e "\033[0;32m$(strip $(1))\033[0m: $($(strip $(1)))"
endef

all:
	make print-config
	make submodules
	make asdf
	make boost_build
	make boehm
	(cd src/lisp; $(BJAM) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp gc=boehm bundle )
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/$(VARIANT) gc=boehm $(VARIANT) clasp_install )
	make -C src/main min-boehm
	make -C src/main bclasp-boehm
	make -C src/main cclasp-boehm-bitcode
	make -C src/main cclasp-boehm-fasl
	make -C src/main cclasp-boehm-addons
	make executable-symlinks
	echo Clasp is now built

mps-build:
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/$(VARIANT) gc=mps $(VARIANT) clasp_install )
	make -C src/main link-min.lsp

boot:
	make submodules
	make asdf
	make boost_build
	make boehm
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/$(VARIANT) gc=boehmdc $(VARIANT) clasp_install )
	make -C src/main min-boehmdc
	make -C src/main bclasp-boehmdc
	make -C src/main bclasp-boehmdc-addons
	make -C src/main mps-interface
	make executable-symlinks
#	make -C src/main bclasp-boehmdc
#	make -C src/main bclasp-boehmdc-addons

clasp-libraries:
	(cd src/gctools; $(BJAM) link=$(LINK) program=clasp gctools install-lib)

devbuild:
	(cd src/main; $(BUILD) -j$(PJOBS) link=$(LINK) program=clasp gc=boehmdc release dist )

$(BINDIR)/clasp_boehm_o : $(BINDIR)/release/boehm/clasp
	@ln -s $(BINDIR)/release/boehm/clasp $(BINDIR)/clasp_boehm_o

$(BINDIR)/clasp_mps_o : $(BINDIR)/release/boehm/clasp
	echo $< $>

executable-symlinks:
	install -d $(BINDIR)
	ln -sf $(EXECS)/boehm/release/bin/clasp $(BINDIR)/clasp_boehm_o
	ln -sf $(EXECS)/boehmdc/release/bin/clasp $(BINDIR)/clasp_boehmdc_o
	ln -sf $(EXECS)/mps/release/bin/clasp $(BINDIR)/clasp_mps_o
	ln -sf $(EXECS)/boehm/debug/bin/clasp $(BINDIR)/clasp_boehm_d
	ln -sf $(EXECS)/boehmdc/debug/bin/clasp $(BINDIR)/clasp_boehmdc_d
	ln -sf $(EXECS)/mps/debug/bin/clasp $(BINDIR)/clasp_mps_d

libatomic-setup:
	-(cd $(LIBATOMIC_OPS_SOURCE_DIR); autoreconf -vif)
	-(cd $(LIBATOMIC_OPS_SOURCE_DIR); automake --add-missing )
	install -d $(CLASP_APP_RESOURCES_LIB_COMMON_DIR);
	(cd $(LIBATOMIC_OPS_SOURCE_DIR); \
		export ALL_INTERIOR_PTRS=1; \
		CFLAGS="-DUSE_MMAP -g" \
		./configure --enable-shared=yes --enable-static=yes --enable-handle-fork --enable-cplusplus --prefix=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR);)

libatomic-compile:
	(cd $(LIBATOMIC_OPS_SOURCE_DIR); make -j$(PJOBS) | tee _libatomic_ops.log)
	(cd $(LIBATOMIC_OPS_SOURCE_DIR); make -j$(PJOBS) install | tee _libatomic_ops_install.log)

boehm-setup:
	-(cd $(BOEHM_SOURCE_DIR); autoreconf -vif)
	-(cd $(BOEHM_SOURCE_DIR); automake --add-missing )
	(cd $(BOEHM_SOURCE_DIR); \
		export ALL_INTERIOR_PTRS=1; \
		CFLAGS="-DUSE_MMAP -g" \
		PKG_CONFIG_PATH=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR)/lib/pkgconfig/ \
		./configure --enable-shared=yes --enable-static=yes --enable-handle-fork --enable-cplusplus --prefix=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR);)

boehm-compile:
	(cd $(BOEHM_SOURCE_DIR); make -j$(PJOBS) | tee _boehm.log)
	(cd $(BOEHM_SOURCE_DIR); make -j$(PJOBS) install | tee _boehm_install.log)


boehm:
	@if test ! -e src/boehm/libatomic_ops/configure; then make libatomic-setup ; fi
	@if test ! -e $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common/lib/libatomic_ops.a ; then make libatomic-compile ; fi
	@if test ! -e src/boehm/bdwgc/configure ; then make boehm-setup ; fi
	@if test ! -e $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common/lib/libgc.a ; then make boehm-compile ; fi


boehm-release-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/release gc=boehm release clasp-clbind-install)

boehm-debug-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/debug gc=boehm debug clasp-clbind-install)

boehmdc-release-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/release gc=boehmdc release clasp-clbind-install)

mps-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/$(VARIANT) gc=mps $(VARIANT) clasp-clbind-install)

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
	make boost_build
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
	-git submodule update --init tools/boost_build
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
	make boost_build
	make clasp-boehm

boehm-build-mps-interface:
	make submodules
	make boost_build
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
export DEV_CLASP_LISP_SOURCE_DIR := $(shell pwd)/src/lisp

devemacs:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) $(DEVEMACS))

devemacs_no_clasp_lisp_source_dir:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	$(DEVEMACS)

devshell:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) bash)


devshell-telemetry:
	@echo This shell sets up environment variables like BJAM
	@echo as they are defined when commands execute within the makefile
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR); export CLASP_MPS_CONFIG="32 32 16 80 32 80"; export CLASP_TELEMETRY_FILE=/tmp/clasp.tel; export CLASP_TELEMETRY_MASK=3; bash)


testing:
	which clang++

clasp-mps-cpp:
	$(BUILD) -j$(PJOBS) gc=mps link=$(LINK) program=clasp release src/main//dist

clasp-boehm-cpp:
	$(BUILD) -j$(PJOBS) gc=boehm link=$(LINK) program=clasp release src/main//dist

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
	$(BJAM) everything gc=boehm link=$(LINK) program=clasp release
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

boost_build:
	@if test ! -e tools/boost_build/bin/bjam ; then make boost_build-compile ; fi

boost_build-compile:
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

dump-local-config:
	cat $(CLASP_HOME)/local.config

print-config:
	$(info >> Makefile Configuration:)
	$(call varprint, CLASP_HOME)
	$(call varprint, LLVM_CONFIG)
	$(call varprint, TARGET_OS)
	$(call varprint, ADDRESS-MODEL)
	$(call varprint, LINK)
	$(call varprint, PJOBS)
	$(call varprint, LLVM_CONFIG_DEBUG)
	$(call varprint, LLVM_CONFIG_RELEASE)
	$(call varprint, LLVM_BIN_DIR)
	$(call varprint, GIT_COMMIT)
	$(call varprint, CLASP_VERSION)
	$(call varprint, CLASP_INTERNAL_BUILD_TARGET_DIR)
	$(call varprint, LIBATOMIC_OPS_SOURCE_DIR)
	$(call varprint, BOEHM_SOURCE_DIR)
	$(call varprint, BOOST_BUILD_SOURCE_DIR)
	$(call varprint, BOOST_BUILD_INSTALL)
	$(call varprint, BJAM)
	$(call varprint, BUILD)
	$(call varprint, CLASP_APP_EXECS)
	$(call varprint, CLASP_APP_RESOURCES_DIR)
	$(call varprint, CLASP_APP_RESOURCES_LIB_COMMON_DIR)
	$(call varprint, CLASP_DEBUG_LLVM_LIB_DIR)
	$(call varprint, CLASP_RELEASE_LLVM_LIB_DIR)
	$(call varprint, CLASP_DEBUG_CXXFLAGS)
	$(call varprint, CLASP_DEBUG_LINKFLAGS)
	$(call varprint, CLASP_RELEASE_CXXFLAGS)
	$(call varprint, CLASP_RELEASE_LINKFLAGS)
	$(call varprint, VARIANT)
	$(call varprint, TOOLSET)
	$(call varprint, DEVEMACS)
	$(call varprint, PYTHON2)
	$(call varprint, EXECUTABLE_DIR)
	$(call varprint, BINDIR)
	$(call varprint, EXECS)
	$(call varprint, PATH)
	$(call varprint, USE_CXXFLAGS)
