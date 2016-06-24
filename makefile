# -*- mode: GNUmakefile; indent-tabs-mode: t -*-
# Cleaned up by Shinmera October 13, 2015

export CLASP_HOME ?= $(shell pwd)

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
                    $(error Invalid LINK: $(LINK)))

export VARIANT ?= release
export VARIANT := $(or $(filter $(VARIANT), debug),\
                       $(filter $(VARIANT), release),\
                       $(error Invalid VARIANT: $(VARIANT)))

export TOOLSET ?= $(or $(and $(filter $(TARGET_OS),Linux), clang-linux),\
                       $(and $(filter $(TARGET_OS),Darwin), clang-darwin))
export TOOLSET := $(or $(filter $(TOOLSET), clang-linux),\
                       $(filter $(TOOLSET), clang-darwin),\
                       $(error Invalid TOOLSET: $(TOOLSET)))

# From the GNU Make manual; portably search PATH for a program. We can't rely on `which` existing...
# Use $(call pathsearch,foo) instead of $(shell which foo)
pathsearch = $(firstword $(wildcard $(addsuffix /$(strip $(1)),$(subst :, ,$(PATH)))))


export CLASP_SBCL := $(or $(CLASP_SBCL),\
			$(call pathsearch, sbcl),\
			$(error Could not find sbcl - it needs to be installed an in your path.))

export PYTHON2 := $(or $(PYTHON2),\
                       $(call pathsearch, python2.7),\
                       $(call pathsearch, python2),\
                       $(call pathsearch, python),\
                       $(error Could not find python.))

export EXECUTABLE_DIR ?= $(or $(and $(filter $(TARGET_OS),Linux), bin),\
                              $(and $(filter $(TARGET_OS),Darwin), MacOS))

export DEVEMACS ?= $(or $(and $(filter $(TARGET_OS),Linux), emacs -nw ./),\
                        $(and $(filter $(TARGET_OS),Darwin), open -n -a /Applications/Emacs.app ./))

ifneq ($(EXTERNALS_CLASP_DIR),)
export LLVM_CONFIG := $(or $(LLVM_CONFIG),\
                           $(wildcard $(EXTERNALS_CLASP_DIR)/build/release/bin/llvm-config),\
                           $(error Could not find llvm-config (release build) in externals-clasp.))
export LLVM_CONFIG_DEBUG := $(or $(LLVM_CONFIG_DEBUG),\
                                 $(wildcard $(EXTERNALS_CLASP_DIR)/build/debug/bin/llvm-config),\
                                 $(warning Could not find llvm-config (debug build) in externals-clasp.),\
                                 $(LLVM_CONFIG))
else
# XXX: confirm the necessity of llvm-config* pathsearch!
export LLVM_CONFIG ?= $(or $(call pathsearch, llvm-config),\
                           $(call pathsearch, llvm-config*),\
                           $(error Could not find llvm-config.))
endif

export GIT_COMMIT ?= $(shell git rev-parse --short HEAD || echo "unknown-commit")
export CLASP_VERSION ?= $(shell git describe --always || echo "unknown-version")

export LLVM_CONFIG_RELEASE ?= $(LLVM_CONFIG)
export LLVM_CONFIG_DEBUG ?= $(LLVM_CONFIG)
export LLVM_BIN_DIR ?= $(shell $(LLVM_CONFIG_RELEASE) --bindir)
# Not always the same as LLVM_BIN_DIR!

export LLVM_VERSION := $(shell $(LLVM_CONFIG) --version)
export LLVM_MAJOR_MINOR_VERSION := $(shell echo $(LLVM_VERSION) | sed 's/^\([0-9]*\)[.]\([0-9]*\)[.]\([0-9]*\)/\1.\2/')
#$(info llvm-version is $(LLVM_VERSION))
#$(info llvm-major-minor-version is $(LLVM_MAJOR_MINOR_VERSION))
export CLASP_CLANG_PATH := $(or $(CLASP_CLANG_PATH),\
			$(wildcard $(LLVM_BIN_DIR)/clang),\
			$(wildcard $(LLVM_BIN_DIR)/clang-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(call pathsearch, clang),\
			$(call pathsearch, clang-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(error Could not find clang - it needs to be installed an in your path.))
export CLASP_CLANGXX_PATH := $(or $(CLASP_CLANGXX_PATH),\
			$(wildcard $(LLVM_BIN_DIR)/clang++),\
			$(wildcard $(LLVM_BIN_DIR)/clang++-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(call pathsearch, clang++),\
			$(call pathsearch, clang++-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(error Could not find clang - it needs to be installed an in your path.))

export CLASP_INTERNAL_BUILD_TARGET_DIR ?= $(shell pwd)/build/clasp
export LIBATOMIC_OPS_SOURCE_DIR ?= $(CLASP_HOME)/src/boehm/libatomic_ops
export BOEHM_SOURCE_DIR ?= $(CLASP_HOME)/src/boehm/bdwgc
export BUILD ?= $(CLASP_HOME)/src/common/build
export CLASP_APP_EXECS ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/execs
export CLASP_APP_RESOURCES_DIR ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources
export CLASP_APP_RESOURCES_LIB_COMMON_DIR ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common

export BOOST_BUILD_SOURCE_DIR ?= $(CLASP_HOME)/tools/boost_build
export BOOST_BUILD_INSTALL ?= $(CLASP_APP_RESOURCES_DIR)/boost_build
export BJAM ?= $(BOOST_BUILD_INSTALL)/bin/bjam --ignore-site-config --user-config= -q

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
ifneq ($(EXTERNALS_CLASP_DIR),)
  export CLASP_DEBUG_CXXFLAGS += -I$(EXTERNALS_CLASP_DIR)/build/common/include
  export CLASP_DEBUG_LINKFLAGS += -L$(EXTERNALS_CLASP_DIR)/build/common/lib -lgmp -lgmpxx -lreadline -lexpat
  export CLASP_RELEASE_CXXFLAGS += -I$(EXTERNALS_CLASP_DIR)/build/common/include
  export CLASP_RELEASE_LINKFLAGS += -L$(EXTERNALS_CLASP_DIR)/build/common/lib -lgmp -lgmpxx -lreadline -lexpat
endif

ifeq ($(TARGET_OS),Darwin)
  export INCLUDE_DIRS += /usr/local/Cellar/gmp/6.0.0a/include
  export INCLUDE_DIRS += /opt/local/include
  export LIB_DIRS += /usr/local/Cellar/gmp/6.0.0a/lib
  export LIB_DIRS += /opt/local/lib
  export BOEHM_CC = gcc
  export BOEHM_CXX = g++
endif

ifeq ($(TARGET_OS),Linux)
  export BOEHM_CC = $(CLASP_CLANG_PATH)
  export BOEHM_CXX = $(CLASP_CLANGXX_PATH)
endif

include_flags := $(foreach dir,$(INCLUDE_DIRS),$(and $(wildcard $(dir)),-I$(dir)))
lib_flags := $(foreach dir,$(LIB_DIRS),$(and $(wildcard $(dir)),-L$(dir)))
export CLASP_DEBUG_CXXFLAGS += $(include_flags)
export CLASP_DEBUG_LINKFLAGS += $(lib_flags)
export CLASP_RELEASE_CXXFLAGS += $(include_flags)
export CLASP_RELEASE_LINKFLAGS += $(lib_flags)

export BINDIR ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/$(EXECUTABLE_DIR)
export EXECS ?= $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/execs/

export PATH := $(LLVM_BIN_DIR):$(PATH)
export PATH := $(CLASP_HOME)/src/common:$(PATH)
export PATH := $(BOOST_BUILD_INSTALL)/bin:$(PATH)
export PATH := $(BINDIR):$(PATH)

ifneq ($(CXXFLAGS),)
  export USE_CXXFLAGS := cxxflags=$(CXXFLAGS)
endif

ifeq ($(NO_COLOR),)
TPUT = $(call pathsearch,tput)
ifneq ($(TPUT),)
  COLOR_GREEN = $(shell $(TPUT) setaf 2 2>/dev/null)
  COLOR_RESET = $(shell $(TPUT) sgr0    2>/dev/null)
endif
endif
define varprint
	@echo -e "$(COLOR_GREEN)$(strip $(1))$(COLOR_RESET): $($(strip $(1)))"
endef

all:
	make submodules
	make build_all

build_all:
	make print-config
	make asdf
	make boost_build
	make boehm
	install -d build/clasp/Contents/Resources
	@if test ! -e build/clasp/Contents/Resources/clasp; then (cd build/clasp/Contents/Resources; ln -s ../../../../ clasp) ; fi
	(cd src/lisp; $(BJAM) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp gc=boehm bundle )
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/$(VARIANT) gc=boehm $(VARIANT) clasp_install )
	make -C src/main min-boehm
	make -C src/main bclasp-boehm-bitcode
	make -C src/main bclasp-boehm-fasl
	make -C src/main cclasp-from-bclasp-boehm-bitcode
#	make -C src/main cclasp-boehm-fasl
	make -C src/main cclasp-boehm-fasl
	make -C src/main cclasp-boehm-addons
	make executable-symlinks
	echo Clasp is now built

lisp-source:
	(cd src/lisp; $(BJAM) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp gc=boehm bundle )

fixup:
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/$(VARIANT) gc=boehm $(VARIANT) clasp_install )
	make -C src/main cclasp-boehm-addons
	make executable-symlinks
	echo Fixed-up Clasp is now built

ARCHIVE_NAME ?= clasp-$(CLASP_VERSION)-$(GIT_COMMIT).tar.bz2

archive:
	$(MAKE) submodules
	$(PYTHON2) src/common/git_archive_all.py $(ARCHIVE_NAME)

mps-build:
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install )
	(cd src/main; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/debug gc=mps debug clasp_install )
	make -C src/main link-min-mps

boot:
	make submodules
	make asdf
	make boost_build
	make boehm
	make -C src/main boehmdc-release-cxx
	make executable-symlinks
	make -C src/main min-boehmdc
	make -C src/main bclasp-boehmdc-bitcode
	make -C src/main bclasp-boehmdc-fasl
	make -C src/main bclasp-boehmdc-addons

boot-mps-interface:
	make boot
	make -C src/main mps-interface
#	make -C src/main bclasp-boehmdc
#	make -C src/main bclasp-boehmdc-addons

clasp-libraries:
	(cd src/gctools; $(BJAM) link=$(LINK) program=clasp gctools install-lib)

executable-symlinks:
	install -d $(BINDIR)
	ln -sf ../Contents/execs/boehm/release/bin/clasp $(BINDIR)/clasp_boehm_o
	ln -sf ../Contents/execs/boehmdc/release/bin/clasp $(BINDIR)/clasp_boehmdc_o
	ln -sf ../Contents/execs/mps/release/bin/clasp $(BINDIR)/clasp_mps_o
	ln -sf ../Contents/execs/boehm/debug/bin/clasp $(BINDIR)/clasp_boehm_d
	ln -sf ../Contents/execs/boehmdc/debug/bin/clasp $(BINDIR)/clasp_boehmdc_d
	ln -sf ../Contents/execs/mps/debug/bin/clasp $(BINDIR)/clasp_mps_d

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
                CC=$(BOEHM_CC) \
		CXX=$(BOEHM_CXX) \
                CFLAGS="-DUSE_MMAP -g" \
		PKG_CONFIG_PATH=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR)/lib/pkgconfig/ \
		./configure --enable-shared=yes --enable-static=yes --enable-handle-fork --enable-cplusplus --prefix=$(CLASP_APP_RESOURCES_LIB_COMMON_DIR) --with-libatomic-ops=yes;)

boehm-compile:
	(cd $(BOEHM_SOURCE_DIR); make -j$(PJOBS) | tee _boehm.log)
	(cd $(BOEHM_SOURCE_DIR); make -j$(PJOBS) install | tee _boehm_install.log)

export LIBATOMIC_OPS_CONFIGURE=src/boehm/libatomic_ops/configure
export BDWGC_CONFIGURE=src/boehm/bdwgc/configure
boehm:
	@if test ! -e $(LIBATOMIC_OPS_CONFIGURE); then make libatomic-setup ; fi
	@if test ! -e $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common/lib/libatomic_ops.a ; then make libatomic-compile ; fi
	@if test ! -e $(BDWGC_CONFIGURE); then make boehm-setup ; fi
	@if test ! -e $(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources/lib/common/lib/libgc.a ; then make boehm-compile ; fi


boehm-release-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/release gc=boehm release clasp-clbind-install)

boehm-release-clbind-a:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/release gc=boehm release clasp-clbind-install -a)

boehm-debug-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/debug gc=boehm debug clasp-clbind-install)

boehm-debug-clbind-a:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/debug gc=boehm debug clasp-clbind-install -a)

boehmdc-release-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/release gc=boehmdc release clasp-clbind-install)

mps-clbind:
	(cd src/clbind; $(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/$(VARIANT) gc=mps $(VARIANT) clasp-clbind-install)

boehm-clean:
	install -d $(BOEHM_SOURCE_DIR)
	-(cd $(BOEHM_SOURCE_DIR); make clean )
	if test -e $(LIBATOMIC_OPS_CONFIGURE); then rm $(LIBATOMIC_OPS_CONFIGURE) ; fi
	if test	-e $(BDWGC_CONFIGURE); then rm $(BDWGC_CONFIGURE) ; fi

pump:
	(cd src/core; make pump)
	(cd src/clbind; make pump)

submodules:
	$(MAKE) submodules-boehm
	$(MAKE) submodules-mps

submodules-boehm:
	-git submodule update --init tools/boost_build
	-git submodule update --init src/boehm/libatomic_ops
	-git submodule update --init src/boehm/bdwgc
	-git submodule update --init src/lisp/kernel/contrib/sicl
	-git submodule update --init src/lisp/modules/asdf
	-git submodule update --init tools/boost_build
#	-(cd src/lisp/modules/asdf; git checkout master; git pull origin master)

submodules-mps:
	-git submodule update --init src/mps

asdf:
	(cd src/lisp/modules/asdf; make)


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


boost_build:
	@if test ! -e $(BOOST_BUILD_INSTALL)/bin/bjam ; then make boost_build-compile ; fi

boost_build-compile:
	install -d $(BOOST_BUILD_INSTALL)
	(cd $(BOOST_BUILD_SOURCE_DIR); export BOOST_BUILD_PATH=`pwd`; ./bootstrap.sh; ./b2 toolset=clang install --prefix=$(BOOST_BUILD_INSTALL) --ignore-site-config)

clean:
	git submodule sync
	make boehm-clean
	(cd include/clasp/main/generated; rm *)
	(cd src/main; rm -rf bin bundle)
	(cd src/core; rm -rf bin bundle)
	(cd src/gctools; rm -rf bin bundle)
	(cd src/llvmo; rm -rf bin bundle)
	(cd src/asttooling; rm -rf bin bundle)
	(cd src/cffi; rm -rf bin bundle)
	(cd src/clbind; rm -rf bin bundle)
	(cd src/sockets; rm -rf bin bundle)
	(cd src/serveEvent; rm -rf bin bundle)
	rm -f $(ARCHIVE_NAME)
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
	$(call varprint, EXTERNALS_CLASP_DIR)
	$(call varprint, LLVM_CONFIG)
	$(call varprint, TARGET_OS)
	$(call varprint, ADDRESS-MODEL)
	$(call varprint, LINK)
	$(call varprint, PJOBS)
	$(call varprint, LLVM_CONFIG_DEBUG)
	$(call varprint, LLVM_CONFIG_RELEASE)
	$(call varprint, LLVM_BIN_DIR)
	$(call varprint, CLASP_CLANG_PATH)
	$(call varprint, CLASP_CLANGXX_PATH)
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

clang-format:
	git ls-files src/ include/ \
	| perl -ne 'chomp;print "$$_\n" if -f $$_ and (/\.[hc][hcp]?p?$$/) and !-l and !m#^include/.+/generated#;' \
	| xargs -P$(PJOBS) -n1 --verbose clang-format -i
