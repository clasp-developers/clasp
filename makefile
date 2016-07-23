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
			$(error Could not find sbcl - it needs to be installed and in your path.))

export PYTHON2 := $(or $(PYTHON2),\
                       $(call pathsearch, python2.7),\
                       $(call pathsearch, python2),\
                       $(call pathsearch, python),\
                       $(warning Could not find python.))

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
export LLVM_CONFIG ?= $(or $(call pathsearch, llvm-config-3.8),\
                           $(call pathsearch, llvm-config),\
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
export LLVM_VERSION_X100 := $(shell echo $(LLVM_VERSION) | sed 's/[.]//g' )
#$(info llvm-version is $(LLVM_VERSION))
#$(info llvm-major-minor-version is $(LLVM_MAJOR_MINOR_VERSION))
export CLASP_CLANG_PATH := $(or $(CLASP_CLANG_PATH),\
			$(wildcard $(LLVM_BIN_DIR)/clang-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(wildcard $(LLVM_BIN_DIR)/clang),\
			$(call pathsearch, clang-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(call pathsearch, clang),\
			$(error Could not find clang - it needs to be installed and in your path.))
export CLASP_CLANGXX_PATH := $(or $(CLASP_CLANGXX_PATH),\
			$(wildcard $(LLVM_BIN_DIR)/clang++-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(wildcard $(LLVM_BIN_DIR)/clang++),\
			$(call pathsearch, clang++-$(LLVM_MAJOR_MINOR_VERSION)),\
			$(call pathsearch, clang++),\
			$(error Could not find clang - it needs to be installed and in your path.))

export BUILD ?= $(CLASP_HOME)/src/common/build

export CLASP_DEBUG_LLVM_LIB_DIR ?= $(shell $(LLVM_CONFIG_DEBUG) --libdir | tr -d '\n')
export CLASP_RELEASE_LLVM_LIB_DIR ?= $(shell $(LLVM_CONFIG_RELEASE) --libdir | tr -d '\n')

export CLASP_LIB_EXTENSION ?= so
export CLASP_LIB_EXTENSION := $(or $(and $(filter $(TARGET_OS), Linux ), so), \
		                    $(and $(filter $(TARGET_OS), Darwin ), dylib), \
		                    $(error Invalid TARGET_OS: $(TARGET_OS)))

export CLASP_DEBUG_CXXFLAGS += -I$(shell $(LLVM_CONFIG_DEBUG) --includedir)
#export CLASP_DEBUG_LINKFLAGS += -L$(CLASP_DEBUG_LLVM_LIB_DIR)
#export CLASP_DEBUG_LINKFLAGS += $(shell $(LLVM_CONFIG_DEBUG) --libs)
#export CLASP_DEBUG_LINKFLAGS += $(shell $(LLVM_CONFIG_DEBUG) --system-libs)
export CLASP_RELEASE_CXXFLAGS += -I$(shell $(LLVM_CONFIG_RELEASE) --includedir)
#export CLASP_RELEASE_LINKFLAGS += -L$(CLASP_RELEASE_LLVM_LIB_DIR)
#export CLASP_RELEASE_LINKFLAGS += $(shell $(LLVM_CONFIG_RELEASE) --libs)
#export CLASP_RELEASE_LINKFLAGS += $(shell $(LLVM_CONFIG_RELEASE) --system-libs)
ifneq ($(EXTERNALS_CLASP_DIR),)
  export CLASP_DEBUG_CXXFLAGS += -I$(EXTERNALS_CLASP_DIR)/build/common/include
#  export CLASP_DEBUG_LINKFLAGS += -L$(EXTERNALS_CLASP_DIR)/build/common/lib -lgmp -lgmpxx -lreadline -lexpat
  export CLASP_RELEASE_CXXFLAGS += -I$(EXTERNALS_CLASP_DIR)/build/common/include
#  export CLASP_RELEASE_LINKFLAGS += -L$(EXTERNALS_CLASP_DIR)/build/common/lib -lgmp -lgmpxx -lreadline -lexpat
endif

ifeq ($(TARGET_OS),Darwin)
  export INCLUDE_DIRS += /usr/local/Cellar/gmp/6.0.0a/include
  export INCLUDE_DIRS += /opt/local/include
  export LIB_DIRS += /usr/local/Cellar/gmp/6.0.0a/lib
  export LIB_DIRS += /opt/local/lib
endif

include_flags := $(foreach dir,$(INCLUDE_DIRS),$(and $(wildcard $(dir)),-I$(dir)))
lib_flags := $(foreach dir,$(LIB_DIRS),$(and $(wildcard $(dir)),-L$(dir)))
export CLASP_DEBUG_CXXFLAGS += $(include_flags)
#export CLASP_DEBUG_LINKFLAGS += $(lib_flags)
export CLASP_RELEASE_CXXFLAGS += $(include_flags)
#export CLASP_RELEASE_LINKFLAGS += $(lib_flags)

export PATH := $(LLVM_BIN_DIR):$(PATH)
export PATH := $(CLASP_HOME)/src/common:$(PATH)
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
	make configure
	make build

configure:
	make submodules
	make asdf
	./waf configure --prefix=$(PREFIX)

build:
	make boehmdc-o

boehmdc-o:
	./waf -j $(PJOBS) build_cboehmdc_o

install:
	./waf -j $(PJOBS) install_cboehmdc_o

#	make -C src/main bclasp-boehmdc-addons


redeye-prep:
	./waf -j $(PJOBS) build_cboehmdc_o
	./waf -j $(PJOBS) build_impsprep_o

redeye-run:
	wbuild/boehmdc_o/cclasp-boehmdc-o \
		-e "(require :clasp-analyzer)" \
		-e "(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database \"lib:compile_commands.json\")))" \
		-e "(quit)"



pump:
	(cd src/core; make pump)
	(cd src/clbind; make pump)

submodules:
	$(MAKE) submodules-other
	$(MAKE) submodules-mps

submodules-other:
	-git submodule update --init src/lisp/kernel/contrib/sicl
	-git submodule update --init src/lisp/modules/asdf
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
export CLASP_LISP_SOURCE_DIR ?= $(DEV_CLASP_LISP_SOURCE_DIR)

devemacs:
	@echo This shell sets up environment variables
	@echo as they are defined when commands execute within the makefile
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) $(DEVEMACS))

devemacs_no_clasp_lisp_source_dir:
	@echo This shell sets up environment variables
	@echo as they are defined when commands execute within the makefile
	$(DEVEMACS)

devshell:
	@echo This shell sets up environment variables
	@echo as they are defined when commands execute within the makefile
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) bash)


devshell-telemetry:
	@echo This shell sets up environment variables
	@echo as they are defined when commands execute within the makefile
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR); export CLASP_MPS_CONFIG="32 32 16 80 32 80"; export CLASP_TELEMETRY_FILE=/tmp/clasp.tel; export CLASP_TELEMETRY_MASK=3; bash)


cloc-files:
	find src/ -name '*.cc' -print >/tmp/files.cloc
	find include/ -name '*.h' -print >>/tmp/files.cloc

clean:
	git submodule sync
	./waf distclean

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
	$(call varprint, LIBATOMIC_OPS_SOURCE_DIR)
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
