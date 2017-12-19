# -*- mode: GNUmakefile; indent-tabs-mode: t -*-
# Cleaned up by Shinmera October 13, 2015

all:
	./waf update_submodules
	./waf configure
	./waf build_cboehm

configure:
	make submodules
	make asdf
	./waf configure --prefix=$(PREFIX)

build_cboehm:
	./waf -j $(PJOBS) build_cboehm

build_cboehmdc:
	./waf -j $(PJOBS) build_cboehmdc


redeye-clean:
	./waf -j $(PJOBS) clean_impsprep

redeye-prep:
	./waf -j $(PJOBS) build_impsprep build_cboehmdc

redeye-run:
	(./build/boehmdc/iclasp-boehmdc -i ./build/boehmdc/cclasp-boehmdc-image.fasl -f ignore-extensions \
			-e "(require :clasp-analyzer)" \
			-e "(defparameter *compile-commands* \"`pwd`/build/mpsprep/compile_commands.json\")" \
			-e "(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database (pathname *compile-commands*))))" \
			-e "(quit)")

redeye:
	make redeye-prep
	make redeye-run
	./waf build_cboehm


pump:
	(cd src/core; make pump)
	(cd src/clbind; make pump)

submodules:
	$(MAKE) submodules-other
	$(MAKE) submodules-mps

submodules-other:
	-git submodule update --init src/lisp/kernel/contrib/sicl
	-git submodule update --init src/lisp/modules/asdf
	-git submodule update --init src/lisp/kernel/contrib/Acclimation
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
	@echo EXTERNALS_CLASP_DIR = $(EXTERNALS_CLASP_DIR)
	@echo LLVM_CONFIG = $(LLVM_CONFIG)
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
	(CLASP_LISP_SOURCE_DIR=$(DEV_CLASP_LISP_SOURCE_DIR) bash -l)


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


docker:
	time sudo docker-compose run clasp-build
	time sudo docker-compose build clasp
	time sudo docker-compose build cando-compile
	time sudo docker-compose run cando --help 

push-to-master:
	git fetch origin dev:testing
	git push origin testing
	git fetch origin testing:preview
	git push origin preview
	git fetch origin preview:master
	git push origin master

push-to-preview:
	git fetch origin dev:testing
	git push origin testing
	git fetch origin testing:preview
	git push origin preview

push-to-testing:
	git fetch origin dev:testing
	git push origin testing

analyze:
	./waf build_cboehmdc
	make analyze_rest

analyze_rest:
	./waf build_impsprep
	./waf analyze_clasp

push-cando-to-testing:
	git push origin dev
	git fetch origin dev:testing
	git push origin testing
	(cd extensions/cando; git push origin dev)
	(cd extensions/cando; git fetch origin dev:testing)
	(cd extensions/cando; git push origin testing)

cando-deploy:
	aws s3 cp s3://clasp-cando/docker-cando/cando-build.tgz cando-build.tgz
	aws s3 cp s3://clasp-cando/demos/demos.tar demos.tar
	docker-compose build cando-deploy
