# -*- mode: GNUmakefile; indent-tabs-mode: t -*-

include $(CLASP_HOME)/makefile.inc


project-directories = ../gctools ../core ../clbind ../cffi ../asttooling ../llvmo ../serveEvent ../sockets


ifneq ($(CLASP_INTERNAL_BUILD_TARGET_DIR),)
  CLASP_BOEHMDC = clasp-boehmdc-o
  BCLASP_BOEHMDC = $(CLASP_APP_EXECS)/boehmdc/release/bin/clasp -f bclasp
  ICLASP_BOEHM = iclasp-boehm-o
  ACLASP_BOEHM = aclasp-boehm-o
  BCLASP_BOEHM = bclasp-boehm-o
  CCLASP_BOEHMDC = $(CLASP_APP_EXECS)/boehmdc/release/bin/clasp
  CCLASP_BOEHM = cclasp-boehm-o
  ACLASP_MPS = $(CLASP_APP_EXECS)/mps/release/bin/clasp -I -f ecl-min
  BCLASP_MPS = $(CLASP_APP_EXECS)/mps/release/bin/clasp -f bclasp
  CCLASP_MPS = $(CLASP_APP_EXECS)/mps/release/bin/clasp
  CLASP_MPS = $(CLASP_APP_EXECS)/mps/release/bin/clasp
else
  echo "ERROR CLASP_INTERNAL_BUILD_TARGET_DIR is not defined - what should I do now?"
endif

export CLASP_APP_RESOURCES_DIR=$(CLASP_INTERNAL_BUILD_TARGET_DIR)/Contents/Resources


clasp:
	make clasp-release
	make clasp-debug

clasp-debug:
	build -j$(PJOBS) gc=boehm link=static program=clasp release debug

clasp-release:
	build -j$(PJOBS) gc=boehm link=static program=clasp release release

all:
	make boehm
#	make mps


link-cclasp-mps:
	$(CCLASP_MPS) -I -l "link-cclasp.lsp"

application:
	install -d $(CLASP_APP_DIR)
	cp -R $(CLASP_INTERNAL_BUILD_TARGET_DIR)/* $(CLASP_APP_DIR)

boehm-cleavir:
	make boehm
	make cclasp

cclasp-boehm-with-addons:
	make cclasp-boehm
	make cclasp-boehm-addons

clasp-reload:
	$(BCLASP_BOEHM) -l "buildCleavirClaspReload.lsp"

bclasp-to-cclasp:
	$(BCLASP_BOEHM) -l "buildBclaspToCclasp.lsp"

bclasp-boehmdc-addons:
	$(BCLASP_BOEHMDC) -l "build_addons.lsp"

link-bclasp-boehmdc-addons:
	$(BCLASP_BOEHMDC) -l "link_addons.lsp"

cclasp-boehmdc-addons:
	$(CCLASP_BOEHMDC) -l "build_addons.lsp"

link-cclasp-boehmdc-addons:
	$(CCLASP_BOEHMDC) -l "link_addons.lsp"

cclasp-boehm-addons:
	$(CCLASP_BOEHM) -l "build_addons.lsp"

cclasp-mps-addons:
	$(CCLASP_MPS) -l "build_addons.lsp"

link-cclasp-mps-addons:
	$(CCLASP_MPS) -l "link_addons.lsp"

#	make clasp-gc-interface


boehmdc-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/release gc=boehmdc release clasp_install

boehmdc-release-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/release gc=boehmdc release clasp_install -a

boehm-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/release gc=boehm release clasp_install

boehm-release-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/release gc=boehm release clasp_install -a

clasp-mps-o:
	make etags
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install
	(cd $(CLASP_HOME)/build/clasp/$(EXECUTABLE_DIR); if test -e ../Contents/execs/mps/release/bin/clasp ; then ln -sf ../Contents/execs/mps/release/bin/clasp clasp-mps-o ; fi)

clasp-mps-o-a:
	make etags
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install -a
	(cd $(CLASP_HOME)/build/clasp/$(EXECUTABLE_DIR); if test -e ../Contents/execs/mps/release/bin/clasp ; then ln -sf ../Contents/execs/mps/release/bin/clasp clasp-mps-o ; fi)

clasp-mps-d:
	make etags
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/debug gc=mps debug clasp_install
	(cd $(CLASP_HOME)/build/clasp/$(EXECUTABLE_DIR); if test -e ../Contents/execs/mps/debug/bin/clasp ; then ln -sf ../Contents/execs/mps/debug/bin/clasp clasp-mps-d ; fi)

clasp-mps-d-a:
	make etags
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/debug gc=mps debug clasp_install -a
	(cd $(CLASP_HOME)/build/clasp/$(EXECUTABLE_DIR); if test -e ../Contents/execs/mps/debug/bin/clasp ; then ln -sf ../Contents/execs/mps/debug/bin/clasp clasp-mps-d ; fi)


mps-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install

mps-prep-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/release gc=mps-prep release clasp_install

mps-release-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install -a

boehmdc-debug-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/debug gc=boehmdc debug clasp_install

boehmdc-debug-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/debug gc=boehmdc debug clasp_install -a

boehm-debug-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/debug gc=boehm debug clasp_install

boehm-debug-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/debug gc=boehm debug clasp_install -a

mps-debug-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/debug gc=mps debug clasp_install

mps-debug-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/debug gc=mps debug clasp_install -a


min-boehm-clean:
	$(BCLASP_BOEHM) -I -f ecl-min -e "(clean-system :init :no-prompt t)" -e "(quit)"

bclasp-boehm-clean:
	$(BCLASP_BOEHM) -I -f ecl-min -e "(bclasp-features)" -e "(clean-system :init :no-prompt t)" -e "(quit)"

cclasp-boehm-clean:
	$(BCLASP_BOEHM) -I -f ecl-min -e "(cclasp-features)" -e "(clean-system :init :no-prompt t)" -e "(quit)"

min-boehm:
	$(ICLASP_BOEHM) -I -f ecl-min -e "(compile-min)" -e "(link-min)" -e "(quit)"

min-mps:
	$(CLASP_MPS) -I -f ecl-min -e "(compile-min)" -e "(link-min)" -e "(quit)"

min-boehmdc:
	$(CLASP_BOEHMDC) -I -f ecl-min -e "(compile-min)" -e "(link-min)" -e "(quit)"

bclasp-boehmdc-bitcode:
	$(BCLASP_BOEHMDC) -f ecl-min -e "(compile-bclasp)" -e "(quit)"

bclasp-boehmdc-fasl:
	$(BCLASP_BOEHMDC) -f ecl-min -e "(link-bclasp)" -e "(quit)"

bclasp-boehmdc:
	make bclasp-boehmdc-bitcode
	make bclasp-boehmdc-fasl

cclasp-boehmdc-bitcode:
	$(BCLASP_BOEHMDC) -e "(compile-cclasp)" -e "(quit)"

cclasp-boehmdc-fasl:
	$(BCLASP_BOEHMDC) -e "(link-cclasp :force t)" -e "(quit)"

link-all-boehmdc-fasl:
	$(CLASP_BOEHMDC) -I -f ecl-min -e "(link-min)" -e "(quit)"
	$(CLASP_BOEHMDC) -f ecl-min -e "(link-bclasp)" -e "(quit)"
	$(CLASP_BOEHMDC) -f bclasp -e "(link-cclasp)" -e "(quit)"
	$(CLASP_BOEHMDC) -l "link_addons.lsp"

bclasp-boehm:
	make bclasp-boehm-bitcode
	make bclasp-boehm-fasl

bclasp-boehm-bitcode:
	$(BCLASP_BOEHM) -f ecl-min -e "(compile-bclasp)" -e "(quit)"

bclasp-mps-bitcode:
	$(CLASP_MPS) -f ecl-min -e "(compile-bclasp)" -e "(quit)"

bclasp-boehm-fasl:
	$(BCLASP_BOEHM) -f ecl-min -e "(link-bclasp)" -e "(quit)"


bclasp-boehm-addons:
	$(BCLASP_BOEHM) -l "build_addons.lsp"

bclasp-mps-addons:
	$(BCLASP_MPS) -l "build_addons.lsp"

bclasp-boehm-addons-fasl:
	$(BCLASP_BOEHM) -l "link-addons.lsp"

cclasp-from-bclasp-boehm-bitcode:
	$(BCLASP_BOEHM) -l "buildCleavirClasp.lsp"

cclasp-from-bclasp-mps-bitcode:
	$(BCLASP_MPS) -l "buildCleavirClasp.lsp"

cclasp-boehm:
	make cclasp-from-bclasp-boehm-bitcode
	make cclasp-boehm-fasl

cclasp-boehm-bitcode:
	$(BCLASP_BOEHM) -f ecl-min -e "(compile-cclasp)" -e "(quit)"

cclasp-boehm-fasl:
	$(BCLASP_BOEHM) -f ecl-min -e "(link-cclasp :force t)" -e "(quit)"

cclasp-mps-fasl:
	$(BCLASP_MPS) -e "(link-cclasp :force t)" -e "(quit)"

aclasp-mps-fasl:
	$(CLASP_MPS) -I -f ecl-min -e "(link-min)" -e "(quit)"

bclasp-mps-fasl:
	$(CLASP_MPS) -f ecl-min -e "(link-bclasp)" -e "(quit)"

clasp-gc-interface:
	$(BCLASP_BOEHM) -l "buildClaspGC.lsp"

debug-mps:
	export CLASP_TELEMETRY_MASK=1
	export CLASP_TELEMETRY_FILE=/tmp/clasp.tel
	export CLASP_MPS_DEBUG="32 32 16 80 32 80"
	bash


etags:
	-$(BJAM) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/$(VARIANT) gc=mps-prep release clasp_install -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/build-databases
	$(CLASP_SBCL) \
		--load $(CLASP_HOME)/src/common/build-json.lisp \
		--eval "(build-json:dump-source-files \"bjam.out\" \"etags-source-files.txt\")" \
		--eval "(quit)"
	etags `cat etags-source-files.txt`



scrape-all:
	(export PYTHONPATH=$(CLASP_HOME)/src/common:$(PYTHONPATH); $(PYTHON2) $(CLASP_HOME)/src/common/symbolGroveler.py include/generated/symbols_scraped_inc.h $(project-directories))
	(export PYTHONPATH=$(CLASP_HOME)/src/common:$(PYTHONPATH); $(PYTHON2) $(CLASP_HOME)/src/common/classGroveler.py include/generated/initClasses_inc.h $(project-directories) 2>registerClasses.log)



run-clang-check:
	clang-check -p . main.cc

run-clasp:
	clasp_d clang0.lsp

mps-interface-clasp:
	@echo Generating compile-commands
	make compile-commands
	@echo Running parallel search and generating code when done
	$(BCLASP_BOEHM) -l "sys:mps-interface.lsp" -e "(parallel-search-all-then-generate-code-and-quit)"

compile-commands:
	-$(BJAM) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/$(VARIANT) gc=mps-prep release clasp_install -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/build-databases
	$(CLASP_SBCL) \
		--load $(CLASP_HOME)/src/common/build-json.lisp \
		--eval "(build-json:build-db \"bjam.out\" \"$(CLASP_APP_RESOURCES_DIR)/build-databases/clasp_compile_commands.json\")" \
		--eval "(quit)"

mps-interface-run-boot:
	clasp_boehmdc_o -f bclasp  \
		-e "(require :clasp-analyzer)" \
		-e "(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database \"app-resources:build-databases;clasp_compile_commands.json\")))" \
		-e "(quit)"

mps-interface-run-boot-d:
	clasp_boehmdc_d -f bclasp  \
		-e "(require :clasp-analyzer)" \
		-e "(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database \"app-resources:build-databases;clasp_compile_commands.json\")))" \
		-e "(quit)"

mps-interface-run-cclasp:
	$(CCLASP_BOEHM)  \
		-e "(require :clasp-analyzer)" \
		-e "(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database \"app-resources:build-databases;clasp_compile_commands.json\")))" \
		-e "(quit)"

redeye-prepare:
	@echo Building mps-prep
	build -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/$(VARIANT) gc=mps-prep release clasp_install
	make compile-commands
#	make mps-interface-run

mps-interface-cclasp:
	@echo Building mps-prep
	build -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/$(VARIANT) gc=mps-prep release clasp_install
	make compile-commands
	make mps-interface-run-cclasp



mps-interface-boot:
	@echo Building mps-prep
	build -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/$(VARIANT) gc=mps-prep release clasp_install
	make compile-commands
	make mps-interface-run-boot
