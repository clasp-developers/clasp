include $(CLASP_HOME)/makefile.inc


project-directories = ../gctools ../core ../clbind ../cffi ../asttooling ../llvmo ../serveEvent ../sockets


ifneq ($(CLASP_INTERNAL_BUILD_TARGET_DIR),)
  BCLASP_BOEHMDC = $(CLASP_APP_EXECS)/boehmdc/release/bin/clasp -f bclasp
  BCLASP_BOEHM = $(CLASP_APP_EXECS)/boehm/release/bin/clasp -f bclasp
  CCLASP_BOEHM = $(CLASP_APP_EXECS)/boehm/release/bin/clasp
  CCLASP_MPS = $(CLASP_APP_EXECS)/mps/release/bin/clasp
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
	cp -R $(CLASP_LISP_SOURCE_DIR)/build $(CLASP_APP_RESOURCES_DIR)/lisp/
	cp -R $(CLASP_INTERNAL_BUILD_TARGET_DIR)/* $(CLASP_APP_DIR)

boehm-cleavir:
	make boehm
	make cclasp

cclasp-boehm-with-addons:
	make cclasp-boehm
	make cclasp-boehm-addons

cclasp-boehm-bitcode:
	$(BCLASP_BOEHM) -l "buildCleavirClasp.lsp"

cclasp-boehm-fasl:
	$(BCLASP_BOEHM) -I -f ecl-min -l "link-cclasp.lsp"

cclasp-reload:
	$(BCLASP_BOEHM) -l "buildCleavirClaspReload.lsp"

bclasp-to-cclasp:
	$(BCLASP_BOEHM) -l "buildBclaspToCclasp.lsp"

bclasp-boehmdc-addons:
	$(BCLASP_BOEHMDC) -l "build_addons.lsp"

link-bclasp-boehmdc-addons:
	$(BCLASP_BOEHMDC) -l "link_addons.lsp"

bclasp-boehm-addons:
	$(BCLASP_BOEHM) -l "build_addons.lsp"

cclasp-boehm-addons:
	$(CCLASP_BOEHM) -l "build_addons.lsp"

link-cclasp-mps-addons:
	$(CCLASP_MPS) -l "link_addons.lsp"

#	make clasp-gc-interface


boehmdc-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/release gc=boehmdc release clasp_install

boehmdc-release-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/release gc=boehmdc release clasp_install -a

boehm-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/release gc=boehm release clasp_install

mps-release-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install
mps-release-cxx-a:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/release gc=mps release clasp_install -a

boehmdc-debug-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehmdc/debug gc=boehmdc debug clasp_install

boehm-debug-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/boehm/debug gc=boehm debug clasp_install

mps-debug-cxx:
	$(BUILD) -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps/debug gc=mps debug clasp_install

min-boehmdc:
	$(BCLASP_BOEHMDC) -I -f ecl-min -l "buildClaspMinBoot.lsp"

min-boehmdc-recompile:
	$(BCLASP_BOEHMDC) -f ecl-min -l "buildClaspMinRecompile.lsp"

bclasp-boehmdc:
	$(BCLASP_BOEHMDC) -f ecl-min -l "buildClasp.lsp"

min-boehm:
	$(BCLASP_BOEHM) -I -f ecl-min -l "buildClaspMinBoot.lsp"

min-boehm-recompile:
	$(BCLASP_BOEHM) -f ecl-min -l "buildClaspMinRecompile.lsp"

bclasp-boehm:
	$(BCLASP_BOEHM) -f ecl-min -l "buildClasp.lsp"



mps-recompile:
	make min-mps-recompile
	make bclasp-mps


clasp-gc-interface:
	$(BCLASP_BOEHM) -l "buildClaspGC.lsp"

debug-mps:
	export CLASP_TELEMETRY_MASK=1
	export CLASP_TELEMETRY_FILE=/tmp/clasp.tel
	export CLASP_MPS_DEBUG="32 32 16 80 32 80"
	bash




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
	-bjam link=static gc=mps-prep program=clasp link=static -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/build-databases
	python ../common/buildJSON.py clang bjam.out `$(BCLASP_BOEHMDC) -N -e "(progn (format t \"~a\"(namestring (translate-logical-pathname (ensure-directories-exist #P\"APP-RESOURCES:build-databases;clasp_compile_commands.json\")))) (quit))"` # $(CLASP_APP_RESOURCES_DIR)/build-databases/clasp_compile_commands.json

mps-interface-run:
	$(BCLASP_BOEHMDC)  \
		-e "(require :asdf)" \
	        -e "(asdf:load-system \"clasp-analyzer\")" \
		-e "(clasp-analyzer:load-compilation-database \"app-resources:build-databases;clasp_compile_commands.json\")" \
	        -e "(clasp-analyzer:serial-search-all-then-generate-code-and-quit (current-dir))"

mps-interface:
	@echo Building mps-prep
	build -j$(PJOBS) toolset=$(TOOLSET) link=$(LINK) program=clasp --prefix=$(CLASP_APP_EXECS)/mps-prep/$(VARIANT) gc=mps-prep release clasp_install
	make compile-commands
	make mps-interface-run
