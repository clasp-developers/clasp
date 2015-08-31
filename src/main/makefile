include $(CLASP_HOME)/makefile.inc


project-directories = ../gctools ../core ../clbind ../cffi ../asttooling ../llvmo ../serveEvent ../sockets


ifneq ($(CLASP_INTERNAL_BUILD_TARGET_DIR),)
  BCLASP_BOEHM = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/$(EXECUTABLE_DIR)/release/boehm/clasp -f bclasp
  CCLASP_BOEHM = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/$(EXECUTABLE_DIR)/release/boehm/clasp
  CLASP_MPS = $(CLASP_INTERNAL_BUILD_TARGET_DIR)/$(EXECUTABLE_DIR)/release/mps/clasp
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

application:
	cp -R $(CLASP_LISP_SOURCE_DIR)/build $(CLASP_APP_RESOURCES_DIR)/lisp/
	cp -R $(CLASP_INTERNAL_BUILD_TARGET_DIR)/* $(CLASP_APP_DIR)

boehm-cleavir:
	make boehm
	make cclasp

cclasp-boehm-with-addons:
	make cclasp-boehm
	make cclasp-boehm-addons

cclasp-boehm:
	$(BCLASP_BOEHM) -l "buildCleavirClasp.lsp"

cclasp-reload:
	$(BCLASP_BOEHM) -l "buildCleavirClaspReload.lsp"

bclasp-to-cclasp:
	$(BCLASP_BOEHM) -l "buildBclaspToCclasp.lsp"

cclasp-boehm-addons:
	$(CCLASP_BOEHM) -l "build_addons.lsp"

bclasp-boehm-addons:
	$(BCLASP_BOEHM) -l "build_addons.lsp"

#	make clasp-gc-interface



boehm:
	make min-boehm
	make full-boehm

boehm-recompile:
	make min-boehm-recompile
	make full-boehm

min-boehm:
	$(BCLASP_BOEHM) -I -f ecl-min -l "buildClaspMinBoot.lsp"

min-boehm-recompile:
	$(BCLASP_BOEHM) -f ecl-min -l "buildClaspMinRecompile.lsp"

full-boehm:
	$(BCLASP_BOEHM) -f ecl-min -l "buildClasp.lsp"


mps:
	make min-mps
	make full-mps

mps-recompile:
	make min-mps-recompile
	make full-mps

min-mps:
	$(CLASP_MPS) -I -f ecl-min -l "buildClaspMinMps.lsp"

min-mps-recompile:
	$(CLASP_MPS) -f ecl-min -l "buildClaspMinRecompile.lsp"

full-mps:
	$(CLASP_MPS) -f ecl-min -l "buildClasp.lsp"


short-compile-mps:
	time $(BCLASP_MPS) -f ecl-min -l "shortCompile.lsp"

short-compile-boehm:
	time $(BCLASP_BOEHM) -f ecl-min -l "shortCompile.lsp"



clasp-gc-interface:
	$(BCLASP_BOEHM) -l "buildClaspGC.lsp"

compile-commands:
	-bjam link=static gc=mps-prep program=clasp link=static -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/build-databases
	python ../common/buildJSON.py clang bjam.out `$(BCLASP_BOEHM) -N -e "(progn (format t \"~a\"(namestring (translate-logical-pathname (ensure-directories-exist #P\"APP-RESOURCES:build-databases;clasp_compile_commands.json\")))) (quit))"` # $(CLASP_APP_RESOURCES_DIR)/build-databases/clasp_compile_commands.json




scrape-all:
	(export PYTHONPATH=$(CLASP_HOME)/src/common:$(PYTHONPATH); $(PYTHON2) $(CLASP_HOME)/src/common/symbolGroveler.py include/generated/symbols_scraped_inc.h $(project-directories))
	(export PYTHONPATH=$(CLASP_HOME)/src/common:$(PYTHONPATH); $(PYTHON2) $(CLASP_HOME)/src/common/classGroveler.py include/generated/initClasses_inc.h $(project-directories) 2>registerClasses.log)



run-clang-check:
	clang-check -p . main.cc

run-clasp:
	clasp_d clang0.lsp


mps-interface:
	@echo Building mps-prep
	build -j$(PJOBS) link=static gc=mps-prep program=clasp install_clasp_mps-prep_o
	make boehm
	make compile-commands
	@echo Running parallel search and generating code when done
	$(CCLASP_BOEHM) -l "sys:mps-interface.lsp" -e "(parallel-search-all-then-generate-code-and-quit)"

export IDIR=$(CLASP_HOME)/build/clasp/Contents/Resources/lib/release
fix-intrinsics:
	cp $(IDIR)/intrinsics_bitcode_boehm.o $(IDIR)/intrinsics_bitcode_boehm.bc
	opt -strip-debug $(IDIR)/intrinsics_bitcode_boehm.bc -o=$(IDIR)/intrinsics_bitcode_boehm_stripped.bc
	cp $(IDIR)/intrinsics_bitcode_boehm_stripped.bc $(IDIR)/intrinsics_bitcode_boehm.o

