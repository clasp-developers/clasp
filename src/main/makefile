CLASP_BOEHM = clasp_boehm_o
CLASP_MPS = clasp_mps_o
export CLASP_APP_RESOURCES_DIR=$(HOME)/local/clasp/Contents/Resources



all:
	make boehm
	make clasp-gc



boehm:
	make min-boehm
	make full-boehm

boehm-recompile:
	make min-boehm-recompile
	make full-boehm

min-boehm:
	$(CLASP_BOEHM) -I -f ecl-min -l "buildClaspMinBoot.lsp"

min-boehm-recompile:
	$(CLASP_BOEHM) -f ecl-min -l "buildClaspMinRecompile.lsp"

full-boehm:
	$(CLASP_BOEHM) -f ecl-min -l "buildClasp.lsp"


mps:
	make min-mps
	make full-mps

mps-recompile:
	make min-mps-recompile
	make full-mps

min-mps:
	$(CLASP_MPS) -I -f ecl-min -l "buildClaspMinBoot.lsp"

min-mps-recompile:
	$(CLASP_MPS) -f ecl-min -l "buildClaspMinRecompile.lsp"

full-mps:
	$(CLASP_MPS) -f ecl-min -l "buildClasp.lsp"


short-compile-mps:
	time $(CLASP_MPS) -f ecl-min -l "shortCompile.lsp"

short-compile-boehm:
	time $(CLASP_BOEHM) -f ecl-min -l "shortCompile.lsp"



clasp-gc:
	$(CLASP_BOEHM) -l "buildClaspGc.lsp"

compile-commands:
	-bjam bundle release mps-prep -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/build-databases
	python ../common/buildJSON.py clang bjam.out $(CLASP_APP_RESOURCES_DIR)/build-databases/clasp_compile_commands.json


run-clang-check:
	clang-check -p . main.cc

run-clasp:
	clasp_d clang0.lsp

