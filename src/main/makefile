CLASP = clasp_mps_o
export CLASP_APP_RESOURCES_DIR=$(HOME)/local/clasp/Contents/Resources



all:
	make system-boot
	make clasp-gc



system-boot:
	make system-min-boot
	make system-full

system-recompile:
	make system-min-recompile
	make system-full

system-min-boot:
	$(CLASP) -I -f ecl-min -l "buildClaspMinBoot.lsp"

system-min-boot-reuse:
	$(CLASP) -I -f ecl-min -l "buildClaspMinBootReuse.lsp"

# Use the old min-image
system-min-recompile:
	$(CLASP) -f ecl-min -l "buildClaspMinRecompile.lsp"

system-full:
	$(CLASP) -f ecl-min -l "buildClasp.lsp"

clasp-gc:
	$(CLASP) -l "buildClaspGc.lsp"

compile-commands:
	-bjam bundle release mps-prep -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/build-databases
	python ../common/buildJSON.py clang bjam.out $(CLASP_APP_RESOURCES_DIR)/build-databases/clasp_compile_commands.json


run-clang-check:
	clang-check -p . main.cc

run-clasp:
	clasp_d clang0.lsp

