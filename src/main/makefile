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
	clasp -I -f ecl-min -l "buildClaspMinBoot.lsp"

# Use the old min-image
system-min-recompile:
	clasp -f ecl-min -l "buildClaspMinRecompile.lsp"

system-full:
	clasp -f ecl-min -l "buildClasp.lsp"

clasp-gc:
	clasp -l "buildClaspGc.lsp"

compile-commands:
	-bjam bundle release mps-prep -n -a >bjam.out
	-install -d $(CLASP_APP_RESOURCES_DIR)/buildDatabases
	python ../common/buildJSON.py clang bjam.out $(CLASP_APP_RESOURCES_DIR)/buildDatabases/clasp_compile_commands.json


run-clang-check:
	clang-check -p . main.cc

run-clasp:
	clasp_d clang0.lsp

