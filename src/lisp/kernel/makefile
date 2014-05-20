


cl-exports:
	(export LISP_CLASP_DIR=`pwd`;clasp_o -i make-exports.lsp)



printenv:
	echo CLASP_APP_RESOURCES_LISP_DIR=$(CANDO_APP_RESOURCES_LISP_DIR)


subAll sa sad:
	make subBundle


subBundle sb:
	install -d $(CLASP_APP_RESOURCES_LISP_DIR)/CANDO/init
	install -C *.l *.lsp *.sh $(CLASP_APP_RESOURCES_LISP_DIR)/CANDO/init
	make -C lsp $*
	make -C clos $*
	make -C closette $*
	make -C cmp $*


subInstall:
	@echo Install buildDatabase cando-script code, for now do nothing


subClean:
	@echo DO nothing


clean-compile:
	-rm $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/init/*.bc
	-rm $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/init/compiler/*.bc

compile:
	time candoLisp -+compiler compile-boot.lsp

test-interp:
	candoLisp_d | tee >_interp.txt

test-compile:
	candoLisp_d -+compiler | tee >_compile.txt


tc:
	candoLisp_d tc.lsp
