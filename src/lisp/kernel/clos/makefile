
include	$(CANDO_HOME)/makefile.inc

printenv:
	echo CANDO_APP_RESOURCES_LISP_DIR=$(CANDO_APP_RESOURCES_LISP_DIR)


subAll sa sad:
	make subBundle


subBundle sb:
	install -d $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/init/clos
	install -c *.lsp $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/init/clos


subInstall:
	@echo Install buildDatabase cando-script code, for now do nothing


subClean:
	@echo DO nothing


diff_ecl:
	./update_ecl diff

install_ecl:
	./update_ecl install -c
