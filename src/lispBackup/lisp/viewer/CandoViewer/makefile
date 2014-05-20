
include	$(CANDO_HOME)/makefile.inc

printenv:
	echo CANDO_APP_RESOURCES_LISP_DIR=$(CANDO_APP_RESOURCES_LISP_DIR)


subAll sa:
	make subBundle

subBundle sb:
	install -d $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/viewer/CandoViewer
	install -c *.l *.xrc $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/viewer/CandoViewer

subInstall:
	@echo Install buildDatabase cando-script code, for now do nothing


subClean:
	@echo DO nothing


