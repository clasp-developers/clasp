
include	$(CANDO_HOME)/makefile.inc


subAll sa:
	make subBundle

subBundle sb:
	install -d $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/gbw
	install -c *.l $(CANDO_APP_RESOURCES_LISP_DIR)/CANDO/gbw

subInstall:
	@echo Install buildDatabase cando-script code, for now do nothing


subClean:
	@echo DO nothing



fix1:
	grep -n '[[:space:]]_[A-Za-z]' *.csc >_a
	grep -v 'defClass' _a > _b
	grep -v '_DATABASE_' _b > _c
	grep -v 'method' _c > _d
