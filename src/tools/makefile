
include	$(CANDO_HOME)/makefile.inc

SCRIPTS=bdb checkSeed toXml viewAggs mr viewDatabase extractObjectNodes xmlRun dlView dbg \
	 advanceTrainers mpiAdvanceTrainers viewMinimizerDebug updateCsc fixLog logView \
	cv cvd candoFormatView gdbcvd cleanupCompile dbinfo dbinfo_d gdb_dbinfo_d



#
# Targets are subAll, subInstall, subClean
#
sa subAll:
	make subBundle

subBundle sb:
	install -d $(CANDO_APP_RESOURCES_BIN_DIR)
	install -c $(SCRIPTS) $(CANDO_APP_RESOURCES_BIN_DIR)

subMeta sm:
	echo Nothing

subClean:
	echo Nothing

subClassTree sct:
	@echo do nothing

subParsers:
	@echo No parsers do nothing
