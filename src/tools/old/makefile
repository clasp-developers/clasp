
include	../../makefile.inc


export TOOLS_INSTALLDIR=$(INSTALLDIR)/tools
#export TOOLS_INSTALLDIR=$(CANDO_INSTALLDIR)



sa subAll:
	@echo Do nothing
#	-rm -rf $(TOOLS_INSTALLDIR)/*
#	-mkdir -p $(TOOLS_INSTALLDIR)
#	ln -f [abcdefghijklmnopqrstuvwxyz]* $(TOOLS_INSTALLDIR)
#	make osSpecific_subAll

subInstall:
	@echo Install cando for now do nothing


subClean:
	-rm -f *.pyc _* out/*


job:
	./createXplorFiles.py
	scp _* gandalf:Rdc/xplor


ifeq ($(CANDO_OS),linux)
osSpecific_subAll:
#	cp $(C_meister_OMMON_INSTALLDIR)/* $(TOOLS_INSTALLDIR)
	cp $(CANDO_HOME)/targets/linux/candoConfig.gml $(TOOLS_INSTALLDIR)


endif

ifeq ($(CANDO_OS),windows)

SRC=$(CANDO_HOME)/targets/windows/

osSpecific_subAll:
	# copy libraries
	(export DEST=$(TOOLS_INSTALLDIR); make copyBoostLibraries )
	(export DEST=$(TOOLS_INSTALLDIR); make copyExpatLibraries )
	(export DEST=$(TOOLS_INSTALLDIR); make copyWindowsLibraries )
#	cp $(C_meister_OMMON_INSTALLDIR)/* $(TOOLS_INSTALLDIR)
	# link source files
	ln -f ../common/*.py $(TOOLS_INSTALLDIR)
	# create an examples directory
	rm -rf $(TOOLS_INSTALLDIR)/Data
	mkdir $(TOOLS_INSTALLDIR)/Data
	ln -f Data/* $(TOOLS_INSTALLDIR)/Data

endif





#
#
#
#
# Directory specific stuff
#
#
#
#
#



lmtest:
	./cgExtract.py -f 10 -o out/_middle `cygpath -w /cygdrive/s/Meister/simulations/spinProbes/run3merSS/frames` _dynamicsOut 2>_dbg.xml


lall:
	make lmextract
	make llextract
	make ltextract

lmextract:
	./cgExtract.py -o out/_middle `cygpath -w /cygdrive/s/Meister/simulations/spinProbes/run3merSS/frames` _dynamicsOut 2>_dbg.xml

llextract:
	./cgExtract.py -o out/_lead `cygpath -w /cygdrive/s/Meister/simulations/spinProbes/runLeadPoac/frames` _dynamicsOut 2>_dbg.xml

ltextract:
	./cgExtract.py -o out/_tail `cygpath -w /cygdrive/s/Meister/simulations/spinProbes/runTrailPoac/frames` _dynamicsOut 2>_dbg.xml


cgAll:
	./cgBuilderFast.py -n 8 -o "_h%dmer.txt"
	./cgBuilderFast.py -n 8 --rotx0 +10 -o "_h%dmer_rotx0+10.txt"
	./cgBuilderFast.py -n 8 --rotx0 -10 -o "_h%dmer_rotx0-10.txt"
	./cgBuilderFast.py -n 8 --roty0 +10 -o "_h%dmer_roty0+10.txt"
	./cgBuilderFast.py -n 8 --roty0 -10 -o "_h%dmer_roty0-10.txt"
	./cgBuilderFast.py -n 8 --rotz0 +10 -o "_h%dmer_rotz0+10.txt"
	./cgBuilderFast.py -n 8 --rotz0 -10 -o "_h%dmer_rotz0-10.txt"
	./cgBuilderFast.py -n 8 --rotyy0 +10 -o "_h%dmer_rotyy0+10.txt"
	./cgBuilderFast.py -n 8 --rotyy0 -10 -o "_h%dmer_rotyy0-10.txt"
	./cgBuilderFast.py -n 8 --rotzz0 +10 -o "_h%dmer_rotzz0+10.txt"
	./cgBuilderFast.py -n 8 --rotzz0 -10 -o "_h%dmer_rotzz0-10.txt"
	./cgBuilderFast.py -n 8 --dist0 +0.3 -o "_h%dmer_dist0+3.txt"
	./cgBuilderFast.py -n 8 --dist0 -0.3 -o "_h%dmer_dist0-3.txt"

cgSeries:
	./cgBuilderFast.py -n 4 -o "_h%dmer.txt"
	./cgBuilderFast.py -n 5 -o "_h%dmer.txt"
	./cgBuilderFast.py -n 6 -o "_h%dmer.txt"
	./cgBuilderFast.py -n 7 -o "_h%dmer.txt"
	./cgBuilderFast.py -n 8 -o "_h%dmer.txt"
