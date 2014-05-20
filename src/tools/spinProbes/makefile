

#
# Minimum QSTEPS>=5.5 for reasonable convergence
QSTEPS = 5



all:
	make one

one:
	./oneOligomer --createNMer 4

rest:
	./oneOligomer --createNMer 5
	./oneOligomer --createNMer 6
	./oneOligomer --createNMer 7
	./oneOligomer --createNMer 8


qsub4:
	-rm out/_opt4*
	qsub opt4.pbs

qsub48:
	-rm out/_opt48*
	qsub opt48.pbs

qsub45678:
	qsub opt45678.pbs
#	-rm out/_opt45678*

qdummy:
	-rm out/_optdummy*
	qsub optdummy.pbs

extractAll:
	make lmextract
	make ltextract
	make llextract


sorayaFiles:
	cp parms/lead_entries* soraya/
	cp parms/tail_entries_back* soraya/
	cp parms/dkp-pro4SS+pro4SS.xml soraya/
	cp ../../mbbCore/coordSys.cc soraya/


export DIR=/home/other/meister/Development/cando/src/tools/spinProbes

debug:
	./mpiSpectrumFit --outputPrefix $(DIR)/out/_startingPoint\
	--singlePoint \
	--parmFile $(DIR)/parms/dkp-pro4SS+pro4SS.xml \
	--spectrum $(DIR)/parms/ss4mer.txt --column 2 --createNMer 4 \
	--spectrum $(DIR)/parms/ss5mer.txt --column 2 --createNMer 5 \
	--spectrum $(DIR)/parms/ss6mer.txt --column 2 --createNMer 6 \
	--spectrum $(DIR)/parms/ss7mer.txt --column 2 --createNMer 7 \
	--spectrum $(DIR)/parms/ss8mer.txt --column 2 --createNMer 8 \
	--logMonteCarloStep 3.5 | tee $(DIR)/out/_startingPoint.log
	./mpiSpectrumFit --outputPrefix $(DIR)/out/_endingPoint\
	--singlePoint \
	--parmFile $(DIR)/parms/dkp-pro4SS+pro4SS.xml \
	--spectrum $(DIR)/parms/ss4mer.txt --column 2 --createNMer 4 \
	--spectrum $(DIR)/parms/ss5mer.txt --column 2 --createNMer 5 \
	--spectrum $(DIR)/parms/ss6mer.txt --column 2 --createNMer 6 \
	--spectrum $(DIR)/parms/ss7mer.txt --column 2 --createNMer 7 \
	--spectrum $(DIR)/parms/ss8mer.txt --column 2 --createNMer 8 \
	--logMonteCarloStep 3.5 | tee $(DIR)/out/_endingPoint.log


testBuilder:
	./oneOligomer --createNMer 3
	./oneOligomer --createNMer 4
	./oneOligomer --createNMer 5
	./oneOligomer --createNMer 6
	./oneOligomer --createNMer 7
	./oneOligomer --createNMer 8


testSolution:
	./mpiSpectrumFit --outputPrefix solution/_seropt4\
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--randomStart \
		--maxFunctionEvaluations 5 \
		--logMonteCarloStep 3.5 | tee out/_seropt4.txt

#
# Limit the number of function evaluations to text xml output
#
so shortopt4:
	./mpiSpectrumFit --outputPrefix out/_seropt4\
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--logMonteCarloStep 5 | tee out/_shortopt4.txt


profile_opt4:
	-rm out/_seropt4*
	/home/other/meister/sandbox/bin/python ./mpiSpectrumFit \
		--outputPrefix out/_seropt4 \
		--maxFunctionEvaluations 5 \
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--logMonteCarloStep 5 | tee out/_seropt4.txt

opt4:
	./mpiSpectrumFit --outputPrefix out/_seropt4\
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--logMonteCarloStep 4.5 | tee out/_seropt4.txt


lopt4:
	-rm out/_seropt4*
	python ./mpiSpectrumFit --outputPrefix out/_seropt4\
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--logMonteCarloStep 5 | tee out/_seropt4.txt

lopt48:
	-rm out/_seropt48*
	python ./mpiSpectrumFit --outputPrefix out/_seropt48\
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--spectrum parms/ss8mer.txt --column 2 --createNMer 8 \
		--logMonteCarloStep 4 | tee out/_seropt48.txt

lopt468:
	-rm out/_seropt468*
	python ./mpiSpectrumFit --outputPrefix out/_seropt468\
		--spectrum parms/ss4mer.txt --column 2 --createNMer 4 \
		--spectrum parms/ss6mer.txt --column 2 --createNMer 6 \
		--spectrum parms/ss8mer.txt --column 2 --createNMer 8 \
		--logMonteCarloStep 4 | tee out/_seropt468.txt



ifeq ($(DEVELOPMENT_ENVIRONMENT),Windows_NT)
DYNAMICS=_dynamics/
else
DYNAMICS=/home/other/meister/work/parms/spinProbes
endif

lmextract:
	python cgExtract.py -o parms/middle $(DYNAMICS)/run3merSS/frames _dynamicsOut

llextract:
	python cgExtract.py -o parms/lead $(DYNAMICS)/runLeadPoac/frames _dynamicsOut

ltextract:
	python cgExtract.py -o parms/tail $(DYNAMICS)/runTrailPoac/frames _dynamicsOut






temp:
	./singleMc



clean:
	rm _*
