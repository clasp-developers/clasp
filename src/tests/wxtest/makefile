include	$(CANDO_HOME)/makefile.inc



########## ########## ########## ########## ########## 
#
# Windows
#
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),Windows_NT)



printenv:
	echo CANDOVIEW_APP=$(CANDOVIEW_APP)

sa subAll:
	make wxgl
	make subBundle

subClean sc:
	rm -f *.o wxgl.exe




CXX	= cl
LINK	= link.exe
CXXFLAGS= -DWIN32 -DEXPAT /EHsc /GR /MD


#.cc.o:
#	$(CXX) /D__WXMSW__ /DWIN32 $(CXXFLAGS) /c \
#		"/I$(WIN_WX_INCLUDES)" \
#		"/I../../mbb/cxx" \
#		"/I$(WIN_BOOST_INCLUDES)"\
#		"/I$(shell cygpath -w /cygdrive/c/wxWidgets-2.8.10/lib/vc_lib/msw)" \
#		"/I$(shell cygpath -w /cygdrive/c/Program Files/Microsoft Visual Studio .NET 2003/Vc7/PlatformSDK/Include)" \
#		-Fo$@ $< 


.cc.o:
	$(CXX) /c /nologo /TP \
		/Fo$@ \
		/MD \
		/DWIN32 \
		/DEXPAT \
		/EHsc \
		/D__WXMSW__ \
		/GR \
		"/I..\..\mbb\cxx" \
		"/I$(WIN_BOOST_INCLUDES)"\
		"/Ic:\wxWidgets-2.8.10\include" \
		"/Ic:\wxWidgets-2.8.10\lib\vc_lib\msw" \
		$< 



.cc.i:
	$(CXX) /P -D__WXMSW__ $(CXXFLAGS) /c \
		"/I$(WIN_BOOST_INCLUDES)" \
		"/I$(WIN_EXPAT_INCLUDES)" \
		"/I$(WIN_WX_INCLUDES)" \
		"/I$(shell cygpath -w /cygdrive/c/wxWidgets-2.8.10/lib/vc_lib/msw)" \
		"/I$(shell cygpath -w /cygdrive/c/Program Files/Microsoft Visual Studio .NET 2003/Vc7/PlatformSDK/Include)" \
		/Fo$@ $<


clhelp:
	$(CXX) /help


linkhelp:
	link /link

OBJECTS=aggregate.o alias.o anchor.o archive.o archiveNode.o archiveNodeWrite.o atom.o atomGrid.o atomIndexer.o bi_primatives.o bitVector.o bond.o boostPython.o boundFrame.o builder.o builderDatabase.o builderDatabaseDependent.o builderState.o candoScript.o chemInfo.o chimera.o command.o commandOligomer.o confSearchDatabase.o conformationCollection.o conformationExplorer.o cons.o constitution.o coordSys.o coordinateArray.o coordinateDatabase.o coupling.o deer.o energyAnchorRestraint.o energyAngle.o energyAtomTable.o energyChiralRestraint.o energyDihedral.o energyFunction.o energyImproperRestraint.o energyNonbond.o energyStretch.o exceptions.o exhaustiveSearch.o externalInterface.o extractFragment.o extractFrame.o extractScaffold.o ffAngleDb.o ffBaseDb.o ffItorDb.o ffNonbondDb.o ffPtorDb.o ffStretchDb.o ffTypesDb.o ffVdwDb.o forceField.o foundation.o fragment.o fragmentCoordinates.o fragmentHolder.o frame.o frameRecognizer.o frameTransform.o gaffParse.tab.o gamessInterface.o gaussianInterface.o geometryScorer.o hits.o holder.o intArray.o intrinsics.o jobArchive.o jobHistory.o largeSquareMatrix.o lex.lisp.o lightProfiler.o linearAlgebra.o lisp.o lispParse.tab.o loop.o macroModel.o main.o matrix.o matter.o mbbCoreTools.o minimizer.o minimizerLog.o model.o moe.o mol2.o molecule.o moleculeJob.o monomer.o monomerContext.o monomerCoordinates.o monomerGroup.o monomerPack.o monomerSet.o msmartsParse.tab.o mySaxInterface.o nVector.o nameSpace.o numericalFunction.o numerics.o object.o objectDictionary.o objectList.o objectSet.o oligomer.o oligomerBuilder.o plug.o profiler.o pythonCallback.o quickDom.o randomGenerators.o render.o residue.o restraint.o scaffold.o scaffoldHolder.o scorer.o search.o spanningLoop.o spline.o statusTracker.o stereochemistry.o stringList.o stringSet.o structureComparer.o structureList.o superposableConformationCollection.o superposeEngine.o superposeScorer.o symbol.o topology.o torsionDriver.o transformCascade.o twister.o typeAssignmentRules.o values.o vector2.o vector3.o vector4.o virtualSphere.o wxgl.o xmlLoadArchive.o xmlSaveArchive.o zMatrix.o
wxgl: wxgl.o
	link /OUT:wxgl.exe wxgl.o $(OBJECTS) \
		/SUBSYSTEM:windows \
		/verbose:lib \
		"/LIBPATH:$(shell cygpath -w $(CANDO_APP_BIN_DIR))" \
		wxmsw28_gl.lib\
		opengl32.lib\
		glu32.lib\
		wxbase28.lib\
		wxmsw28_core.lib\
		wxtiff.lib\
		wxjpeg.lib\
		wxpng.lib\
		wxzlib.lib\
		wxregex.lib\
		wxexpat.lib\
		kernel32.lib\
		user32.lib\
		gdi32.lib\
		comdlg32.lib\
		winspool.lib\
		winmm.lib\
		shell32.lib\
		comctl32.lib\
		ole32.lib\
		oleaut32.lib\
		uuid.lib\
		rpcrt4.lib\
		advapi32.lib\
		wsock32.lib\
		odbc32.lib


endif
