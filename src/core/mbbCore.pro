
CONFIG		= dll
DEFINES		= USEBOOSTPYTHON

########
##
## For debugging
## 
#win32-msvc:DEFINES	+= DEBUG_ON
win32-msvc:CONFIG	+= debug
unix:CONFIG		+= debug

########
##
## For release
##
#win32-msvc:CONFIG		+= release




#
# Windows specific defines
#

win32-msvc:INCLUDEPATH	= c:\local\boost-1.30.2
win32-msvc:LIBS		+= C:\local\boost-1.30.2\libs\python\build\bin-stage\boost_python.lib
win32-msvc:LIBS		+= C:\local\boost-1.30.2\libs\python\build\bin-stage\libboost_python.lib

#win32-msvc:INCLUDEPATH	= c:\local\boost-1.31
#win32-msvc:LIBS	+= C:\local\boost-1.31\libs\python\build\bin-stage\boost_python.lib
#win32-msvc:LIBS	+= C:\local\boost-1.31\libs\python\build\bin-stage\libboost_python.lib

#win32-msvc:INCLUDEPATH	+= c:\python22\include
#win32-msvc:LIBS	+= c:\python22\libs\python22.lib


win32-msvc:INCLUDEPATH	+= c:\python23\include
win32-msvc:LIBS	+= c:\python23\libs\python23.lib



win32-msvc:INCLUDEPATH	+= c:\Expat-1.95.6\Source\lib
win32-msvc:LIBS	+= c:\Expat-1.95.6\Libs\libexpat.lib
win32-msvc:TMAKE_CXXFLAGS	= /EHsc /GR
win32-msvc:DEFINES		+= WIN32 EXPAT

unix:INCLUDEPATH	= /usr/local/include/boost-1_31 /usr/local/expat-1.95.6/lib /usr/include/python2.3
unix:LIBS	= -shared -lboost_python-gcc -lexpat
unix:DEFINES		+= EXPAT
unix:TMAKE_CXXFLAGS_DEBUG = -gstabs
unix:TMAKE_CXXFLAGS_RELEASE = -O1




########################################
########################################
########################################

TEMPLATE	= lib
HEADERS		= StrX.h \
		  aggregate.h \
		  amber.h \
		  atom.h \
		  bisAminoAcid.h \
		  bond.h \
		  boostPython.h \
		  boostSmartPointers.h \
		  conformation.h \
		  container.h \
		  context.h \
		  coordSys.h \
		  cyclomerDatabase.h \
		  dkp.h \
		  exceptions.h \
		  execContext.h \
		  foundation.h \
		  loop.h \
		  macroModel.h \
		  matrix.h \
		  mbbCompTools.h \
		  mbbDatabaseSaxHandler.h \
		  minimizer.h \
		  moe.h \
		  molecule.h \
		  mySaxInterface.h \
		  nVector.h \
		  parmSet.h \
		  polyCyclomer.h \
		  pythonCallback.h \
		  quickDom.h \
		  residue.h \
		  restraint.h \
		  skeletonBuilder.h \
		  tnc.h \
		  vector3.h \
		  virtualSphere.h
SOURCES		= aggregate.cc \
		  amber.cc \
		  atom.cc \
		  bisAminoAcid.cc \
		  bond.cc \
		  bi_bisAminoAcid.cc \
		  bi_containers.cc \
		  bi_containers2.cc \
		  bi_conformation.cc \
		  bi_primatives.cc \
		  bi_quickDom.cc \
		  bi_moe.cc \
		  boostPython.cc \
		  conformation.cc \
		  container.cc \
		  context.cc \
		  coordSys.cc \
		  cyclomerDatabase.cc \
		  dkp.cc \
		  exceptions.cc \
		  execContext.cc \
		  foundation.cc \
		  loop.cc \
		  macroModel.cc \
		  matrix.cc \
		  mbbCompTools.cc \
		  mbbDatabaseSaxHandler.cc \
		  minimizer.cc \
		  moe.cc \
		  molecule.cc \
		  mySaxInterface.cc \
		  nVector.cc \
		  parmSet.cc \
		  polyCyclomer.cc \
		  pythonCallback.cc \
		  quickDom.cc \
		  residue.cc \
		  restraint.cc \
		  skeletonBuilder.cc \
		  tnc.cc \
		  vector3.cc \
		  virtualSphere.cc
#		  bi_communication.cc \
INTERFACES	= 
TARGET		= mbbCore




