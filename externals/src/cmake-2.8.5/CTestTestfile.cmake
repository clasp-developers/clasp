# CMake generated Testfile for 
# Source directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5
# Build directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
INCLUDE("/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/EnforceConfig.cmake")
ADD_TEST(SystemInformationNew "/Users/meister/Development/cando/externals/src/cmake-2.8.5/bin/cmake" "--system-information" "-G" "Unix Makefiles")
SUBDIRS(Source/kwsys)
SUBDIRS(Utilities/cmzlib)
SUBDIRS(Utilities/cmcurl)
SUBDIRS(Utilities/cmcompress)
SUBDIRS(Utilities/cmbzip2)
SUBDIRS(Utilities/cmlibarchive)
SUBDIRS(Utilities/cmexpat)
SUBDIRS(Source/CursesDialog/form)
SUBDIRS(Source)
SUBDIRS(Utilities)
SUBDIRS(Tests)
