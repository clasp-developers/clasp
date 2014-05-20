# CMake generated Testfile for 
# Source directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5/Utilities
# Build directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5/Utilities
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(CMake.HTML "/Users/meister/Development/cando/externals/src/cmake-2.8.5/bin/cmake" "-E" "chdir" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Utilities/xml" "/opt/local/bin/xmllint" "--valid" "--noout" "--nonet" "--path" "." "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-policies.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-properties.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-variables.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-modules.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-commands.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-compatcommands.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ctest.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cpack.html" "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ccmake.html")
SUBDIRS(Doxygen)
SUBDIRS(KWStyle)
