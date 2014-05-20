# CMake generated Testfile for 
# Source directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys
# Build directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(kwsys.testEncode "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsC" "testEncode")
ADD_TEST(kwsys.testTerminal "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsC" "testTerminal")
ADD_TEST(kwsys.testAutoPtr "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testAutoPtr")
ADD_TEST(kwsys.testHashSTL "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testHashSTL")
ADD_TEST(kwsys.testRegistry "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testRegistry")
ADD_TEST(kwsys.testIOS "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testIOS")
ADD_TEST(kwsys.testSystemTools "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testSystemTools")
ADD_TEST(kwsys.testCommandLineArguments "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testCommandLineArguments" "--another-bool-variable" "--long3=opt" "--set-bool-arg1" "-SSS" "ken" "brad" "bill" "andy" "--some-bool-variable=true" "--some-double-variable12.5" "--some-int-variable" "14" "--some-string-variable=test string with space" "--some-multi-argument" "5" "1" "8" "3" "7" "1" "3" "9" "7" "1" "-N" "12.5" "-SS=andy" "-N" "1.31" "-N" "22" "-SS=bill" "-BBtrue" "-SS=brad" "-BBtrue" "-BBfalse" "-SS=ken" "-A" "-C=test" "--long2" "hello")
ADD_TEST(kwsys.testCommandLineArguments1 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testCommandLineArguments1" "--ignored" "-n" "24" "--second-ignored" "-m=test value" "third-ignored" "-p" "some" "junk" "at" "the" "end")
ADD_TEST(kwsys.testSystemInformation "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testSystemInformation")
ADD_TEST(kwsys.testDynamicLoader "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestsCxx" "testDynamicLoader")
ADD_TEST(kwsys.testProcess-1 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "1")
SET_TESTS_PROPERTIES(kwsys.testProcess-1 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testProcess-2 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "2")
SET_TESTS_PROPERTIES(kwsys.testProcess-2 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testProcess-3 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "3")
SET_TESTS_PROPERTIES(kwsys.testProcess-3 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testProcess-4 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "4")
SET_TESTS_PROPERTIES(kwsys.testProcess-4 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testProcess-5 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "5")
SET_TESTS_PROPERTIES(kwsys.testProcess-5 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testProcess-6 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "6")
SET_TESTS_PROPERTIES(kwsys.testProcess-6 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testProcess-7 "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestProcess" "7")
SET_TESTS_PROPERTIES(kwsys.testProcess-7 PROPERTIES  TIMEOUT "120")
ADD_TEST(kwsys.testSharedForward "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Source/kwsys/cmsysTestSharedForward" "1")
