set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
set(CTEST_PROJECT_NAME "CTestConfig")
set(CTEST_SOURCE_DIRECTORY "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestConfig")
set(CTEST_BINARY_DIRECTORY "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestConfig/Release-script")

ctest_start(Experimental)

ctest_configure(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE rv)
if(NOT rv STREQUAL 0)
  message(FATAL_ERROR "*** error in ctest_configure ***")
endif()

ctest_build(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE rv)
if(NOT rv STREQUAL 0)
  message(FATAL_ERROR "*** error in ctest_build ***")
endif()

ctest_test(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE rv)
if(NOT rv STREQUAL 0)
  message(FATAL_ERROR "*** error in ctest_test ***")
endif()
