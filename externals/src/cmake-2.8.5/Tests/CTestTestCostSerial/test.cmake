CMAKE_MINIMUM_REQUIRED(VERSION 2.1)

# Settings:
SET(CTEST_DASHBOARD_ROOT                "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestTest")
SET(CTEST_SITE                          "fry.home")
SET(CTEST_BUILD_NAME                    "CTestTest-Darwin-g++-CostSerial")

SET(CTEST_SOURCE_DIRECTORY              "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestTestCostSerial")
SET(CTEST_BINARY_DIRECTORY              "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestTestCostSerial")
SET(CTEST_CVS_COMMAND                   "/usr/bin/cvs")
SET(CTEST_CMAKE_GENERATOR               "Unix Makefiles")
SET(CTEST_BUILD_CONFIGURATION           "$ENV{CMAKE_CONFIG_TYPE}")
SET(CTEST_COVERAGE_COMMAND              "/usr/bin/gcov")
SET(CTEST_NOTES_FILES                   "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")

#CTEST_EMPTY_BINARY_DIRECTORY(${CTEST_BINARY_DIRECTORY})

# Remove old cost data file if it exists
IF(EXISTS "${CTEST_BINARY_DIRECTORY}/Testing/Temporary/CTestCostData.txt")
  FILE(REMOVE "${CTEST_BINARY_DIRECTORY}/Testing/Temporary/CTestCostData.txt")
ENDIF(EXISTS "${CTEST_BINARY_DIRECTORY}/Testing/Temporary/CTestCostData.txt")

CTEST_START(Experimental)
CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
# Run test set a second time to make sure they run in same specified order
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
