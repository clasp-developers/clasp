CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# CTestConfig.cmake settings:
set(CTEST_PROJECT_NAME "SmallAndFast")

# Intentionally leave out other upload-related CTestConfig.cmake settings
# so that the ctest_submit call below fails with an error message...
#
set(CTEST_DROP_METHOD "cp")

# Settings:
SET(CTEST_USE_LAUNCHERS 1)

# Emit these compiler warnings:
set(ENV{CXXFLAGS} "$ENV{CXXFLAGS} -Wall")

SET(CTEST_SITE                          "fry.home")
SET(CTEST_BUILD_NAME                    "CTestTestLaunchers-cp")

SET(CTEST_SOURCE_DIRECTORY              "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestTest/SmallAndFast")
SET(CTEST_BINARY_DIRECTORY              "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CTestTestFailedSubmits/cp")
SET(CTEST_CVS_COMMAND                   "/usr/bin/cvs")
SET(CTEST_CMAKE_GENERATOR               "Unix Makefiles")
SET(CTEST_BUILD_CONFIGURATION           "$ENV{CMAKE_CONFIG_TYPE}")
SET(CTEST_COVERAGE_COMMAND              "/usr/bin/gcov")
SET(CTEST_NOTES_FILES                   "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")

CTEST_EMPTY_BINARY_DIRECTORY(${CTEST_BINARY_DIRECTORY})

CTEST_START(Experimental)

# explicitly do not use CTEST_UPDATE - avoid network activity

CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}"
  OPTIONS "-DCTEST_USE_LAUNCHERS:BOOL=${CTEST_USE_LAUNCHERS};-DSAF_INTENTIONAL_COMPILE_ERROR:BOOL=ON;-DSAF_INTENTIONAL_COMPILE_WARNING:BOOL=ON"
  RETURN_VALUE res)
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_COVERAGE(BUILD "${CTEST_BINARY_DIRECTORY}" LABELS Everything RETURN_VALUE res)

# ok to call ctest_submit - still avoids network activity because there is
# not a valid drop location given above...
CTEST_SUBMIT(RETURN_VALUE res)

# Add coverage for the new APPEND arg to ctest_start:
#
CTEST_START(Experimental APPEND)
