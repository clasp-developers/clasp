# Choose a configuration with which to drive CTest tests.
IF(CTEST_CONFIGURATION_TYPE)
  SET(CTestTest_CONFIG "${CTEST_CONFIGURATION_TYPE}")
ELSE(CTEST_CONFIGURATION_TYPE)
  SET(CTestTest_CONFIG "Debug")
ENDIF(CTEST_CONFIGURATION_TYPE)

# Choose a configuration that was built if none is given.
IF(NOT CTEST_CONFIGURATION_TYPE)
  SET(CTEST_CMD "/Users/meister/Development/cando/externals/src/cmake-2.8.5/bin/ctest")
  GET_FILENAME_COMPONENT(CTEST_DIR "${CTEST_CMD}" PATH)
  GET_FILENAME_COMPONENT(CTEST_EXE "${CTEST_CMD}" NAME)
  FOREACH(cfg Release Debug MinSizeRel RelWithDebInfo)
    IF(NOT CTEST_CONFIGURATION_TYPE)
      IF(EXISTS "${CTEST_DIR}/${cfg}/${CTEST_EXE}")
        SET(CTEST_CONFIGURATION_TYPE ${cfg})
      ENDIF(EXISTS "${CTEST_DIR}/${cfg}/${CTEST_EXE}")
    ENDIF(NOT CTEST_CONFIGURATION_TYPE)
  ENDFOREACH(cfg)
  IF(NOT CTEST_CONFIGURATION_TYPE)
    SET(CTEST_CONFIGURATION_TYPE NoConfig)
  ENDIF(NOT CTEST_CONFIGURATION_TYPE)
  MESSAGE("Guessing configuration ${CTEST_CONFIGURATION_TYPE}")
ENDIF(NOT CTEST_CONFIGURATION_TYPE)

# Fake a user home directory to avoid polluting the real one.
# But provide original ENV{HOME} value in ENV{CTEST_REAL_HOME} for tests that
# need access to the real HOME directory.
SET(ENV{CTEST_REAL_HOME} "$ENV{HOME}")
SET(ENV{HOME} "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CMakeFiles/TestHome")

