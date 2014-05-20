# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OPAL_WRAPPER_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (opal-wrapper ${OPAL_WRAPPER_SOURCE_FILES})

ADD_DEPENDENCIES(opal-wrapper libopen-pal)
IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(opal-wrapper PROPERTIES
    COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS ${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

SET_TARGET_PROPERTIES(opal-wrapper PROPERTIES
  DEBUG_POSTFIX "${OMPI_EXE_DEBUG_POSTFIX}")

  TARGET_LINK_LIBRARIES (opal-wrapper libopen-pal Ws2_32.lib shlwapi.lib)

ADD_CUSTOM_COMMAND (TARGET opal-wrapper
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe
  ${PROJECT_BINARY_DIR}/mpicc${OMPI_EXE_DEBUG_POSTFIX}.exe
  COMMENT "Copying C compiler wrappers...")

IF(OMPI_WANT_CXX_BINDINGS)
  ADD_CUSTOM_COMMAND (TARGET opal-wrapper
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper${OMPI_EXE_DEBUG_POSTFIXX}.exe
    ${PROJECT_BINARY_DIR}/mpic++${OMPI_EXE_DEBUG_POSTFIX}.exe
    COMMAND ${CMAKE_COMMAND} -E copy
    ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe
    ${PROJECT_BINARY_DIR}/mpicxx${OMPI_EXE_DEBUG_POSTFIX}.exe
    COMMENT "Copying C++ compiler wrappers...")

  INSTALL(FILES ${PROJECT_BINARY_DIR}/mpic++${OMPI_EXE_DEBUG_POSTFIX}.exe
    ${PROJECT_BINARY_DIR}/mpicxx${OMPI_EXE_DEBUG_POSTFIX}.exe
    DESTINATION bin)
ENDIF(OMPI_WANT_CXX_BINDINGS)

IF(OMPI_WANT_F77_BINDINGS)
  ADD_CUSTOM_COMMAND(TARGET opal-wrapper
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe
  ${PROJECT_BINARY_DIR}/mpif77${OMPI_EXE_DEBUG_POSTFIX}.exe
  COMMENT "Copying F77 compiler wrappers...")

  INSTALL(FILES ${PROJECT_BINARY_DIR}/mpif77${OMPI_EXE_DEBUG_POSTFIX}.exe
    DESTINATION bin)
ENDIF(OMPI_WANT_F77_BINDINGS)

ADD_CUSTOM_COMMAND (TARGET opal-wrapper
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${PROJECT_SOURCE_DIR}/tools/wrappers/help-opal-wrapper.txt
  ${PROJECT_BINARY_DIR}/help-opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe.txt
  COMMENT "Copying wrapper help files...")

INSTALL(TARGETS opal-wrapper
  DESTINATION bin)
INSTALL(FILES help-opal-wrapper.txt
  ${PROJECT_BINARY_DIR}/help-opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe.txt
  DESTINATION share/openmpi)
INSTALL(FILES ${PROJECT_BINARY_DIR}/mpicc${OMPI_EXE_DEBUG_POSTFIX}.exe
  DESTINATION bin)

IF(WANT_INSTALL_HEADERS)

  ADD_CUSTOM_COMMAND (TARGET opal-wrapper
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe
    ${PROJECT_BINARY_DIR}/opalcc${OMPI_EXE_DEBUG_POSTFIX}.exe
    COMMENT "Copying renamed executables...")

  IF(OMPI_WANT_CXX_BINDINGS)
    ADD_CUSTOM_COMMAND (TARGET opal-wrapper
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy
      ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper${OMPI_EXE_DEBUG_POSTFIX}.exe
      ${PROJECT_BINARY_DIR}/opalc++${OMPI_EXE_DEBUG_POSTFIX}.exe
      COMMENT "Copying renamed executables...")

    CONFIGURE_FILE(${OPAL_SOURCE_DIR}/tools/wrappers/opalc++-wrapper-data.txt.in
                   ${OPAL_BINARY_DIR}/tools/wrappers/opalc++-wrapper-data.txt @ONLY)
    INSTALL(FILES ${OPAL_BINARY_DIR}/tools/wrappers/opalc++-wrapper-data.txt
            DESTINATION share/openmpi)
  ENDIF(OMPI_WANT_CXX_BINDINGS)

  CONFIGURE_FILE(${OPAL_SOURCE_DIR}/tools/wrappers/opalcc-wrapper-data.txt.in
                 ${OPAL_BINARY_DIR}/tools/wrappers/opalcc-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OPAL_BINARY_DIR}/tools/wrappers/opalcc-wrapper-data.txt
          DESTINATION share/openmpi)

ENDIF(WANT_INSTALL_HEADERS)
