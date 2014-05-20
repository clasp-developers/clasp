# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE ORTE_PS_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (orte-ps ${ORTE_PS_SOURCE_FILES})

ADD_DEPENDENCIES(orte-ps libopen-rte libopen-pal)
IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(orte-ps PROPERTIES
    COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS ${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

SET_TARGET_PROPERTIES(orte-ps PROPERTIES
  DEBUG_POSTFIX "${OMPI_EXE_DEBUG_POSTFIX}")

TARGET_LINK_LIBRARIES (orte-ps libopen-pal libopen-rte Ws2_32.lib shlwapi.lib)

ADD_CUSTOM_COMMAND (TARGET orte-ps
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/orte-ps.exe
  ${PROJECT_BINARY_DIR}/ompi-ps.exe
  COMMENT "Copying renamed executables...")

INSTALL(TARGETS orte-ps
  DESTINATION bin)
INSTALL(FILES help-orte-ps.txt DESTINATION share/openmpi)
INSTALL(FILES ${PROJECT_BINARY_DIR}/ompi-ps.exe
  DESTINATION bin)
