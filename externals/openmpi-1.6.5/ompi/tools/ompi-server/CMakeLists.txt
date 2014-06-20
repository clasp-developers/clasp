# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OMPI_SERVER_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (ompi-server ${OMPI_SERVER_SOURCE_FILES})

ADD_DEPENDENCIES(ompi-server libmpi)
IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(ompi-server PROPERTIES
    COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS ${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

SET_TARGET_PROPERTIES(ompi-server PROPERTIES
  DEBUG_POSTFIX "${OMPI_EXE_DEBUG_POSTFIX}")

TARGET_LINK_LIBRARIES (ompi-server libmpi libopen-pal libopen-rte Ws2_32.lib shlwapi.lib)

INSTALL(TARGETS ompi-server
  DESTINATION bin)
INSTALL(FILES help-ompi-server.txt DESTINATION share/openmpi)
