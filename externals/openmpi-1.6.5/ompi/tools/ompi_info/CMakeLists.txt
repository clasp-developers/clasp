# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OMPI_INFO_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")
ADD_EXECUTABLE (ompi_info ${OMPI_INFO_SOURCE_FILES})

ADD_DEPENDENCIES(ompi_info libmpi)
IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(ompi_info PROPERTIES
     COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS")
  SET_SOURCE_FILES_PROPERTIES(${PROJECT_SOURCE_DIR}/tools/ompi_info/components.c PROPERTIES COMPILE_FLAGS "${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

SET_TARGET_PROPERTIES(ompi_info PROPERTIES
  DEBUG_POSTFIX "${OMPI_EXE_DEBUG_POSTFIX}")

TARGET_LINK_LIBRARIES (ompi_info libmpi libopen-rte libopen-pal Ws2_32.lib shlwapi.lib)

INSTALL(TARGETS ompi_info
  DESTINATION bin)
INSTALL(FILES help-ompi_info.txt DESTINATION share/openmpi)
