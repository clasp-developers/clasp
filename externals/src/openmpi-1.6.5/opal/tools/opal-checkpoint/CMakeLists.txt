# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OPAL_CHECKPOINT_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (opal-checkpoint ${OPAL_CHECKPOINT_SOURCE_FILES})

ADD_DEPENDENCIES(opal-checkpoint libopen-pal)
IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(opal-checkpoint PROPERTIES 
    COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS ${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

SET_TARGET_PROPERTIES(opal-checkpoint PROPERTIES
  DEBUG_POSTFIX "${OMPI_EXE_DEBUG_POSTFIX}")

TARGET_LINK_LIBRARIES (opal-checkpoint libopen-pal Ws2_32.lib shlwapi.lib)

INSTALL(TARGETS opal-checkpoint
  DESTINATION bin)
INSTALL(FILES help-opal-checkpoint.txt DESTINATION share/openmpi)
