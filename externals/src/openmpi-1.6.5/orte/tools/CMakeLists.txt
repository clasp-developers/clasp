# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
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

ADD_SUBDIRECTORY(orted)
ADD_SUBDIRECTORY(orterun)
ADD_SUBDIRECTORY(orte-clean)
ADD_SUBDIRECTORY(orte-ps)
IF(OPAL_ENABLE_FT)
  ADD_SUBDIRECTORY(orte-checkpoint)
ENDIF(OPAL_ENABLE_FT)

IF(WANT_INSTALL_HEADERS)

  CONFIGURE_FILE(${ORTE_SOURCE_DIR}/tools/wrappers/ortec++-wrapper-data.txt.in
                 ${ORTE_BINARY_DIR}/tools/wrappers/ortec++-wrapper-data.txt @ONLY)
  CONFIGURE_FILE(${ORTE_SOURCE_DIR}/tools/wrappers/ortecc-wrapper-data.txt.in
                 ${ORTE_BINARY_DIR}/tools/wrappers/ortecc-wrapper-data.txt @ONLY)

  INSTALL(FILES ${ORTE_BINARY_DIR}/tools/wrappers/ortec++-wrapper-data.txt
                DESTINATION share/openmpi)
  INSTALL(FILES ${ORTE_BINARY_DIR}/tools/wrappers/ortecc-wrapper-data.txt
                DESTINATION share/openmpi)

ENDIF(WANT_INSTALL_HEADERS)