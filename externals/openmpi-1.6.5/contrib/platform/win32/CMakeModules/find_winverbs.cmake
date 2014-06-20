# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Find OFED SDK Winverbs headers and libraries.

#
#  WINVERBS_PATH           - path to OFED SDK dir
#  WINVERBS_INCLUDE_PATH   - path to verbs.h
#  WINVERBS_FOUND          - system has OFED SDK
#

# This module is included in a .windows file, it must return two variables:
# RESULT                : find result
# RESULT_INCLUDE_PATH   : the path need to include if RESULT is true
# RESULT_LINK_LIBRARIES : libraries need to be linked

IF(NOT OMPI_WANT_WINVERBS)
  SET(RESULT FALSE)
ELSE(NOT OMPI_WANT_WINVERBS)

  SET(WINVERBS_PATH "" CACHE PATH
    "Path to OFED SDK WINVERBS root directory, if not in system path, has to be set manually by user.")

  #generate the lex file, if it's not there
  IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/mca/btl/wv/btl_wv_lex.c)
    ADD_FLEX_FILE(OPAL_SOURCE_FILES ./mca/btl/wv/btl_wv_lex.l
    btl_wv_ini_yy "${PROJECT_BINARY_DIR}/mca/btl/wv/")
    SET(RESULT_APPEND_FILES ${PROJECT_BINARY_DIR}/mca/btl/wv/btl_wv_lex.c)
  ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/mca/btl/wv/btl_wv_lex.c)

  IF(NOT WINVERBS_FOUND)

    MESSAGE(STATUS "looking for Winverbs...")

    # set the default search path
    IF("${WINVERBS_PATH}" STREQUAL "")
      SET(WINVERBS_PATH "$ENV{SystemDrive}/OFED_SDK")
    ENDIF("${WINVERBS_PATH}" STREQUAL "")

    # clear the old find results and start a new search
    UNSET(WINVERBS_INCLUDE_PATH)
	UNSET(WINVERBS_LIB)

    FIND_PATH(WINVERBS_INCLUDE_PATH NAMES winverbs.h PATHS ${WINVERBS_PATH}/Inc/rdma
      DOC "Path to Winverbs include directory, will be detected automatically.")

	FIND_LIBRARY(WINVERBS_LIB winverbs PATHS ${WINVERBS_PATH}/Lib
      DOC "Path to OFED SDK libraries directory, will be detected automatically.")

    IF(WINVERBS_INCLUDE_PATH AND WINVERBS_LIB)
      SET(WINVERBS_FOUND TRUE CACHE INTERNAL "find result of Winverbs.")
      SET(RESULT_INCLUDE_PATH ${WINVERBS_INCLUDE_PATH}/..;${PROJECT_SOURCE_DIR}/mca/btl/wv)
	  SET(RESULT_LINK_LIBRARIES ${WINVERBS_LIB})
      SET(RESULT TRUE)
      MESSAGE(STATUS "looking for Winverbs...found.")

      INSTALL(FILES ${PROJECT_SOURCE_DIR}/mca/btl/wv/mca-btl-wv-device-params.ini
        ${PROJECT_SOURCE_DIR}/mca/btl/wv/help-mpi-btl-wv.txt
        DESTINATION share/openmpi)

    ELSE(WINVERBS_INCLUDE_PATH AND WINVERBS_LIBIBVERBS)
      SET(WINVERBS_FOUND FALSE CACHE INTERNAL "find result of Winverbs.")
      SET(RESULT FALSE)
      MESSAGE(STATUS "looking for Winverbs...not found.")
    ENDIF(WINVERBS_INCLUDE_PATH AND WINVERBS_LIB)

  ELSE(NOT WINVERBS_FOUND)
    SET(RESULT_INCLUDE_PATH ${WINVERBS_INCLUDE_PATH}/..;${OpenMPI_SOURCE_DIR}/ompi/mca/btl/wv)
    SET(RESULT_LINK_LIBRARIES ${WINVERBS_LIB})
    SET(RESULT TRUE)

    INSTALL(FILES ${PROJECT_SOURCE_DIR}/mca/btl/wv/mca-btl-wv-device-params.ini
      ${PROJECT_SOURCE_DIR}/mca/btl/wv/help-mpi-btl-wv.txt
      DESTINATION share/openmpi)
  ENDIF(NOT WINVERBS_FOUND)

ENDIF(NOT OMPI_WANT_WINVERBS)
