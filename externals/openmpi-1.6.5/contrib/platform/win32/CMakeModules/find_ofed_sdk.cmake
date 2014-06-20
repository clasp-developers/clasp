# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Find OFED SDK headers and libraries.

#
#  OFED_SDK_PATH           - path to OFED SDK dir
#  OFED_SDK_INCLUDE_PATH   - path to verbs.h
#  OFED_SDK_FOUND          - system has OFED SDK
#

# This module is included in a .windows file, it must return two variables:
# RESULT                : find result
# RESULT_INCLUDE_PATH   : the path need to include if RESULT is true
# RESULT_LINK_LIBRARIES : libraries need to be linked

IF(NOT OMPI_WANT_OFED)
  SET(RESULT FALSE)
ELSE(NOT OMPI_WANT_OFED)

  SET(OFED_SDK_PATH "" CACHE PATH
    "Path to OFED SDK root directory, if not in system path, has to be set manually by user.")

  #generate the lex file, if it's not there
  IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/mca/btl/openib/btl_openib_lex.c)
    ADD_FLEX_FILE(OPAL_SOURCE_FILES ./mca/btl/openib/btl_openib_lex.l
    btl_openib_ini_yy "${PROJECT_BINARY_DIR}/mca/btl/openib/")
    SET(RESULT_APPEND_FILES ${PROJECT_BINARY_DIR}/mca/btl/openib/btl_openib_lex.c)
  ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/mca/btl/openib/btl_openib_lex.c)

  IF(NOT OFED_SDK_FOUND)

    MESSAGE(STATUS "looking for OFED SDK...")

    # set the default search path
    IF("${OFED_SDK_PATH}" STREQUAL "")
      SET(OFED_SDK_PATH "$ENV{SystemDrive}/OFED_SDK")
    ENDIF("${OFED_SDK_PATH}" STREQUAL "")

    # clear the old find results and start a new search
    UNSET(OFED_SDK_INCLUDE_PATH)
	UNSET(OFED_SDK_LIBIBVERBS)

    FIND_PATH(OFED_SDK_INCLUDE_PATH NAMES verbs.h PATHS ${OFED_SDK_PATH}/Inc/infiniband
      DOC "Path to OFED SDK include directory, will be detected automatically.")

	FIND_LIBRARY(OFED_SDK_LIBIBVERBS libibverbs PATHS ${OFED_SDK_PATH}/Lib
      DOC "Path to OFED SDK libraries directory, will be detected automatically.")

    FIND_LIBRARY(OFED_SDK_LIBRDMACM librdmacm PATHS ${OFED_SDK_PATH}/Lib
      DOC "Path to OFED SDK libraries directory, will be detected automatically.")

    IF(OFED_SDK_INCLUDE_PATH AND OFED_SDK_LIBIBVERBS)
      SET(OFED_SDK_FOUND TRUE CACHE INTERNAL "find result of OFED SDK.")
      SET(RESULT_INCLUDE_PATH ${OFED_SDK_INCLUDE_PATH}/..;${PROJECT_SOURCE_DIR}/mca/btl/openib)
	  SET(RESULT_LINK_LIBRARIES ${OFED_SDK_LIBIBVERBS} ${OFED_SDK_LIBRDMACM})
      SET(RESULT TRUE)
      MESSAGE(STATUS "looking for OFED SDK...found.")

      UNSET(CMAKE_REQUIRED_INCLUDES)
      UNSET(CMAKE_REQUIRED_LIBRARIES)

      SET(CMAKE_REQUIRED_INCLUDES ${OFED_SDK_PATH}/Inc/infiniband/verbs.h ${OFED_SDK_PATH}/Inc/infiniband/sa.h)
      SET(CMAKE_REQUIRED_LIBRARIES ${OFED_SDK_PATH}/Lib/libibverbs.lib ${OFED_SDK_PATH}/Lib/librdmacm.lib)

      OMPI_CHECK_FUNCTION_EXISTS(ibv_fork_init HAVE_IBV_FORK_INIT)
      OMPI_CHECK_FUNCTION_EXISTS(ibv_get_device_list HAVE_IBV_GET_DEVICE_LIST)
      OMPI_CHECK_FUNCTION_EXISTS(ibv_resize_cq HAVE_IBV_RESIZE_CQ)
      OMPI_CHECK_SYMBOL_EXISTS(IBV_EVENT_CLIENT_REREGISTER "" HAVE_DECL_IBV_EVENT_CLIENT_REREGISTER)

      INSTALL(FILES ${PROJECT_SOURCE_DIR}/mca/btl/openib/mca-btl-openib-device-params.ini
        ${PROJECT_SOURCE_DIR}/mca/btl/openib/help-mpi-btl-openib.txt
        DESTINATION share/openmpi)

    ELSE(OFED_SDK_INCLUDE_PATH AND OFED_SDK_LIBIBVERBS)
      SET(OFED_SDK_FOUND FALSE CACHE INTERNAL "find result of OFED SDK.")
      SET(RESULT FALSE)
      MESSAGE(STATUS "looking for OFED SDK...not found.")
    ENDIF(OFED_SDK_INCLUDE_PATH AND OFED_SDK_LIBIBVERBS)

  ELSE(NOT OFED_SDK_FOUND)
    SET(RESULT_INCLUDE_PATH ${OFED_SDK_INCLUDE_PATH}/..;${OpenMPI_SOURCE_DIR}/ompi/mca/btl/openib)
    SET(RESULT_LINK_LIBRARIES ${OFED_SDK_LIBIBVERBS} ${OFED_SDK_LIBRDMACM})
    SET(RESULT TRUE)

    INSTALL(FILES ${PROJECT_SOURCE_DIR}/mca/btl/openib/mca-btl-openib-device-params.ini
      ${PROJECT_SOURCE_DIR}/mca/btl/openib/help-mpi-btl-openib.txt
      DESTINATION share/openmpi)
  ENDIF(NOT OFED_SDK_FOUND)

ENDIF(NOT OMPI_WANT_OFED)
