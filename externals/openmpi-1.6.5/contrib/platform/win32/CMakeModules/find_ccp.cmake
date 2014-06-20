# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# The CCP components need to import the type library ccpapi.tlb,
# if it's not installed, the CCP components won't be built.

# This module is included in a .windows file, it must return two variables:
# RESULT               : find result
# RESULT_INCLUDE_PATH  : the path need to include if RESULT is true

IF(NOT ORTE_WANT_CCP)
  SET(RESULT FALSE)
ELSEIF(NOT CCP_FOUND)
  MESSAGE(STATUS "looking for ccp...")

  IF(CMAKE_CL_64)
    FIND_PATH(CCP_LIB_PATH ccpapi.tlb PATHS $ENV{CCP_LIB64} )
  ELSE(CMAKE_CL_64)
    FIND_PATH(CCP_LIB_PATH ccpapi.tlb PATHS $ENV{CCP_LIB32} )
  ENDIF(CMAKE_CL_64)

  IF(CCP_LIB_PATH)
    SET(CCP_FOUND TRUE CACHE INTERNAL "find result of CCP.")
    SET(RESULT TRUE)
    SET(RESULT_INCLUDE_PATH ${CCP_LIB_PATH})
    MESSAGE(STATUS "looking for ccp...found.")
  ELSE(CCP_LIB_PATH)
    SET(CCP_FOUND FALSE CACHE INTERNAL "find result of CCP.")
    SET(RESULT FALSE)   
    MESSAGE(STATUS "looking for ccp...not found.")
  ENDIF(CCP_LIB_PATH)

ELSE(NOT CCP_FOUND)
  SET(RESULT_INCLUDE_PATH ${CCP_LIB_PATH})
  SET(RESULT TRUE)
ENDIF(NOT ORTE_WANT_CCP)
