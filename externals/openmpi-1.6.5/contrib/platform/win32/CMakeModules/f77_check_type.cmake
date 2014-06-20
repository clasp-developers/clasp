# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_F77_CHECK_TYPE
#         in: TYPE         - fortran type to check.
#        out: HAVE_TYPE    - 0/1 whether we have that type.
# -----------------------------------------------------------------

MACRO(OMPI_F77_CHECK_TYPE TYPE HAVE_TYPE)

  IF(NOT DEFINED ${TYPE_NAME}_CHECK_DONE)

    MESSAGE(STATUS "Check if Fortran 77 compiler supports ${TYPE}...")

    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_fortran_type.f
      "\t program main \n"
      "\t ${TYPE} bogus_variable \n"
      "\t END \n")

    EXECUTE_PROCESS(COMMAND ${F77} check_fortran_type.f
      WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      OUTPUT_VARIABLE    OUTPUT
      RESULT_VARIABLE    RESULT
      ERROR_VARIABLE     ERROR)

    IF(RESULT)
      SET(${HAVE_TYPE} 0 CACHE INTERNAL "have Fortran ${TYPE}")
      MESSAGE(STATUS "Check if Fortran 77 compiler supports ${TYPE}...no")
    ELSE(RESULT)
      SET(${HAVE_TYPE} 1 CACHE INTERNAL "have Fortran ${TYPE}")
      MESSAGE(STATUS "Check if Fortran 77 compiler supports ${TYPE}...yes")
    ENDIF(RESULT)

    SET(${TYPE_NAME}_CHECK_DONE TRUE CACHE INTERNAL "${TYPE_NAME} check done")

  ENDIF(NOT DEFINED ${TYPE_NAME}_CHECK_DONE)

ENDMACRO(OMPI_F77_CHECK_TYPE TYPE HAVE_TYPE)