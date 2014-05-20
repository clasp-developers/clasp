#
# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# Some data types are defined in SDK headers, but the SDK include path
# is only reachable from Visual Studio ENV. So try to compile and run a test
# program to check if the data types is defined in SDK headers.
# If the type is defined, also return the size of the type.
# 
# TYPE:             the type to check
# TYPE_NAME:        the uppercase of the type, with underlines if necessary.
# INCLUDE_HEADERS:  the header files defines the type.
# 
# HAVE_${TYPE_NAME}:    if type is found, this value is define as 1.
# SIZEOF_${TYPE_NAME}:  size of the type.

MACRO(CHECK_C_TYPE_EXISTS TYPE TYPE_NAME INCLUDE_HEADERS)

  IF(NOT ${TYPE_NAME}_CHECK_DONE)
    MESSAGE( STATUS "Checking for ${TYPE}...")

    SET(INCLUDE "")
    FOREACH(HEADER ${INCLUDE_HEADERS})
      SET(INCLUDE ${INCLUDE} "#include <${HEADER}>\n")
    ENDFOREACH(HEADER ${INCLUDE_HEADERS})

    STRING(REPLACE ";" "" INCLUDE ${INCLUDE})

    FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_${TYPE_NAME}.c"
      "${INCLUDE}
       int main(){ ${TYPE} test; return sizeof(${TYPE});}")

    TRY_RUN(SIZEOF_${TYPE_NAME} COMPILE_RESULT "${CMAKE_BINARY_DIR}"
      "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_${TYPE_NAME}.c")

    IF(SIZEOF_${TYPE_NAME} GREATER 0)
      SET(HAVE_${TYPE_NAME} 1 CACHE INTERNAL "HAVE_${TYPE_NAME}")
      MESSAGE( STATUS "Checking for ${TYPE}...done")
    ELSE(SIZEOF_${TYPE_NAME} GREATER 0)
      SET(HAVE_${TYPE_NAME} 0 CACHE INTERNAL "HAVE_${TYPE_NAME}")
      MESSAGE( STATUS "Checking for ${TYPE}...failed")
    ENDIF(SIZEOF_${TYPE_NAME} GREATER 0)

    SET(${TYPE_NAME}_CHECK_DONE 1 CACHE INTERNAL "${TYPE_NAME} check finished.")

  ENDIF(NOT ${TYPE_NAME}_CHECK_DONE)

  OMPI_DEF(HAVE_${TYPE_NAME} ${HAVE_${TYPE_NAME}} "Define to 1 if you have the `${TYPE_NAME}' type in ${INCLUDE_HEADERS}" 0 0)

ENDMACRO(CHECK_C_TYPE_EXISTS TYPE TYPE_NAME INCLUDE_HEADERS)
