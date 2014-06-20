#
# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# check compiler support of bool and
# possibly get its size and alignment
MACRO(OMPI_CHECK_BOOL)

  FOREACH(LANG c cxx)

    STRING(TOUPPER ${LANG} LANG_U)

    IF(NOT DEFINED SIZEOF_${LANG_U}_BOOL)

      MESSAGE( STATUS "Checking size of ${LANG} bool...")

      FILE (WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_sizeof_bool.${LANG}"
        "#include <stdio.h>
       int main() {return sizeof(bool);}
      ")

      TRY_RUN(SIZEOF_${LANG_U}_BOOL COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
        "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_sizeof_bool.${LANG}")

      IF(SIZEOF_${LANG_U}_BOOL GREATER 0)
        MESSAGE(STATUS "Checking size of ${LANG} bool...${SIZEOF_${LANG_U}_BOOL}")
        C_GET_ALIGNMENT(bool ${LANG} BOOL)
        SET(OPAL_ALIGNMENT_${LANG_U}_BOOL ${OPAL_ALIGNMENT_BOOL} CACHE INTERNAL "Sizeof ${LANG} bool.")
      ELSE(SIZEOF_${LANG_U}_BOOL GREATER 0)
        MESSAGE(STATUS "Checking size of ${LANG} bool...failed")
        SET(SIZEOF_${LANG_U}_BOOL 1 CACHE INTERNAL "Sizeof ${LANG} bool.")
        SET(OPAL_ALIGNMENT_${LANG_U}_BOOL 1 CACHE INTERNAL "Sizeof ${LANG} bool.")
        SET(OPAL_NEED_${LANG_U}_BOOL 1 CACHE INTERNAL "true if compiler doesn't support bool.")
      ENDIF(SIZEOF_${LANG_U}_BOOL GREATER 0)

    ENDIF(NOT DEFINED SIZEOF_${LANG_U}_BOOL)

    OMPI_DEF_VAR(OPAL_NEED_${LANG_U}_BOOL
      "Define to 1 if the C compiler doesn't support bool\n   without any other help (such as <stdbool.h>)." 0 0)
    OMPI_DEF_VAR(SIZEOF_${LANG_U}_BOOL "The size of ${LANG} `bool'." 0 1)
    OMPI_DEF_VAR(OPAL_ALIGNMENT_${LANG_U}_BOOL "Alignment of ${LANG} `bool'." 0 1)

  ENDFOREACH(LANG c cxx)

  # Opal defines these without language type....
  OMPI_DEF(SIZEOF_BOOL 1 "The size of `bool'." 0 1)
  OMPI_DEF(OPAL_ALIGNMENT_BOOL 1 "Sizeof bool." 0 1)

ENDMACRO(OMPI_CHECK_BOOL)