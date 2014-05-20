# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MACRO(OMPI_F77_FIND_EXT_SYMBOL_CONVENTION)
  IF(NOT SYMBOL_CONVENTION_CHECK_DONE)
    SET(OMPI_F77_DOUBLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - double underscore")
    SET(OMPI_F77_SINGLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - single underscore")
    SET(OMPI_F77_CAPS 0
      CACHE INTERNAL "external symbol convention - captital")
    SET(OMPI_F77_PLAIN 0
      CACHE INTERNAL "external symbol convention - plain")

    # make sure we know our linking convention...
    MESSAGE(STATUS "Check ${F77} external symbol convention...")
    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.f
      "\t subroutine FOO_bar(a) \n"
      "\t integer a \n"
      "\t a = 1 \n"
      "\t return \n"
      "\t end \n")

    EXECUTE_PROCESS(COMMAND ${F77} ${F77_OPTION_COMPILE} conftest.f ${F77_OUTPUT_OBJ}conftest.lib
      WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      OUTPUT_VARIABLE    OUTPUT
      RESULT_VARIABLE    RESULT
      ERROR_VARIABLE     ERROR)

    SET(OUTPUT_OBJ_FILE "conftest.lib")

    # now run dumpbin to generate an output file
    EXECUTE_PROCESS(COMMAND ${DUMP_UTIL} ${OUTPUT_OBJ_FILE} ${DUMP_UTIL_OPT}
      WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      OUTPUT_VARIABLE    DUMP_OUTPUT
      RESULT_VARIABLE    RESULT
      ERROR_VARIABLE     ERROR)

    STRING(REGEX MATCH foo_bar__\n DOUBLE_UNDERSCORE ${DUMP_OUTPUT})
    STRING(REGEX MATCH foo_bar_\n SINGLE_UNDERSCORE ${DUMP_OUTPUT})
    STRING(REGEX MATCH FOO_bar\n MIXED_CASE ${DUMP_OUTPUT})
    STRING(REGEX MATCH foo_bar\n NO_UNDERSCORE ${DUMP_OUTPUT})
    STRING(REGEX MATCH FOO_BAR\n UPPER_CASE ${DUMP_OUTPUT})

    # set up the corresponding values
    IF(NOT DOUBLE_UNDERSCORE STREQUAL "")
      SET(OMPI_F77_DOUBLE_UNDERSCORE 1
        CACHE INTERNAL "external symbol convention - double underscore")
      SET(FUNC_NAME "foo_bar__")
      SET(ompi_cv_f77_external_symbol "double underscore"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT SINGLE_UNDERSCORE STREQUAL "")
      SET(OMPI_F77_SINGLE_UNDERSCORE 1
        CACHE INTERNAL "external symbol convention - single underscore")
      SET(FUNC_NAME "foo_bar_")
      SET(ompi_cv_f77_external_symbol "single underscore"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT MIXED_CASE STREQUAL "")
      SET(OMPI_F77_CAPS 1
        CACHE INTERNAL "external symbol convention - captital")
      SET(FUNC_NAME "FOO_bar")
      SET(ompi_cv_f77_external_symbol "mixed case"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT NO_UNDERSCORE STREQUAL "")
      SET(OMPI_F77_PLAIN 1
        CACHE INTERNAL "external symbol convention - plain")
      SET(FUNC_NAME "foo_bar")
      SET(ompi_cv_f77_external_symbol "no underscore"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT UPPER_CASE STREQUAL "")
      SET(OMPI_F77_CAPS 1
        CACHE INTERNAL "external symbol convention - captital")
      SET(FUNC_NAME "FOO_BAR")
      SET(ompi_cv_f77_external_symbol "upper case"
        CACHE INTERNAL "F77 external symbol convention")
    ELSE(NOT UPPER_CASE STREQUAL "")
      MESSAGE(FATAL_ERROR "unknow Fortran naming convertion.")
      SET(ompi_cv_f77_external_symbol "unknow")
    ENDIF(NOT DOUBLE_UNDERSCORE STREQUAL "")

    MESSAGE(STATUS "Check ${F77} external symbol convention...${ompi_cv_f77_external_symbol}")

    # now test if we can link the library with c program
    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_c.c 
      "int main(){${FUNC_NAME}();return(0);}")

    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/CMakeLists.txt
      "CMAKE_MINIMUM_REQUIRED(VERSION 2.4.6 FATAL_ERROR)\n"
      "PROJECT(conftest_c C)\n"
      "IF(NOT \"${F77_LIB_PATH}\" STREQUAL \"\")\n"
      "  LINK_DIRECTORIES(\"${F77_LIB_PATH}\")\n"
      "ENDIF(NOT \"${F77_LIB_PATH}\" STREQUAL \"\")\n"
      "LINK_DIRECTORIES(${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/)\n"
      "ADD_EXECUTABLE(conftest_c conftest_c.c)\n"
      "TARGET_LINK_LIBRARIES(conftest_c ${OUTPUT_OBJ_FILE})\n")

    TRY_COMPILE(
      TEST_OK
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      conftest_c
      OUTPUT_VARIABLE MY_OUTPUT)

    #MESSAGE("MY_OUTPUT:${MY_OUTPUT}")

    IF(TEST_OK)
      SET(SYMBOL_CONVENTION_CHECK_DONE TRUE CACHE INTERNAL "Symbol convention check done.")
    ELSE(TEST_OK)
      UNSET(SYMBOL_CONVENTION_CHECK_DONE CACHE)
      MESSAGE(STATUS "${MY_OUTPUT}")
      MESSAGE(STATUS "*** Probably you have to setup the library path of the Fortran compiler.")
      MESSAGE(FATAL_ERROR "C and Fortran 77 compilers are not link compatible.  Cannot continue.")
    ENDIF(TEST_OK)

  ENDIF(NOT SYMBOL_CONVENTION_CHECK_DONE)

ENDMACRO(OMPI_F77_FIND_EXT_SYMBOL_CONVENTION)


# return the corresponding C function name
# OMPI_F77_MAKE_C_FUNCTION
#             in: FUNCTION_NAME     -Fortran function name
#            out: OUTPUT_VARIABLE   -C function name
MACRO(OMPI_F77_MAKE_C_FUNCTION OUTPUT_VARIABLE FUNCTION_NAME)
  IF("${ompi_cv_f77_external_symbol}" STREQUAL "double underscore")
    # so the general rule is that if there is an _ in the function
    # name, then there are two trailing underscores.  Otherwise,
    # there is only one trailing underscore.
    STRING(TOLOWER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
    STRING(REGEX MATCH "_" RESULT ${FUNCTION_NAME})
    IF("${RESULT}" STREQUAL "")
      SET(${OUTPUT_VARIABLE} "${${OUTPUT_VARIABLE}}_")
    ELSE("${RESULT}" STREQUAL "")
      SET(${OUTPUT_VARIABLE} "${${OUTPUT_VARIABLE}}__")
    ENDIF("${RESULT}" STREQUAL "")
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "single underscore")
    STRING(TOLOWER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
    SET(${OUTPUT_VARIABLE} "${${OUTPUT_VARIABLE}}_")
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "mixed case")
    SET(${OUTPUT_VARIABLE} ${FUNCTION_NAME})
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "no underscore")
    STRING(TOLOWER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "upper case")
    STRING(TOUPPER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
  ELSE("${ompi_cv_f77_external_symbol}" STREQUAL "double underscore")
    MESSAGE(FATAL_ERROR "unknown naming convention: ${ompi_cv_f77_external_symbol}")
  ENDIF("${ompi_cv_f77_external_symbol}" STREQUAL "double underscore")

ENDMACRO(OMPI_F77_MAKE_C_FUNCTION OUTPUT_VARIABLE FUNCTION_NAME)