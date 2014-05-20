# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_F77_CHECK_REAL16_C_EQUIV
# ----------------------------------------------------
MACRO(OMPI_F77_CHECK_REAL16_C_EQUIV)
  SET(OMPI_REAL16_MATCHES_C 0)

  IF(OMPI_WANT_F77_BINDINGS AND OMPI_HAVE_FORTRAN_REAL16 AND NOT DEFINED REAL16_MATCHES_CHECK_DONE)
    IF(NOT ${ompi_fortran_real16_t} STREQUAL "")
      STRING(TOUPPER ${ompi_fortran_real16_t} REAL16_C_TYPE)

      IF(${OMPI_SIZEOF_FORTRAN_REAL16} EQUAL SIZEOF_${REAL16_C_TYPE})
        OMPI_F77_MAKE_C_FUNCTION(ompi_ac_c_fn c)
        FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_c.c
          "#include <stdio.h>\n"
          "#include <stdlib.h>\n"
          "\n"
          "#ifdef __cplusplus\n"
          "extern \"C\" {\n"
          "#endif\n"
          "void ${ompi_ac_c_fn}(${ompi_fortran_real16_t} *a) {\n"
          "    FILE *fp = fopen(\"conftestval\", \"w\");\n"
          "    if (NULL == fp) exit(1);\n"
          "    fprintf(fp, \"%s\n\", (1.1L == *a) ? \"yes\" : \"no\");\n"
          "    fclose(fp);\n"
          "}\n"
          "#ifdef __cplusplus\n"
          "}\n"
          "#endif\n")

        FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_f.f 
          "\tprogram bogus\n"
          "\tREAL*16 :: foo\n"
          "\tfoo = 1.1\n"
          "\tcall c(foo)\n"
          "\tend program bogus\n")

        EXECUTE_PROCESS(COMMAND ${CMAKE_C_COMPILER} ${OMPI_C_OPTION_COMPILE} conftest_c.c ${OMPI_C_INCLUDE_DIR}${C_COMPILER_INCLUDE}
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
          OUTPUT_VARIABLE OUTPUT
          RESULT_VARIABLE RESULT
          ERROR_VARIABLE ERROR)
      
        EXECUTE_PROCESS(COMMAND ${F77} conftest_f.f conftest_c.obj ${F77_OUTPUT_OBJ}conftest
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
          OUTPUT_VARIABLE OUTPUT
          RESULT_VARIABLE RESULT
          ERROR_VARIABLE ERROR)

        EXECUTE_PROCESS(COMMAND conftest.exe
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
          OUTPUT_VARIABLE OUTPUT
          RESULT_VARIABLE RESULT
          ERROR_VARIABLE ERROR)

        IF(RESULT)
          UNSET(REAL16_MATCHES_CHECK_DONE CACHE)
          MESSAGE(FATAL_ERROR "Can not determine if REAL*16 bit-matches C.")
        ELSE(RESULT)
          IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
            # read out type size value from the file, and write back to the output variable
            FILE(READ ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval ${OUTPUT_VARIABLE})
            MESSAGE(STATUS "Check if REAL*16 bit-matches C...${OUTPUT_VARIABLE}")
            SET(OMPI_REAL16_MATCHES_C 1)
            SET(REAL16_MATCHES_CHECK_DONE TRUE CACHE INTERNAL "Real16 matches c type check done.")
          ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
            UNSET(REAL16_MATCHES_CHECK_DONE CACHE)
            MESSAGE(STATUS "Check if REAL*16 bit-matches C...failed")
          ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
        ENDIF(RESULT)

      ELSE(${OMPI_SIZEOF_FORTRAN_REAL16} EQUAL SIZEOF_${REAL16_C_TYPE})
        SET(REAL16_MATCHES_CHECK_DONE TRUE CACHE INTERNAL "Real16 matches c type check done.")
        MESSAGE(STATUS "Check if REAL*16 bit-matches C...skipped. (no corresponding C type)")
      ENDIF(${OMPI_SIZEOF_FORTRAN_REAL16} EQUAL SIZEOF_${REAL16_C_TYPE})

    ELSE(NOT ${ompi_fortran_real16_t} STREQUAL "")
      SET(REAL16_MATCHES_CHECK_DONE TRUE CACHE INTERNAL "Real16 matches c type check done.")
      MESSAGE(STATUS "Check if REAL*16 bit-matches C...skipped. (no REAL*16)")
    ENDIF(NOT ${ompi_fortran_real16_t} STREQUAL "")

  ELSEIF(NOT OMPI_WANT_F77_BINDINGS)
    UNSET(REAL16_MATCHES_CHECK_DONE CACHE)
  ENDIF(OMPI_WANT_F77_BINDINGS AND OMPI_HAVE_FORTRAN_REAL16  AND NOT DEFINED REAL16_MATCHES_CHECK_DONE)

  OMPI_DEF_VAR(OMPI_REAL16_MATCHES_C "if REAL*16 bit-matches C." 0 1)

ENDMACRO(OMPI_F77_CHECK_REAL16_C_EQUIV)
