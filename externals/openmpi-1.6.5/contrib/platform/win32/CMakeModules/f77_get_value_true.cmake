# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_F77_GET_VALUE_TRUE()
# -------------------------------------------------------
# Determine the value of .TRUE. of this Fortran compiler.
MACRO(OMPI_F77_GET_VALUE_TRUE)

    IF(OMPI_WANT_F77_BINDINGS AND NOT DEFINED FORTRAN_VALUE_CHECK_DONE)

        MESSAGE(STATUS "Check Fortran value for .TRUE. logical type...")

        OMPI_F77_MAKE_C_FUNCTION(ompi_print_logical_fn print)

        FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_c.c 
            "#include <stdio.h>\n"
            "#include <stdlib.h>\n"
            "\n"
            "#ifdef __cplusplus\n"
            "extern \"C\" {\n"
            "#endif\n"
            "\n"
            "void ${ompi_print_logical_fn}(${ompi_fortran_logical_t} * logical);\n"
            "\n"
            "void ${ompi_print_logical_fn}(${ompi_fortran_logical_t} * logical)\n"
            "{\n"
            "    FILE *f=fopen(\"fortran_true_value\", \"w\");\n"
            "    if (!f) exit(1);\n"
            "\n"
            "    if( ${SIZEOF_INT} >= sizeof(${ompi_fortran_logical_t}) ) {\n"
            "        fprintf(f, \"%d\\n\", (int)*logical);\n"
            "    } else if (${SIZEOF_LONG} >= sizeof(${ompi_fortran_logical_t}) ) {\n"
            "	fprintf(f, \"%ld\\n\", (long) *logical);\n"
            "#ifdef HAVE_LONG_LONG\n"
            "    } else if (${SIZEOF_LONG_LONG} >= sizeof(${ompi_fortran_logical_t}) ) {\n"
            "        fprintf(f, \"%lld\\n\", (long long) *logical);\n"
            "#endif\n"
            "    } else {\n"
            "        exit(1);\n"
            "    }\n"
            "}\n"
            "\n"
            "#ifdef __cplusplus\n"
            "}\n"
            "#endif\n")

        FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_f.f
            "\tprogram main\n"
            "\tlogical value\n"
            "\tvalue=.TRUE.\n"
            "\tCALL print(value)\n"
            "\tend\n")

        IF(NOT "${C_COMPILER_INCLUDE}" STREQUAL "")
          SET(EXECUTE_OPT "${OMPI_C_INCLUDE_DIR}${C_COMPILER_INCLUDE}")
        ENDIF(NOT "${C_COMPILER_INCLUDE}" STREQUAL "")

        EXECUTE_PROCESS(COMMAND ${CMAKE_C_COMPILER} ${OMPI_C_OPTION_COMPILE} conftest_c.c ${EXECUTE_OPT} ${OMPI_C_OUTPUT_OBJ}conftest_c.obj
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
            OUTPUT_VARIABLE OUTPUT
            RESULT_VARIABLE RESULT
            ERROR_VARIABLE ERROR)

        IF(RESULT)
            MESSAGE(STATUS "${OUTPUT}\n${ERROR}")
            MESSAGE(FATAL_ERROR "Could not determine value of Fortran .TRUE..  Aborting.")
        ENDIF(RESULT)

        EXECUTE_PROCESS(COMMAND ${F77} ${F77_OPTION_COMPILE} conftest_f.f ${F77_OUTPUT_OBJ}conftest_f.obj
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
            OUTPUT_VARIABLE OUTPUT
            RESULT_VARIABLE RESULT
            ERROR_VARIABLE ERROR)

        IF(RESULT)
            MESSAGE(STATUS "${OUTPUT}\n${ERROR}")
            MESSAGE(FATAL_ERROR "Could not determine value of Fortran .TRUE..  Aborting.")
        ENDIF(RESULT)

        EXECUTE_PROCESS(COMMAND ${F77} conftest_f.obj conftest_c.obj ${F77_OUTPUT_EXE}conftest.exe
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
            OUTPUT_VARIABLE OUTPUT
            RESULT_VARIABLE RESULT
            ERROR_VARIABLE ERROR)

        IF(RESULT)
            MESSAGE(STATUS "${OUTPUT}\n${ERROR}")
            MESSAGE(FATAL_ERROR "Could not determine value of Fortran .TRUE..  Aborting.")
        ENDIF(RESULT)

        EXECUTE_PROCESS(COMMAND conftest.exe
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
            OUTPUT_VARIABLE OUTPUT
            RESULT_VARIABLE RESULT
            ERROR_VARIABLE ERROR)

        IF(RESULT)
            UNSET(FORTRAN_VALUE_CHECK_DONE CACHE)
            MESSAGE(STATUS "${OUTPUT}\n${ERROR}")
            MESSAGE(FATAL_ERROR "Could not determine value of Fortran .TRUE..  Aborting.")
        ELSE(RESULT)
            IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_true_value)
              FILE(READ ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_true_value OUTPUT_VALUE)
              MESSAGE(STATUS "Check Fortran value for .TRUE. logical type...${OUTPUT_VALUE}")
              SET(OMPI_FORTRAN_VALUE_TRUE ${OUTPUT_VALUE} CACHE INTERNAL "Fortran value true")
              SET(FORTRAN_VALUE_CHECK_DONE TRUE CACHE INTERNAL "Fortran value true check done")
            ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_true_value)
              UNSET(FORTRAN_VALUE_CHECK_DONE CACHE)
              MESSAGE(FATAL_ERROR "Could not determine value of Fortran .TRUE..  Aborting.")
            ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_true_value)
        ENDIF(RESULT)

    ELSEIF(NOT OMPI_WANT_F77_BINDINGS)
        SET(OMPI_FORTRAN_VALUE_TRUE 0)
        UNSET(FORTRAN_VALUE_CHECK_DONE CACHE)
    ENDIF(OMPI_WANT_F77_BINDINGS AND NOT DEFINED FORTRAN_VALUE_CHECK_DONE)

    OMPI_DEF_VAR(OMPI_FORTRAN_VALUE_TRUE "Fortran value for .TRUE. logical type" 0 1)

ENDMACRO(OMPI_F77_GET_VALUE_TRUE)
