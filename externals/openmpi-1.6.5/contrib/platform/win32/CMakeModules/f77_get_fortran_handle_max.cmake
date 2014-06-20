# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_F77_GET_FORTRAN_HANDLE_MAX()
# -------------------------------------------------------
# Find the maximum value of fortran integers, then calculate
# min(INT_MAX, max fortran INTEGER).  This represents the maximum
# number of fortran MPI handle index.
MACRO(OMPI_F77_GET_FORTRAN_HANDLE_MAX)

    #store previous value for later use.
    IF(DEFINED OMPI_FINT_MAX)
        SET(OMPI_FINT_MAX_OLD ${OMPI_FINT_MAX})
    ENDIF(DEFINED OMPI_FINT_MAX)

    IF(NOT OMPI_WANT_F77_BINDINGS)
        SET(OMPI_FINT_MAX 0 CACHE INTERNAL "fortran int max")
    ELSE(NOT OMPI_WANT_F77_BINDINGS)
        # Calculate the number of f's that we need to append to the hex
        # value.  Do one less than we really need becaue we assume the
        # top nybble is 0x7 to avoid sign issues.
        MATH(EXPR OMPI_NUMF ${OMPI_SIZEOF_FORTRAN_INTEGER}*2-1)
        SET(OMPI_FINT_MAX 0x7)

        WHILE(${OMPI_NUMF} GREATER 0)
            SET(OMPI_FINT_MAX ${OMPI_FINT_MAX}f CACHE INTERNAL "fortran int max")
            MATH(EXPR OMPI_NUMF ${OMPI_NUMF}-1)
        ENDWHILE(${OMPI_NUMF} GREATER 0)
    ENDIF(NOT OMPI_WANT_F77_BINDINGS)

    #OMPI_CINT_MAX doesn't change, check only once and cache the result.
    IF(NOT DEFINED CINT_MAX_CHECK_DONE)

        FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_handle_max.c
            "#include <stdio.h>\n"
            "#include <limits.h>\n"
            "\n"
            "#ifdef __cplusplus\n"
            "extern \"C\" {\n"
            "#endif\n"
            "\n"
            "void main()\n"
            "{\n"
            "    FILE *fp = fopen(\"fortran_handle_max\", \"w\");\n"
            "    long cint = INT_MAX;\n"
            "    fprintf(fp, \"%ld\", cint);\n"
            "    fclose(fp);\n"
            "}\n"
            )

        EXECUTE_PROCESS(COMMAND ${CMAKE_C_COMPILER} fortran_handle_max.c ${OMPI_C_INCLUDE_DIR}${C_COMPILER_INCLUDE}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
            OUTPUT_VARIABLE OUTPUT
            RESULT_VARIABLE RESULT
            ERROR_VARIABLE ERROR)

        EXECUTE_PROCESS(COMMAND fortran_handle_max.exe
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
            OUTPUT_VARIABLE OUTPUT
            RESULT_VARIABLE RESULT
            ERROR_VARIABLE ERROR)

        IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_handle_max)
            FILE(READ ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_handle_max OUTPUT_VALUE)
            SET(OMPI_CINT_MAX ${OUTPUT_VALUE} CACHE INTERNAL "c int max")
        ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_handle_max)
            SET(OMPI_CINT_MAX 0)
        ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/fortran_handle_max)

    ENDIF(NOT DEFINED CINT_MAX_CHECK_DONE)

    #whenever OMPI_FINT_MAX changes, recalculate OMPI_FORTRAN_HANDLE_MAX
    IF(NOT DEFINED OMPI_FINT_MAX OR NOT "${OMPI_FINT_MAX}" STREQUAL "${OMPI_FINT_MAX_OLD}")

        MESSAGE(STATUS "Check Max handle value for Fortran MPI handles...")

        IF(${OMPI_CINT_MAX} EQUAL 0)
            # wow - something went really wrong.  Be conservative
            SET(OMPI_FORTRAN_HANDLE_MAX 32767 CACHE INTERNAL "Fortran handle max")
        ELSEIF(${OMPI_FINT_MAX} EQUAL 0)
             # we aren't compiling Fortran - just set it to C INT_MAX
            SET(OMPI_FORTRAN_HANDLE_MAX ${OMPI_CINT_MAX} CACHE INTERNAL "Fortran handle max")
        ELSE(${OMPI_FINT_MAX} EQUAL 0)
             # take the lesser of C INT_MAX and Fortran INTEGER
             # max.  The resulting value will then be storable in
             # either type.  There's no easy way to do this in
             # the shell, so make the preprocessor do it.
            SET(OMPI_FORTRAN_HANDLE_MAX "( ${OMPI_FINT_MAX} < ${OMPI_CINT_MAX} ? ${OMPI_FINT_MAX} : ${OMPI_CINT_MAX} )" CACHE INTERNAL "Fortran handle max")
        ENDIF(${OMPI_CINT_MAX} EQUAL 0)

        MESSAGE(STATUS "Check Max handle value for Fortran MPI handles...${OMPI_FORTRAN_HANDLE_MAX}")
        SET(FORTRAN_MAX_HANDLE_CHECK_DONE TRUE CACHE INTERNAL "Fortran handle max check done")

    ENDIF(NOT DEFINED OMPI_FINT_MAX OR NOT "${OMPI_FINT_MAX}" STREQUAL "${OMPI_FINT_MAX_OLD}")

    OMPI_DEF_VAR(OMPI_FORTRAN_HANDLE_MAX "Max handle value for fortran MPI handles, effectively min(INT_MAX, max fortran INTEGER value)." 0 1)

ENDMACRO(OMPI_F77_GET_FORTRAN_HANDLE_MAX)
