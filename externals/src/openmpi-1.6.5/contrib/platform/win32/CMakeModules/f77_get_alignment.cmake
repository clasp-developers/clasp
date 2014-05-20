# Copyright (c) 2008      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_F77_GET_ALIGNMENT
#             in: TYPE         - fortran type to check
#            out: ALIGNMENT    - alignment to return
# ----------------------------------------------------

INCLUDE(F77_find_ext_symbol_convention)

MACRO(OMPI_F77_GET_ALIGNMENT TYPE OUTPUT_VARIABLE)
  MESSAGE(STATUS "Check alignment of Fortran ${TYPE}...")

  OMPI_F77_MAKE_C_FUNCTION(ompi_ac_align_fn align)

  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.f
    "\t program falign\n"
    "\t external align\n"
    "\t $1  w,x,y,z\n"
    "\t CHARACTER a,b,c\n"
    "\t common /foo/a,w,b,x,y,c,z\n"
    "\t call align(w,x,y,z)\n"
    "\t end \n")
  
  IF(EXISTS "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.h")
    SET(ompi_conftest_h "#include \"conftest.h\"")
  ELSE(EXISTS "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.h")
    SET(ompi_conftest_h "")
  ENDIF(EXISTS "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.h")

  FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.c"
    "\t #include <stdio.h> \n"
    "\t #include <stdlib.h> \n"
    "\t $conftest \n"
    "\t  \n"
    "\t #ifdef __cplusplus \n"
    "\t extern \"C\" { \n"
    "\t #endif \n"
    "\t void ${ompi_ac_align_fn}(char *w, char *x, char *y, char *z) \n"
    "\t {   unsigned long aw, ax, ay, az; \n"
    "\t     FILE *f=fopen(\"conftestval\", \"w\"); \n"
    "\t     if (!f) exit(1); \n"
    "\t     aw = (unsigned long) w; \n"
    "\t     ax = (unsigned long) x; \n"
    "\t     ay = (unsigned long) y; \n"
    "\t     az = (unsigned long) z; \n"
    "\t     if (! ((aw%16)||(ax%16)||(ay%16)||(az%16))) fprintf(f, \"%d\n\", 16); \n"
    "\t     else if (! ((aw%12)||(ax%12)||(ay%12)||(az%12))) fprintf(f, \"%d\n\", 12); \n"
    "\t     else if (! ((aw%8)||(ax%8)||(ay%8)||(az%8))) fprintf(f, \"%d\n\", 8); \n"
    "\t     else if (! ((aw%4)||(ax%4)||(ay%4)||(az%4))) fprintf(f, \"%d\n\", 4); \n"
    "\t     else if (! ((aw%2)||(ax%2)||(ay%2)||(az%2))) fprintf(f, \"%d\n\", 2); \n"
    "\t     else fprintf(f, \"%d\n\", 1);  \n"
    "\t     fclose(f); \n"
    "\t } \n"
    "\t #ifdef __cplusplus \n"
    "\t } \n"
    "\t #endif \n"
    )

  EXECUTE_PROCESS(COMMAND ${CL_EXE} /c conftest.c /I${VC_INCLUDE_PATH}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
    OUTPUT_VARIABLE OUTPUT
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE ERROR)
  
  EXECUTE_PROCESS(COMMAND ${F77} conftest.f conftest.obj -o conftest
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
    OUTPUT_VARIABLE OUTPUT
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE ERROR)

  EXECUTE_PROCESS(COMMAND conftest.exe
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
    OUTPUT_VARIABLE OUTPUT
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE ERROR)

  #MESSAGE("RESULT:${RESULT}")

  IF(RESULT)
    MESSAGE(FATAL_ERROR "Could not determine alignment of ${TYPE}.")
  ELSE(RESULT)
    IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
      # read out type size value from the file, and write back to the output variable
      FILE(READ ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval ${OUTPUT_VARIABLE})
      MESSAGE(STATUS "Check alignment of Fortran ${TYPE}...${${OUTPUT_VARIABLE}}")
    ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
      MESSAGE(FATAL_ERROR "Could not determine alignment of ${TYPE}.")
    ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
  ENDIF(RESULT)

ENDMACRO(OMPI_F77_GET_ALIGNMENT TYPE OUTPUT_VARIABLE)
