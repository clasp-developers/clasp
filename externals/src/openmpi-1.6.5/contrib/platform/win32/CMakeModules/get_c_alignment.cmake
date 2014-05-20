#
# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MACRO(C_GET_ALIGNMENT TYPE LANG NAME)

  IF(NOT OPAL_ALIGNMENT_${NAME})

    #
    # Try to compile and run a foo grogram. 
    # The alignment result will be stored in ${CHECK_TYPE}_ALIGNMENT
    #

    MESSAGE( STATUS "Check alignment of ${TYPE} in ${LANG}...")
    
    SET(INCLUDE_HEADERS "#include <stddef.h>
       #include <stdio.h>
       #include <stdlib.h>")

    IF(HAVE_STDINT_H)
        SET(INCLUDE_HEADERS "${INCLUDE_HEADERS}\n#include <stdint.h>\n")
    ENDIF(HAVE_STDINT_H)

    FILE (WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/c_get_${NAME}_alignment.${LANG}"
      "${INCLUDE_HEADERS}
       int main(){
       char diff;
       struct foo {char a; ${TYPE} b;};
       struct foo *p = (struct foo *) malloc(sizeof(struct foo));
       diff = ((char *)&p->b) - ((char *)&p->a);
       return diff;}
    ")

    TRY_RUN(OPAL_ALIGNMENT_${NAME} COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
      "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/c_get_${NAME}_alignment.${LANG}")

    MESSAGE( STATUS "Check alignment of ${TYPE} in ${LANG}...${OPAL_ALIGNMENT_${NAME}}")

  ENDIF(NOT OPAL_ALIGNMENT_${NAME})

ENDMACRO(C_GET_ALIGNMENT TYPE TYPE_ALIGNMENT LANG )