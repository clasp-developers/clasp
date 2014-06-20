#
# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MACRO(CHECK_C_INLINE)
  #
  # Return the inline definition string.
  #
  
  IF(NOT HAVE_INLINE)
    
    # path of foo test programs
    SET (FOO_SOURCE_DIR ${OpenMPI_SOURCE_DIR}/CMakeTests)
    
    MESSAGE( STATUS "Check inline definition...")
    
    FOREACH(KEYWORD "inline" "__inline__" "__inline")
      
      SET(HAVE_INLINE FALSE)
      FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/test_inline.c"
        "typedef int foo_t;
         static inline foo_t static_foo(){return 0;}
         foo_t foo(){return 0;}
         int main(int argc, char *argv[]){return 0;}
        ")
      
      TRY_COMPILE(C_HAS_${KEYWORD} "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/CMakeTmp/"
        "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/test_inline.c"
        COMPILE_DEFINITIONS "-Dinline=${KEYWORD}")

      IF(C_HAS_${KEYWORD})
        SET(HAVE_INLINE TRUE CACHE INTERNAL "have 'inline' definition")
        SET(INLINE_STRING ${KEYWORD} CACHE INTERNAL "'inline' definition")
        MESSAGE( STATUS "Checking inline definition...${INLINE_STRING}")
        BREAK()
      ENDIF(C_HAS_${KEYWORD})
    ENDFOREACH(KEYWORD)
    
    IF(NOT HAVE_INLINE)
      MESSAGE(FATAL_ERROR "Check inline definition...failed. Cannot continue.")
    ENDIF(NOT HAVE_INLINE)
    
  ENDIF(NOT HAVE_INLINE)

  OMPI_DEF(inline ${INLINE_STRING} "Define to `__inline__' or `__inline'." 0 1)

ENDMACRO(CHECK_C_INLINE)