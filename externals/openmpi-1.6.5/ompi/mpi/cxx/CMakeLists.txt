# Copyright (c) 2007-2011 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB CXX_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

# skip the file interface sources if not wanted
IF(NOT OMPI_PROVIDE_MPI_FILE_INTERFACE)
  SET( TMP_SRC "" )
  FOREACH ( FILENAME ${CXX_SOURCE_FILES})
    GET_FILENAME_COMPONENT(relname ${FILENAME} NAME)
    IF (NOT ${relname} MATCHES "file.*[ch]$")
	  LIST(APPEND TMP_SRC ${FILENAME})
    ENDIF (NOT ${relname} MATCHES "file.*[ch]$")
  ENDFOREACH(FILENAME)
  SET( CXX_SOURCE_FILES ${TMP_SRC})
ENDIF(NOT OMPI_PROVIDE_MPI_FILE_INTERFACE)

ADD_LIBRARY (libmpi_cxx ${CXX_SOURCE_FILES})

SET_TARGET_PROPERTIES (libmpi_cxx PROPERTIES LINKER_LANGUAGE CXX PREFIX "" IMPORT_PREFIX "")

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)

  TARGET_LINK_LIBRARIES(libmpi_cxx libmpi libopen-pal)
  # Need OMPI_BUILDING_CXX_BINDINGS_LIBRARY so that we can get the path 
  # names correct inside the MPI C++ library.  OMPI_SKIP_MPICXX is necessary
  # so that mpi.h doesn't include mpicxx.h through the incorrect pathname 
  # in any of the C++ bindings .c files.  Just use the define for this purpose 
  # from user code.
  ADD_DEFINITIONS (-D_USRDLL -DOMPI_BUILDING 
    -DOMPI_BUILDING_CXX_BINDINGS_LIBRARY=1  -DOMPI_SKIP_MPICXX=1
    -DOMPI_IMPORTS -DOPAL_IMPORTS)

ELSE (BUILD_SHARED_LIBS)
  ADD_DEFINITIONS (-D_LIB -DOMPI_BUILDING
    -DOMPI_BUILDING_CXX_BINDINGS_LIBRARY=1  -DOMPI_SKIP_MPICXX=1)
ENDIF(BUILD_SHARED_LIBS)

INSTALL(TARGETS libmpi_cxx
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)

IF (OMPI_DEBUG_BUILD AND WINDOWS_VS)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libmpi_cxx${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD AND WINDOWS_VS)

