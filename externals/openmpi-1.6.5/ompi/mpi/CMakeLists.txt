# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

IF(OMPI_WANT_CXX_BINDINGS)
  ADD_SUBDIRECTORY(cxx)
ELSE(OMPI_WANT_CXX_BINDINGS)
  MESSAGE(STATUS "Skipping MPI C++ interface")
ENDIF(OMPI_WANT_CXX_BINDINGS)

IF(OMPI_WANT_F77_BINDINGS)
  ADD_SUBDIRECTORY(f77)
ELSE(OMPI_WANT_F77_BINDINGS)
  MESSAGE(STATUS "Skipping MPI F77 interface")
ENDIF(OMPI_WANT_F77_BINDINGS)
