#
# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# make a temporary copy of the input file
FILE(READ ${OpenMPI_SOURCE_DIR}/ompi/include/mpi.h.in MPI_H_IN)

# change the syntax of the copy into CMake understandable
STRING(REGEX REPLACE "#undef ([-_a-zA-Z0-9]+)[ ]*\n" "#cmakedefine \\1 \${\\1}\n" MPI_H_OUT "${MPI_H_IN}")
STRING(REPLACE "#cmakedefine OMPI_HAVE_FORTRAN_" "#define OMPI_HAVE_FORTRAN_"  MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "{OMPI_PROVIDE_MPI_FILE_INTERFACE}"
 "{OMPI_PROVIDE_MPI_FILE_INTERFACE_VAL}" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "\${OMPI_WANT_CXX_BINDINGS}" "1" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "\${OMPI_WANT_F77_BINDINGS}" "1" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "\${OMPI_WANT_F90_BINDINGS}" "1" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "\${OMPI_WANT_MPI_CXX_SEEK}" "1" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "\${OMPI_WANT_MPI_INTERFACE_WARNING}" "1" MPI_H_OUT "${MPI_H_OUT}")

FILE(WRITE ${OpenMPI_BINARY_DIR}/ompi/include/mpi.h.in "${MPI_H_OUT}")

# generate mpi.h
CONFIGURE_FILE(${OpenMPI_BINARY_DIR}/ompi/include/mpi.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpi.h)

IF(OMPI_WANT_F77_BINDINGS)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/mpif.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpif.h)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/mpif-config.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpif-config.h)
ENDIF(OMPI_WANT_F77_BINDINGS)
