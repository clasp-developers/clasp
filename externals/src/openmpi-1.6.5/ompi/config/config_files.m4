# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([OMPI_CONFIG_FILES],[
    AC_CONFIG_FILES([
        ompi/Makefile
        ompi/etc/Makefile
        ompi/include/Makefile
        ompi/include/mpif.h
        ompi/include/mpif-config.h
    
        ompi/datatype/Makefile
        ompi/debuggers/Makefile
    
        ompi/mpi/c/Makefile
        ompi/mpi/c/profile/Makefile
        ompi/mpi/cxx/Makefile
        ompi/mpi/f77/Makefile
        ompi/mpi/f77/profile/Makefile
        ompi/mpi/f90/Makefile
        ompi/mpi/f90/fortran_kinds.sh
        ompi/mpi/f90/fortran_sizes.h
        ompi/mpi/f90/scripts/Makefile
    
        ompi/tools/ompi_info/Makefile
        ompi/tools/wrappers/Makefile
        ompi/tools/wrappers/mpicc-wrapper-data.txt
        ompi/tools/wrappers/mpic++-wrapper-data.txt
        ompi/tools/wrappers/mpif77-wrapper-data.txt
        ompi/tools/wrappers/mpif90-wrapper-data.txt
        ompi/tools/wrappers/ompi.pc
        ompi/tools/wrappers/ompi-c.pc
        ompi/tools/wrappers/ompi-cxx.pc
        ompi/tools/wrappers/ompi-f77.pc
        ompi/tools/wrappers/ompi-f90.pc
        ompi/tools/ortetools/Makefile
        ompi/tools/ompi-server/Makefile
        ompi/tools/ompi-probe/Makefile
        ompi/tools/ompi-profiler/Makefile
    ])
])
