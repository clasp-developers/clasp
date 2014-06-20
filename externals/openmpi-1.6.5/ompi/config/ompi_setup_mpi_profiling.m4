# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI-specific configuration

AC_DEFUN([OMPI_SETUP_MPI_PROFILING],[
    # There are 2 layers to the MPI Language binidings One layer
    # generates MPI_* bindings. The other layer generates PMPI_*
    # bindings. The following conditions determine whether each (or
    # both) these layers are built.
    # 1. MPI_* bindings are needed if:
    #    - Profiling is not required
    #    - Profiling is required but weak symbols are not
    #      supported
    # 2. PMPI_* bindings are needed if profiling is required.  Hence we
    # define 2 conditionals which tell us whether each of these layers
    # need to be built or NOT
    #
    
    AM_CONDITIONAL(WANT_MPI_BINDINGS_LAYER,
        test "$WANT_MPI_PROFILING" = 0 -o "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1)
    
    AM_CONDITIONAL(WANT_PMPI_BINDINGS_LAYER,
        test "$WANT_MPI_PROFILING" = 1)
    AM_CONDITIONAL(COMPILE_PROFILING_SEPARATELY,
        test "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1)
    AC_DEFINE_UNQUOTED(OMPI_ENABLE_MPI_PROFILING, $WANT_MPI_PROFILING,
        [Whether we want MPI profiling or not])
    AC_DEFINE_UNQUOTED(OPAL_HAVE_WEAK_SYMBOLS, $OPAL_C_HAVE_WEAK_SYMBOLS,
        [Whether we have weak symbols or not])
])
