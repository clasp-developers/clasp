#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# Open MPI: Note that we ignore the fortran subdirectory here; we only
# need C bindings.  We also ignore the f2c / c2f source files; we
# don't need them.
#

include $(top_srcdir)/Makefile.options

SUBDIRS = glue/openmpi

# Intentionally do *not* include iotest.c and iowait.c.  Since we're
# using MPI_HAS_GREQUEST, ROMIO makes MPIO_Test and MPIO_Wait be just
# plain vanilla [P]MPI_Test and [P]MPI_WAIT, which we already have
# defined at the top MPI layer (you can get duplicate symbol errors
# this way).  And due to OMPI/ROMIO integration, we don't need
# MPIO_Test and MPIO_Wait, anyway.

noinst_LTLIBRARIES = libmpi-io.la
libmpi_io_la_SOURCES = \
        close.c \
        delete.c \
        fsync.c \
        get_amode.c \
        get_atom.c \
        get_bytoff.c \
        get_errh.c \
        get_extent.c \
        get_group.c \
        get_info.c \
        get_posn.c \
        get_posn_sh.c \
        get_size.c \
        get_view.c \
        iread_at.c \
        iread.c \
        iread_sh.c \
        iwrite_at.c \
        iwrite.c \
        iwrite_sh.c \
        mpioimpl.h \
        mpioprof.h \
        open.c \
        prealloc.c \
        rd_atallb.c \
        rd_atalle.c \
        read_allb.c \
        read_all.c \
        read_alle.c \
        read_atall.c \
        read_at.c \
        read.c \
        read_ordb.c \
        read_ord.c \
        read_orde.c \
        read_sh.c \
        seek.c \
        seek_sh.c \
        set_atom.c \
        set_errh.c \
        set_info.c \
        set_size.c \
        set_view.c \
        wr_atallb.c \
        wr_atalle.c \
        write_allb.c \
        write_all.c \
        write_alle.c \
        write_atall.c \
        write_at.c \
        write.c \
        write_ordb.c \
        write_ord.c \
        write_orde.c \
        write_sh.c \
        register_datarep.c \
        mpiu_greq.c \
        mpich2_fileutil.c \
        mpir-mpioinit.c
