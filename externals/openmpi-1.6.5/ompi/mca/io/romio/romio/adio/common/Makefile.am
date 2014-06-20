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

include $(top_srcdir)/Makefile.options

# Deliberately did not include async_list.c and req_malloc.c because
# MPICH2-1.0.7 doesn't compile async_list.c and req_malloc.c is only
# required for MPIX_Grequest stuff (which is MPICH2-specific).

noinst_LTLIBRARIES = libadio_common.la
libadio_common_la_SOURCES = \
        ad_aggregate.c \
        ad_aggregate_new.c \
        ad_close.c \
        ad_coll_build_req_new.c \
        ad_coll_exch_new.c \
        ad_darray.c \
        ad_delete.c \
        ad_done.c \
        ad_done_fake.c \
        ad_end.c \
        ad_features.c \
        ad_fcntl.c \
        ad_flush.c \
        ad_fstype.c \
        ad_get_sh_fp.c \
        ad_hints.c \
        ad_init.c \
        ad_io_coll.c \
        ad_iopen.c \
        ad_iread.c \
        ad_iread_fake.c \
        ad_iwrite.c \
        ad_iwrite_fake.c \
        ad_open.c \
        ad_opencoll.c \
        ad_opencoll_failsafe.c \
        ad_opencoll_scalable.c \
        ad_prealloc.c \
        ad_read.c \
        ad_read_coll.c \
        ad_read_str.c \
        ad_read_str_naive.c \
        ad_resize.c \
        ad_seek.c \
        ad_set_sh_fp.c \
        ad_set_view.c \
        ad_subarray.c \
        ad_wait.c \
        ad_wait_fake.c \
        ad_write.c \
        ad_write_coll.c \
        ad_write_nolock.c \
        ad_write_str.c \
        ad_write_str_naive.c \
        adi_close.c \
        byte_offset.c \
        cb_config_list.c \
        eof_offset.c \
        error.c \
        flatten.c \
        get_fp_posn.c \
        greq_fns.c \
        heap-sort.c \
        iscontig.c \
        lock.c \
        malloc.c \
        shfp_fname.c \
        status_setb.c \
        strfns.c \
        system_hints.c
