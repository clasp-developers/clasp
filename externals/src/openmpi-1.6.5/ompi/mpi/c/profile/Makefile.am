# -*- makefile -*-
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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# OMPI_PROFILING_DEFINES flag s enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory
# we definately need it to be 1.
#
AM_CPPFLAGS = -DOMPI_PROFILING_DEFINES=1

#
# This build needs to go through only if profiling is required.
# Further, this build HAS to go through if profiling is required.
#

noinst_LTLIBRARIES =
if WANT_PMPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_c_pmpi.la
endif

headers = defines.h

nodist_libmpi_c_pmpi_la_SOURCES = \
        pabort.c \
        padd_error_class.c \
        padd_error_code.c \
        padd_error_string.c \
        paddress.c \
        pallgather.c \
        pallgatherv.c \
        palloc_mem.c \
        pallreduce.c \
        palltoall.c \
        palltoallv.c \
        palltoallw.c \
        pattr_delete.c \
        pattr_get.c \
        pattr_put.c \
        pbarrier.c \
        pbcast.c \
        pbsend.c \
        pbsend_init.c \
        pbuffer_attach.c \
        pbuffer_detach.c \
        pcancel.c \
        pcart_coords.c \
        pcart_create.c \
        pcartdim_get.c \
        pcart_get.c \
        pcart_map.c \
        pcart_rank.c \
        pcart_shift.c \
        pcart_sub.c \
        pclose_port.c \
        pcomm_accept.c \
        pcomm_c2f.c \
        pcomm_call_errhandler.c \
        pcomm_compare.c \
        pcomm_connect.c \
        pcomm_create.c \
        pcomm_create_errhandler.c \
        pcomm_create_keyval.c \
        pcomm_delete_attr.c \
        pcomm_disconnect.c \
        pcomm_dup.c \
        pcomm_f2c.c \
        pcomm_free.c \
        pcomm_free_keyval.c \
        pcomm_get_attr.c \
        pcomm_get_errhandler.c \
        pcomm_get_name.c \
        pcomm_get_parent.c \
        pcomm_group.c \
        pcomm_join.c \
        pcomm_rank.c \
        pcomm_remote_group.c \
        pcomm_remote_size.c \
        pcomm_set_attr.c \
        pcomm_set_errhandler.c \
        pcomm_set_name.c \
        pcomm_size.c \
        pcomm_spawn.c \
        pcomm_spawn_multiple.c \
        pcomm_split.c \
        pcomm_test_inter.c \
        pdims_create.c \
        perrhandler_c2f.c \
        perrhandler_create.c \
        perrhandler_f2c.c \
        perrhandler_free.c \
        perrhandler_get.c \
        perrhandler_set.c \
        perror_class.c \
        perror_string.c \
        pexscan.c \
        pfinalize.c \
        pfinalized.c \
        pfree_mem.c \
        pgather.c \
        pgatherv.c \
        pget_address.c \
        pget_count.c \
        pget_elements.c \
        pget_processor_name.c \
        pget_version.c \
        pgraph_create.c \
        pgraph_get.c \
        pgraph_map.c \
        pgraph_neighbors_count.c \
        pgraph_neighbors.c \
        pgraphdims_get.c \
        pgrequest_complete.c \
        pgrequest_start.c \
        pgroup_c2f.c \
        pgroup_compare.c \
        pgroup_difference.c \
        pgroup_excl.c \
        pgroup_f2c.c \
        pgroup_free.c \
        pgroup_incl.c \
        pgroup_intersection.c \
        pgroup_range_excl.c \
        pgroup_range_incl.c \
        pgroup_rank.c \
        pgroup_size.c \
        pgroup_translate_ranks.c \
        pgroup_union.c \
        pibsend.c \
        pinfo_c2f.c \
        pinfo_create.c \
        pinfo_delete.c \
        pinfo_dup.c \
        pinfo_f2c.c \
        pinfo_free.c \
        pinfo_get.c \
        pinfo_get_nkeys.c \
        pinfo_get_nthkey.c \
        pinfo_get_valuelen.c \
        pinfo_set.c \
        pinit.c \
        pinit_thread.c \
        pinitialized.c \
        pintercomm_create.c \
        pintercomm_merge.c \
        piprobe.c \
        pirecv.c \
        pirsend.c \
        pis_thread_main.c \
        pisend.c \
        pissend.c \
        pkeyval_create.c \
        pkeyval_free.c \
        plookup_name.c \
        pop_c2f.c \
        pop_create.c \
        pop_commutative.c \
        pop_f2c.c \
        pop_free.c \
        popen_port.c \
        ppack_external.c \
        ppack_external_size.c \
        ppack.c \
        ppack_size.c \
        ppcontrol.c \
        pprobe.c \
        ppublish_name.c \
        pquery_thread.c \
        precv_init.c \
        precv.c \
        preduce.c \
        preduce_local.c \
        preduce_scatter.c \
        prequest_c2f.c \
        prequest_f2c.c \
        prequest_free.c \
        prequest_get_status.c \
        prsend_init.c \
        prsend.c \
        pscan.c \
        pscatter.c \
        pscatterv.c \
        psend.c \
        psend_init.c \
        psendrecv.c \
        psendrecv_replace.c \
        pssend_init.c \
        pssend.c \
        pstart.c \
        pstartall.c \
        pstatus_c2f.c \
        pstatus_f2c.c \
        pstatus_set_cancelled.c \
        pstatus_set_elements.c \
        ptestall.c \
        ptestany.c \
        ptest.c \
        ptest_cancelled.c \
        ptestsome.c \
        ptopo_test.c \
        ptype_c2f.c \
        ptype_commit.c \
        ptype_contiguous.c \
        ptype_create_darray.c \
        ptype_create_f90_complex.c \
        ptype_create_f90_integer.c \
        ptype_create_f90_real.c \
        ptype_create_hindexed.c \
        ptype_create_hvector.c \
        ptype_create_indexed_block.c \
        ptype_create_keyval.c \
        ptype_create_resized.c \
        ptype_create_struct.c \
        ptype_create_subarray.c \
        ptype_delete_attr.c \
        ptype_dup.c \
        ptype_extent.c \
        ptype_f2c.c \
        ptype_free.c \
        ptype_free_keyval.c \
        ptype_get_attr.c \
        ptype_get_contents.c \
        ptype_get_envelope.c \
        ptype_get_extent.c \
        ptype_get_name.c \
        ptype_get_true_extent.c \
        ptype_hindexed.c \
        ptype_hvector.c \
        ptype_indexed.c \
        ptype_lb.c \
        ptype_match_size.c \
        ptype_set_attr.c \
        ptype_set_name.c \
        ptype_size.c \
        ptype_struct.c \
        ptype_ub.c \
        ptype_vector.c  \
        punpack_external.c \
        punpack.c \
        punpublish_name.c \
        pwait.c \
        pwaitall.c \
        pwaitany.c \
        pwaitsome.c \
	pwtime.c \
	pwtick.c \
        paccumulate.c \
        pget.c \
        pput.c \
        pwin_c2f.c \
        pwin_call_errhandler.c \
        pwin_complete.c  \
        pwin_create_errhandler.c \
        pwin_create_keyval.c \
        pwin_create.c \
        pwin_delete_attr.c \
        pwin_f2c.c \
        pwin_fence.c \
        pwin_free_keyval.c \
        pwin_free.c \
        pwin_get_attr.c \
        pwin_get_errhandler.c \
        pwin_get_group.c \
        pwin_get_name.c  \
        pwin_lock.c \
        pwin_post.c \
        pwin_set_attr.c \
        pwin_set_errhandler.c \
        pwin_set_name.c \
        pwin_start.c \
        pwin_test.c \
        pwin_unlock.c \
        pwin_wait.c

if OMPI_PROVIDE_MPI_FILE_INTERFACE
nodist_libmpi_c_pmpi_la_SOURCES += \
        pfile_c2f.c \
        pfile_call_errhandler.c \
        pfile_close.c \
        pfile_create_errhandler.c \
        pfile_delete.c \
        pfile_f2c.c \
        pfile_get_amode.c \
        pfile_get_atomicity.c \
        pfile_get_byte_offset.c \
        pfile_get_errhandler.c \
        pfile_get_group.c \
        pfile_get_info.c \
        pfile_get_position.c \
        pfile_get_position_shared.c \
        pfile_get_size.c \
        pfile_get_type_extent.c \
        pfile_get_view.c \
        pfile_iread_at.c \
        pfile_iread.c \
        pfile_iread_shared.c \
        pfile_iwrite_at.c \
        pfile_iwrite.c \
        pfile_iwrite_shared.c \
        pfile_open.c \
        pfile_preallocate.c \
        pfile_read_all_begin.c \
        pfile_read_all.c \
        pfile_read_all_end.c \
        pfile_read_at_all_begin.c \
        pfile_read_at_all.c \
        pfile_read_at_all_end.c \
        pfile_read_at.c \
        pfile_read.c \
        pfile_read_ordered_begin.c \
        pfile_read_ordered.c \
        pfile_read_ordered_end.c \
        pfile_read_shared.c \
        pfile_seek.c \
        pfile_seek_shared.c \
        pfile_set_atomicity.c \
        pfile_set_errhandler.c \
        pfile_set_info.c \
        pfile_set_size.c \
        pfile_set_view.c \
        pfile_sync.c \
        pfile_write_all_begin.c \
        pfile_write_all.c \
        pfile_write_all_end.c \
        pfile_write_at_all_begin.c \
        pfile_write_at_all.c \
        pfile_write_at_all_end.c \
        pfile_write_at.c \
        pfile_write.c \
        pfile_write_ordered_begin.c \
        pfile_write_ordered.c \
        pfile_write_ordered_end.c \
        pfile_write_shared.c \
        pregister_datarep.c
endif

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_c_pmpi_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpi/c/$$pname $@ ; \
	fi

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mpi/c/profile
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_c_pmpi_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
