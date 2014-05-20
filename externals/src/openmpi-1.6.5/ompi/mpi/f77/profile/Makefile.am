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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


#
# OMPI_PROFILING_DEFINES flag is enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory
# we definately need it to be 1.
#
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=1 -DOMPI_COMPILING_F77_WRAPPERS=1
#
# This build needs to go through only if profiling is required.
# Further, this build HAS to go through if profiling is required.
#

noinst_LTLIBRARIES =
if WANT_PMPI_F77_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_f77_pmpi.la
else
noinst_LTLIBRARIES +=
endif

headers = \
    defines.h

nodist_libmpi_f77_pmpi_la_SOURCES = \
        pabort_f.c \
        padd_error_class_f.c \
        padd_error_code_f.c \
        padd_error_string_f.c \
        paddress_f.c \
        pallgather_f.c \
        pallgatherv_f.c \
        palloc_mem_f.c \
        pallreduce_f.c \
        palltoall_f.c \
        palltoallv_f.c \
        palltoallw_f.c \
        pattr_delete_f.c \
        pattr_get_f.c \
        pattr_put_f.c \
        pbarrier_f.c \
        pbcast_f.c \
        pbsend_f.c \
        pbsend_init_f.c \
        pbuffer_attach_f.c \
        pbuffer_detach_f.c \
        pcancel_f.c \
        pcart_coords_f.c \
        pcart_create_f.c \
        pcartdim_get_f.c \
        pcart_get_f.c \
        pcart_map_f.c \
        pcart_rank_f.c \
        pcart_shift_f.c \
        pcart_sub_f.c \
        pclose_port_f.c \
        pcomm_accept_f.c \
        pcomm_call_errhandler_f.c \
        pcomm_compare_f.c \
        pcomm_connect_f.c \
        pcomm_create_errhandler_f.c \
        pcomm_create_f.c \
        pcomm_create_keyval_f.c \
        pcomm_delete_attr_f.c \
        pcomm_disconnect_f.c \
        pcomm_dup_f.c \
        pcomm_free_f.c \
        pcomm_free_keyval_f.c \
        pcomm_get_attr_f.c \
        pcomm_get_errhandler_f.c \
        pcomm_get_name_f.c \
        pcomm_get_parent_f.c \
        pcomm_group_f.c \
        pcomm_join_f.c \
        pcomm_rank_f.c \
        pcomm_remote_group_f.c \
        pcomm_remote_size_f.c \
        pcomm_set_attr_f.c \
        pcomm_set_errhandler_f.c \
        pcomm_set_name_f.c \
        pcomm_size_f.c \
        pcomm_spawn_f.c \
        pcomm_spawn_multiple_f.c \
        pcomm_split_f.c \
        pcomm_test_inter_f.c \
        pdims_create_f.c \
        perrhandler_create_f.c \
        perrhandler_free_f.c \
        perrhandler_get_f.c \
        perrhandler_set_f.c \
        perror_class_f.c \
        perror_string_f.c \
        pexscan_f.c \
        pfinalized_f.c \
        pfinalize_f.c \
        pfree_mem_f.c \
        pgather_f.c \
        pgatherv_f.c \
        pget_address_f.c \
        pget_count_f.c \
        pget_elements_f.c \
        pget_processor_name_f.c \
        pget_version_f.c \
        pgraph_create_f.c \
        pgraphdims_get_f.c \
        pgraph_get_f.c \
        pgraph_map_f.c \
        pgraph_neighbors_count_f.c \
        pgraph_neighbors_f.c \
        pgrequest_complete_f.c \
        pgrequest_start_f.c \
        pgroup_compare_f.c \
        pgroup_difference_f.c \
        pgroup_excl_f.c \
        pgroup_free_f.c \
        pgroup_incl_f.c \
        pgroup_intersection_f.c \
        pgroup_range_excl_f.c \
        pgroup_range_incl_f.c \
        pgroup_rank_f.c \
        pgroup_size_f.c \
        pgroup_translate_ranks_f.c \
        pgroup_union_f.c \
        pibsend_f.c \
        pinfo_create_f.c \
        pinfo_delete_f.c \
        pinfo_dup_f.c \
        pinfo_free_f.c \
        pinfo_get_f.c \
        pinfo_get_nkeys_f.c \
        pinfo_get_nthkey_f.c \
        pinfo_get_valuelen_f.c \
        pinfo_set_f.c \
        pinit_f.c \
        pinitialized_f.c \
        pinit_thread_f.c \
        pintercomm_create_f.c \
        pintercomm_merge_f.c \
        piprobe_f.c \
        pirecv_f.c \
        pirsend_f.c \
        pisend_f.c \
        pissend_f.c \
        pis_thread_main_f.c \
        pkeyval_create_f.c \
        pkeyval_free_f.c \
        plookup_name_f.c \
        pop_commutative_f.c \
        pop_create_f.c \
        popen_port_f.c \
        pop_free_f.c \
        ppack_external_f.c \
        ppack_external_size_f.c \
        ppack_f.c \
        ppack_size_f.c \
        ppcontrol_f.c \
        pprobe_f.c \
        ppublish_name_f.c \
        pquery_thread_f.c \
        precv_f.c \
        precv_init_f.c \
        preduce_f.c \
        preduce_local_f.c \
        preduce_scatter_f.c \
        prequest_free_f.c \
        prequest_get_status_f.c \
        prsend_f.c \
        prsend_init_f.c \
        pscan_f.c \
        pscatter_f.c \
        pscatterv_f.c \
        psend_f.c \
        psend_init_f.c \
        psendrecv_f.c \
        psendrecv_replace_f.c \
        pssend_f.c \
        pssend_init_f.c \
        pstartall_f.c \
        pstart_f.c \
        pstatus_set_cancelled_f.c \
        pstatus_set_elements_f.c \
        ptestall_f.c \
        ptestany_f.c \
        ptest_cancelled_f.c \
        ptest_f.c \
        ptestsome_f.c \
        ptopo_test_f.c \
        ptype_commit_f.c \
        ptype_contiguous_f.c \
        ptype_create_darray_f.c \
        ptype_create_f90_complex_f.c \
        ptype_create_f90_integer_f.c \
        ptype_create_f90_real_f.c \
        ptype_create_hindexed_f.c \
        ptype_create_hvector_f.c \
        ptype_create_indexed_block_f.c \
        ptype_create_keyval_f.c \
        ptype_create_resized_f.c \
        ptype_create_struct_f.c \
        ptype_create_subarray_f.c \
        ptype_delete_attr_f.c \
        ptype_dup_f.c \
        ptype_extent_f.c \
        ptype_free_f.c \
        ptype_free_keyval_f.c \
        ptype_get_attr_f.c \
        ptype_get_contents_f.c \
        ptype_get_envelope_f.c \
        ptype_get_extent_f.c \
        ptype_get_name_f.c \
        ptype_get_true_extent_f.c \
        ptype_hindexed_f.c \
        ptype_hvector_f.c \
        ptype_indexed_f.c \
        ptype_lb_f.c \
        ptype_match_size_f.c \
        ptype_set_attr_f.c \
        ptype_set_name_f.c \
        ptype_size_f.c \
        ptype_struct_f.c \
        ptype_ub_f.c \
        ptype_vector_f.c \
        punpack_external_f.c \
        punpack_f.c \
        punpublish_name_f.c \
        pwaitall_f.c \
        pwaitany_f.c \
        pwait_f.c \
        pwaitsome_f.c \
	pwtick_f.c \
	pwtime_f.c \
        paccumulate_f.c \
        pget_f.c \
        pput_f.c \
        pwin_call_errhandler_f.c \
        pwin_complete_f.c \
        pwin_create_errhandler_f.c \
        pwin_create_f.c \
        pwin_create_keyval_f.c \
        pwin_delete_attr_f.c \
        pwin_fence_f.c \
        pwin_free_f.c \
        pwin_free_keyval_f.c \
        pwin_get_attr_f.c \
        pwin_get_errhandler_f.c \
        pwin_get_group_f.c \
        pwin_get_name_f.c \
        pwin_lock_f.c \
        pwin_post_f.c \
        pwin_set_attr_f.c \
        pwin_set_errhandler_f.c \
        pwin_set_name_f.c \
        pwin_start_f.c \
        pwin_test_f.c \
        pwin_unlock_f.c \
        pwin_wait_f.c

if OMPI_PROVIDE_MPI_FILE_INTERFACE
nodist_libmpi_f77_pmpi_la_SOURCES += \
        pfile_call_errhandler_f.c \
        pfile_close_f.c \
        pfile_create_errhandler_f.c \
        pfile_delete_f.c \
        pfile_get_amode_f.c \
        pfile_get_atomicity_f.c \
        pfile_get_byte_offset_f.c \
        pfile_get_errhandler_f.c \
        pfile_get_group_f.c \
        pfile_get_info_f.c \
        pfile_get_position_f.c \
        pfile_get_position_shared_f.c \
        pfile_get_size_f.c \
        pfile_get_type_extent_f.c \
        pfile_get_view_f.c \
        pfile_iread_at_f.c \
        pfile_iread_f.c \
        pfile_iread_shared_f.c \
        pfile_iwrite_at_f.c \
        pfile_iwrite_f.c \
        pfile_iwrite_shared_f.c \
        pfile_open_f.c \
        pfile_preallocate_f.c \
        pfile_read_all_begin_f.c \
        pfile_read_all_end_f.c \
        pfile_read_all_f.c \
        pfile_read_at_all_begin_f.c \
        pfile_read_at_all_end_f.c \
        pfile_read_at_all_f.c \
        pfile_read_at_f.c \
        pfile_read_f.c \
        pfile_read_ordered_begin_f.c \
        pfile_read_ordered_end_f.c \
        pfile_read_ordered_f.c \
        pfile_read_shared_f.c \
        pfile_seek_f.c \
        pfile_seek_shared_f.c \
        pfile_set_atomicity_f.c \
        pfile_set_errhandler_f.c \
        pfile_set_info_f.c \
        pfile_set_size_f.c \
        pfile_set_view_f.c \
        pfile_sync_f.c \
        pfile_write_all_begin_f.c \
        pfile_write_all_end_f.c \
        pfile_write_all_f.c \
        pfile_write_at_all_begin_f.c \
        pfile_write_at_all_end_f.c \
        pfile_write_at_all_f.c \
        pfile_write_at_f.c \
        pfile_write_f.c \
        pfile_write_ordered_begin_f.c \
        pfile_write_ordered_end_f.c \
        pfile_write_ordered_f.c \
        pfile_write_shared_f.c \
        pregister_datarep_f.c
endif

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_f77_pmpi_la_SOURCES):
	if test ! -r $@ ; then \
	    pname=`echo $@ | cut -b '2-'` ; \
	    $(LN_S) $(top_srcdir)/ompi/mpi/f77/$$pname $@ ; \
	fi

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mpi/f77/profile
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_f77_pmpi_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
