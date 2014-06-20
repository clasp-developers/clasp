#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = CMakeLists.txt

SUBDIRS = profile

#
# OMPI_PRPOFILING_DEFINES flag is enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory,
# we need it to be 0
#

AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_F77_WRAPPERS=1

# The top directory only builds MPI_* bindings and some support
# glue. The bottom directory only builds PMPI_* bindings.  Each
# directory is built when certain conditions are met.  If the F77
# bindings are disabled, nothing in the top or bottom directory is
# built.
#
# If the F77 MPI API bindings are enabled, the glue functions in the
# top directory are always built.
#
# The cases where the MPI API functions are build top directory needs
# to be built are:
#
# 1. When profiling is disabled
# 2. When profiling is enabled but weak symbol support is absent
#

lib_LTLIBRARIES =
libmpi_f77_la_LIBADD = $(top_builddir)/ompi/libmpi.la
libmpi_f77_la_LDFLAGS = -version-info $(libmpi_f77_so_version)

# Are we building the F77 bindings at all?
if OMPI_WANT_F77_BINDINGS
# If yes, then we need to build the installable library and the glue
# convenience library that will be sucked up into the main libmpi.
lib_LTLIBRARIES += libmpi_f77.la
# Do we need to suck in the convenience library from the lower
# directory?
if WANT_PMPI_F77_BINDINGS_LAYER
libmpi_f77_la_LIBADD += profile/libmpi_f77_pmpi.la
endif
endif

headers = \
    bindings.h \
    constants.h \
    datarep.h \
    fint_2_int.h \
    prototypes_mpi.h \
    f77_strings.h

#
# These files are only built and added to libmpi_f77.la in certain cases.
#
libmpi_f77_la_SOURCES =

if WANT_MPI_F77_BINDINGS_LAYER
libmpi_f77_la_SOURCES += \
        abort_f.c \
        add_error_class_f.c \
        add_error_code_f.c \
        add_error_string_f.c \
        address_f.c \
        allgather_f.c \
        allgatherv_f.c \
        alloc_mem_f.c \
        allreduce_f.c \
        alltoall_f.c \
        alltoallv_f.c \
        alltoallw_f.c \
        attr_delete_f.c \
        attr_get_f.c \
        attr_put_f.c \
        barrier_f.c \
        bcast_f.c \
        bsend_f.c \
        bsend_init_f.c \
        buffer_attach_f.c \
        buffer_detach_f.c \
        cancel_f.c \
        cart_coords_f.c \
        cart_create_f.c \
        cartdim_get_f.c \
        cart_get_f.c \
        cart_map_f.c \
        cart_rank_f.c \
        cart_shift_f.c \
        cart_sub_f.c \
        close_port_f.c \
        comm_accept_f.c \
        comm_call_errhandler_f.c \
        comm_compare_f.c \
        comm_connect_f.c \
        comm_create_errhandler_f.c \
        comm_create_f.c \
        comm_create_keyval_f.c \
        comm_delete_attr_f.c \
        comm_disconnect_f.c \
        comm_dup_f.c \
        comm_free_f.c \
        comm_free_keyval_f.c \
        comm_get_attr_f.c \
        comm_get_errhandler_f.c \
        comm_get_name_f.c \
        comm_get_parent_f.c \
        comm_group_f.c \
        comm_join_f.c \
        comm_rank_f.c \
        comm_remote_group_f.c \
        comm_remote_size_f.c \
        comm_set_attr_f.c \
        comm_set_errhandler_f.c \
        comm_set_name_f.c \
        comm_size_f.c \
        comm_spawn_f.c \
        comm_spawn_multiple_f.c \
        comm_split_f.c \
        comm_test_inter_f.c \
        dims_create_f.c \
        errhandler_create_f.c \
        errhandler_free_f.c \
        errhandler_get_f.c \
        errhandler_set_f.c \
        error_class_f.c \
        error_string_f.c \
        exscan_f.c \
        finalized_f.c \
        finalize_f.c \
        free_mem_f.c \
        gather_f.c \
        gatherv_f.c \
        get_address_f.c \
        get_count_f.c \
        get_elements_f.c \
        get_processor_name_f.c \
        get_version_f.c \
        graph_create_f.c \
        graphdims_get_f.c \
        graph_get_f.c \
        graph_map_f.c \
        graph_neighbors_count_f.c \
        graph_neighbors_f.c \
        grequest_complete_f.c \
        grequest_start_f.c \
        group_compare_f.c \
        group_difference_f.c \
        group_excl_f.c \
        group_free_f.c \
        group_incl_f.c \
        group_intersection_f.c \
        group_range_excl_f.c \
        group_range_incl_f.c \
        group_rank_f.c \
        group_size_f.c \
        group_translate_ranks_f.c \
        group_union_f.c \
        ibsend_f.c \
        info_create_f.c \
        info_delete_f.c \
        info_dup_f.c \
        info_free_f.c \
        info_get_f.c \
        info_get_nkeys_f.c \
        info_get_nthkey_f.c \
        info_get_valuelen_f.c \
        info_set_f.c \
        init_f.c \
        initialized_f.c \
        init_thread_f.c \
        intercomm_create_f.c \
        intercomm_merge_f.c \
        iprobe_f.c \
        irecv_f.c \
        irsend_f.c \
        isend_f.c \
        issend_f.c \
        is_thread_main_f.c \
        keyval_create_f.c \
        keyval_free_f.c \
        lookup_name_f.c \
        op_commutative_f.c \
        op_create_f.c \
        open_port_f.c \
        op_free_f.c \
        pack_external_f.c \
        pack_external_size_f.c \
        pack_f.c \
        pack_size_f.c \
        pcontrol_f.c \
        probe_f.c \
        publish_name_f.c \
        query_thread_f.c \
        recv_f.c \
        recv_init_f.c \
        reduce_f.c \
        reduce_local_f.c \
        reduce_scatter_f.c \
        request_free_f.c \
        request_get_status_f.c \
        rsend_f.c \
        rsend_init_f.c \
        scan_f.c \
        scatter_f.c \
        scatterv_f.c \
        send_f.c \
        send_init_f.c \
        sendrecv_f.c \
        sendrecv_replace_f.c \
        ssend_f.c \
        ssend_init_f.c \
        startall_f.c \
        start_f.c \
        status_set_cancelled_f.c \
        status_set_elements_f.c \
        testall_f.c \
        testany_f.c \
        test_cancelled_f.c \
        test_f.c \
        testsome_f.c \
        topo_test_f.c \
        type_commit_f.c \
        type_contiguous_f.c \
        type_create_darray_f.c \
        type_create_f90_complex_f.c \
        type_create_f90_integer_f.c \
        type_create_f90_real_f.c \
        type_create_hindexed_f.c \
        type_create_hvector_f.c \
        type_create_indexed_block_f.c \
        type_create_keyval_f.c \
        type_create_resized_f.c \
        type_create_struct_f.c \
        type_create_subarray_f.c \
        type_delete_attr_f.c \
        type_dup_f.c \
        type_extent_f.c \
        type_free_f.c \
        type_free_keyval_f.c \
        type_get_attr_f.c \
        type_get_contents_f.c \
        type_get_envelope_f.c \
        type_get_extent_f.c \
        type_get_name_f.c \
        type_get_true_extent_f.c \
        type_hindexed_f.c \
        type_hvector_f.c \
        type_indexed_f.c \
        type_lb_f.c \
        type_match_size_f.c \
        type_set_attr_f.c \
        type_set_name_f.c \
        type_size_f.c \
        type_struct_f.c \
        type_ub_f.c \
        type_vector_f.c \
        unpack_external_f.c \
        unpack_f.c \
        unpublish_name_f.c \
        waitall_f.c \
        waitany_f.c \
        wait_f.c \
        waitsome_f.c \
        wtick_f.c \
        wtime_f.c \
        accumulate_f.c \
        get_f.c \
        put_f.c \
        win_call_errhandler_f.c \
        win_complete_f.c \
        win_create_errhandler_f.c \
        win_create_f.c \
        win_create_keyval_f.c \
        win_delete_attr_f.c \
        win_fence_f.c \
        win_free_f.c \
        win_free_keyval_f.c \
        win_get_attr_f.c \
        win_get_errhandler_f.c \
        win_get_group_f.c \
        win_get_name_f.c \
        win_lock_f.c \
        win_post_f.c \
        win_set_attr_f.c \
        win_set_errhandler_f.c \
        win_set_name_f.c \
        win_start_f.c \
        win_test_f.c \
        win_unlock_f.c \
        win_wait_f.c

if OMPI_PROVIDE_MPI_FILE_INTERFACE
libmpi_f77_la_SOURCES += \
        file_call_errhandler_f.c \
        file_close_f.c \
        file_create_errhandler_f.c \
        file_delete_f.c \
        file_get_amode_f.c \
        file_get_atomicity_f.c \
        file_get_byte_offset_f.c \
        file_get_errhandler_f.c \
        file_get_group_f.c \
        file_get_info_f.c \
        file_get_position_f.c \
        file_get_position_shared_f.c \
        file_get_size_f.c \
        file_get_type_extent_f.c \
        file_get_view_f.c \
        file_iread_at_f.c \
        file_iread_f.c \
        file_iread_shared_f.c \
        file_iwrite_at_f.c \
        file_iwrite_f.c \
        file_iwrite_shared_f.c \
        file_open_f.c \
        file_preallocate_f.c \
        file_read_all_begin_f.c \
        file_read_all_end_f.c \
        file_read_all_f.c \
        file_read_at_all_begin_f.c \
        file_read_at_all_end_f.c \
        file_read_at_all_f.c \
        file_read_at_f.c \
        file_read_f.c \
        file_read_ordered_begin_f.c \
        file_read_ordered_end_f.c \
        file_read_ordered_f.c \
        file_read_shared_f.c \
        file_seek_f.c \
        file_seek_shared_f.c \
        file_set_atomicity_f.c \
        file_set_errhandler_f.c \
        file_set_info_f.c \
        file_set_size_f.c \
        file_set_view_f.c \
        file_sync_f.c \
        file_write_all_begin_f.c \
        file_write_all_end_f.c \
        file_write_all_f.c \
        file_write_at_all_begin_f.c \
        file_write_at_all_end_f.c \
        file_write_at_all_f.c \
        file_write_at_f.c \
        file_write_f.c \
        file_write_ordered_begin_f.c \
        file_write_ordered_end_f.c \
        file_write_ordered_f.c \
        file_write_shared_f.c \
        register_datarep_f.c
endif
endif

#
# Conditionally install the header files
#

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mpi/f77
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
