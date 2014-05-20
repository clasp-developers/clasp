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

SUBDIRS = profile

#
# OMPI_PRPOFILING_DEFINES flag s enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory,
# we need it to be 0
#

AM_CPPFLAGS     = -DOMPI_PROFILING_DEFINES=0

#
# The top directory always builds MPI_* bindings. The bottom directory
# always builds PMPI_* bindings. The cases where the top directory
# needs to be built are:
#
# 1. When profiling is disabled.
# 2. When profiling is enabled but weak symbol support is absent.
#

noinst_LTLIBRARIES = libmpi_c.la
if WANT_MPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_c_mpi.la
endif

headers = bindings.h

#
# libmpi_c.la is always build because it contains some non-profilied
# functions.
#

libmpi_c_la_SOURCES = \
        attr_fn.c

#
# libmpi_c_mpi.la is only built in some cases (see above)
#

libmpi_c_mpi_la_SOURCES = \
        abort.c \
        add_error_class.c \
        add_error_code.c \
        add_error_string.c \
        address.c \
        allgather.c \
        allgatherv.c \
        alloc_mem.c \
        allreduce.c \
        alltoall.c \
        alltoallv.c \
        alltoallw.c \
        attr_delete.c \
        attr_get.c \
        attr_put.c \
        barrier.c \
        bcast.c \
        bsend.c \
        bsend_init.c \
        buffer_attach.c \
        buffer_detach.c \
        cancel.c \
        cart_coords.c \
        cart_create.c \
        cartdim_get.c \
        cart_get.c \
        cart_map.c \
        cart_rank.c \
        cart_shift.c \
        cart_sub.c \
        close_port.c \
        comm_accept.c \
        comm_c2f.c \
        comm_call_errhandler.c \
        comm_compare.c \
        comm_connect.c \
        comm_create.c \
        comm_create_errhandler.c \
        comm_create_keyval.c \
        comm_delete_attr.c \
        comm_disconnect.c \
        comm_dup.c \
        comm_f2c.c \
        comm_free.c \
        comm_free_keyval.c \
        comm_get_attr.c \
        comm_get_errhandler.c \
        comm_get_name.c \
        comm_get_parent.c \
        comm_group.c \
        comm_join.c \
        comm_rank.c \
        comm_remote_group.c \
        comm_remote_size.c \
        comm_set_attr.c \
        comm_set_errhandler.c \
        comm_set_name.c \
        comm_size.c \
        comm_spawn.c \
        comm_spawn_multiple.c \
        comm_split.c \
        comm_test_inter.c \
        dims_create.c \
        errhandler_c2f.c \
        errhandler_create.c \
        errhandler_f2c.c \
        errhandler_free.c \
        errhandler_get.c \
        errhandler_set.c \
        error_class.c \
        error_string.c \
        exscan.c \
        finalize.c \
        finalized.c \
        free_mem.c \
        gather.c \
        gatherv.c \
        get_address.c \
        get_count.c \
        get_elements.c \
        get_processor_name.c \
        get_version.c \
        graph_create.c \
        graph_get.c \
        graph_map.c \
        graph_neighbors_count.c \
        graph_neighbors.c \
        graphdims_get.c \
        grequest_complete.c \
        grequest_start.c \
        group_c2f.c \
        group_compare.c \
        group_difference.c \
        group_excl.c \
        group_f2c.c \
        group_free.c \
        group_incl.c \
        group_intersection.c \
        group_range_excl.c \
        group_range_incl.c \
        group_rank.c \
        group_size.c \
        group_translate_ranks.c \
        group_union.c \
        ibsend.c \
        info_c2f.c \
        info_create.c \
        info_delete.c \
        info_dup.c \
        info_f2c.c \
        info_free.c \
        info_get.c \
        info_get_nkeys.c \
        info_get_nthkey.c \
        info_get_valuelen.c \
        info_set.c \
        init.c \
        init_thread.c \
        initialized.c \
        intercomm_create.c \
        intercomm_merge.c \
        iprobe.c \
        irecv.c \
        irsend.c \
        is_thread_main.c \
        isend.c \
        issend.c \
        keyval_create.c \
        keyval_free.c \
        lookup_name.c \
        op_c2f.c \
        op_commutative.c \
        op_create.c \
        op_f2c.c \
        op_free.c \
        open_port.c \
        pack_external.c \
        pack_external_size.c \
        pack.c \
        pack_size.c \
        pcontrol.c \
        probe.c \
        publish_name.c \
        query_thread.c \
        recv_init.c \
        recv.c \
        reduce.c \
        reduce_local.c \
        reduce_scatter.c \
        request_c2f.c \
        request_f2c.c \
        request_free.c \
        request_get_status.c \
        rsend_init.c \
        rsend.c \
        scan.c \
        scatter.c \
        scatterv.c \
        send.c \
        send_init.c \
        sendrecv.c \
        sendrecv_replace.c \
        ssend_init.c \
        ssend.c \
        start.c \
        startall.c \
        status_c2f.c \
        status_f2c.c \
        status_set_cancelled.c \
        status_set_elements.c \
        testall.c \
        testany.c \
        test.c \
        test_cancelled.c \
        testsome.c \
        topo_test.c \
        type_c2f.c \
        type_commit.c \
        type_contiguous.c \
        type_create_darray.c \
        type_create_f90_complex.c \
        type_create_f90_integer.c \
        type_create_f90_real.c \
        type_create_hindexed.c \
        type_create_hvector.c \
        type_create_indexed_block.c \
        type_create_keyval.c \
        type_create_resized.c \
        type_create_struct.c \
        type_create_subarray.c \
        type_delete_attr.c \
        type_dup.c \
        type_extent.c \
        type_f2c.c \
        type_free.c \
        type_free_keyval.c \
        type_get_attr.c \
        type_get_contents.c \
        type_get_envelope.c \
        type_get_extent.c \
        type_get_name.c \
        type_get_true_extent.c \
        type_hindexed.c \
        type_hvector.c \
        type_indexed.c \
        type_lb.c \
        type_match_size.c \
        type_set_attr.c \
        type_set_name.c \
        type_size.c \
        type_struct.c \
        type_ub.c \
        type_vector.c \
        unpack_external.c \
        unpack.c \
        unpublish_name.c \
        wait.c \
        waitall.c \
        waitany.c \
        waitsome.c \
	wtime.c \
	wtick.c \
        accumulate.c \
        get.c \
        put.c \
        win_c2f.c \
        win_call_errhandler.c \
        win_complete.c  \
        win_create_errhandler.c \
        win_create_keyval.c \
        win_create.c \
        win_delete_attr.c \
        win_f2c.c \
        win_fence.c \
        win_free_keyval.c \
        win_free.c \
        win_get_attr.c \
        win_get_errhandler.c \
        win_get_group.c \
        win_get_name.c  \
        win_lock.c \
        win_post.c \
        win_set_attr.c \
        win_set_errhandler.c \
        win_set_name.c \
        win_start.c \
        win_test.c \
        win_unlock.c \
        win_wait.c

if OMPI_PROVIDE_MPI_FILE_INTERFACE
libmpi_c_mpi_la_SOURCES += \
        file_c2f.c \
        file_call_errhandler.c \
        file_close.c \
        file_create_errhandler.c \
        file_delete.c \
        file_f2c.c \
        file_get_amode.c \
        file_get_atomicity.c \
        file_get_byte_offset.c \
        file_get_errhandler.c \
        file_get_group.c \
        file_get_info.c \
        file_get_position.c \
        file_get_position_shared.c \
        file_get_size.c \
        file_get_type_extent.c \
        file_get_view.c \
        file_iread_at.c \
        file_iread.c \
        file_iread_shared.c \
        file_iwrite_at.c \
        file_iwrite.c \
        file_iwrite_shared.c \
        file_open.c \
        file_preallocate.c \
        file_read_all_begin.c \
        file_read_all.c \
        file_read_all_end.c \
        file_read_at_all_begin.c \
        file_read_at_all.c \
        file_read_at_all_end.c \
        file_read_at.c \
        file_read.c \
        file_read_ordered_begin.c \
        file_read_ordered.c \
        file_read_ordered_end.c \
        file_read_shared.c \
        file_seek.c \
        file_seek_shared.c \
        file_set_atomicity.c \
        file_set_errhandler.c \
        file_set_info.c \
        file_set_size.c \
        file_set_view.c \
        file_sync.c \
        file_write_all_begin.c \
        file_write_all.c \
        file_write_all_end.c \
        file_write_at_all_begin.c \
        file_write_at_all.c \
        file_write_at_all_end.c \
        file_write_at.c \
        file_write.c \
        file_write_ordered_begin.c \
        file_write_ordered.c \
        file_write_ordered_end.c \
        file_write_shared.c \
        register_datarep.c
endif

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mpi/c
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
