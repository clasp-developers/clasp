/*   
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/win/win.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"

#include <stdlib.h>

#define GAP_CHECK(NAME, BASE, F1, F2, CGAP) {      \
   offset = (size_t)&BASE.F1 - (size_t)&BASE;       \
   exp_offset = ((size_t)&BASE.F2 - (size_t)&BASE) + sizeof(BASE.F2);	\
   printf(NAME" = %lu, %lu ", offset, sizeof(BASE.F1)); \
   if (CGAP && offset != exp_offset) printf("***"); \
   printf("\n"); \
   }


int main(int argc, char **argv) {
    ompi_communicator_t test_comm;
    ompi_group_t test_group;
    ompi_request_t test_req;
    ompi_op_t test_op;
    ompi_win_t test_win;
    ompi_info_t test_info;
    ompi_file_t test_file;
    size_t exp_offset, offset;

    /* Test Predefined communicator sizes */
    printf("ompi_predefined_communicator_t = %lu bytes\n", sizeof(ompi_predefined_communicator_t));
    printf("ompi_communicator_t = %lu bytes\n", sizeof(ompi_communicator_t));
    GAP_CHECK("c_base", test_comm, c_base, c_base, 0);
    GAP_CHECK("c_lock", test_comm, c_lock, c_base, 1);
    GAP_CHECK("c_name", test_comm, c_name, c_lock, 1);
    GAP_CHECK("c_contextid", test_comm, c_contextid, c_name, 1);
    GAP_CHECK("c_my_rank", test_comm, c_my_rank, c_contextid, 1);
    GAP_CHECK("c_flags", test_comm, c_flags, c_my_rank, 1);
    GAP_CHECK("c_id_available", test_comm, c_id_available, c_flags, 1);
    GAP_CHECK("c_id_start_index", test_comm, c_id_start_index, c_id_available,  1);
    GAP_CHECK("c_remote_group", test_comm, c_remote_group, c_local_group, 1);
    GAP_CHECK("c_local_comm", test_comm, c_local_comm,  c_remote_group,  1);
    GAP_CHECK("c_keyhash", test_comm, c_keyhash, c_local_comm,  1);
    GAP_CHECK("c_cube_dim", test_comm, c_cube_dim, c_keyhash,  1);
    GAP_CHECK("c_topo_component", test_comm, c_topo_component, c_cube_dim,  1);
    GAP_CHECK("c_topo", test_comm, c_topo, c_topo_component,  1);
    GAP_CHECK("c_topo_comm", test_comm, c_topo_comm, c_topo,  1);
    GAP_CHECK("c_topo_module", test_comm, c_topo_module, c_topo_comm,  1);
    GAP_CHECK("c_f_to_c_index", test_comm, c_f_to_c_index, c_topo_module, 1);
#ifdef OMPI_WANT_PERUSE
    GAP_CHECK("c_peruse_handles", test_comm, c_peruse_handles, c_f_to_c_index, 1);
    GAP_CHECK("error_handler", test_comm, error_handler, c_peruse_handles, 1);
#else
    GAP_CHECK("error_handler", test_comm, error_handler, c_f_to_c_index, 1);
#endif
    GAP_CHECK("errhandler_type", test_comm, errhandler_type, error_handler, 1);
    GAP_CHECK("c_pml_comm", test_comm, c_pml_comm, errhandler_type, 1);
    GAP_CHECK("c_coll", test_comm, c_coll, c_pml_comm, 1);

    /* Test Predefined group sizes */
    printf("=============================================\n");
    printf("ompi_predefined_group_t = %lu bytes\n", sizeof(ompi_predefined_group_t));
    printf("ompi_group_t = %lu bytes\n", sizeof(ompi_group_t));
    GAP_CHECK("grp_proc_count", test_group, grp_proc_count, grp_proc_count, 0);
    GAP_CHECK("grp_my_rank", test_group, grp_my_rank, grp_proc_count, 1);
    GAP_CHECK("grp_f_to_c_index", test_group, grp_f_to_c_index, grp_my_rank, 1);
    GAP_CHECK("grp_proc_pointers", test_group, grp_proc_pointers, grp_f_to_c_index, 1);
    GAP_CHECK("grp_flags", test_group, grp_flags, grp_proc_pointers, 1);
    GAP_CHECK("grp_parent_group_ptr", test_group, grp_parent_group_ptr, grp_flags, 1);

    /* Test Predefined request sizes */
    printf("=============================================\n");
    printf("ompi_predefined_request_t = %lu bytes\n", sizeof(ompi_predefined_request_t));
    printf("ompi_request_t = %lu bytes\n", sizeof(ompi_request_t));
    GAP_CHECK("super", test_req, super, super, 0);
    GAP_CHECK("req_type", test_req, req_type, super, 1);
    GAP_CHECK("req_status", test_req, req_status, req_type, 1);
    GAP_CHECK("req_complete", test_req, req_complete, req_status, 1);
    GAP_CHECK("req_state", test_req, req_state, req_complete, 1);
    GAP_CHECK("req_persistent", test_req, req_persistent, req_state, 1);
    GAP_CHECK("req_f_to_c_index", test_req, req_f_to_c_index, req_persistent, 1);
    GAP_CHECK("req_free", test_req, req_free, req_f_to_c_index, 1);

    /* Test Predefined op sizes */
    printf("=============================================\n");
    printf("ompi_predefined_op_t = %lu bytes\n", sizeof(ompi_predefined_op_t));
    printf("ompi_op_t = %lu bytes\n", sizeof(ompi_op_t));
    GAP_CHECK("super", test_op, super, super, 0)
    GAP_CHECK("o_name", test_op, o_name, super, 1)
    GAP_CHECK("o_flags", test_op, o_flags, o_name, 1)
    GAP_CHECK("o_f_to_c_index", test_op, o_f_to_c_index, o_flags, 1)
    GAP_CHECK("o_func", test_op, o_func, o_f_to_c_index, 1)
    GAP_CHECK("o_3buff_instrinsic", test_op, o_3buff_intrinsic, o_func, 1)

    /* Test Predefined datatype sizes */
    printf("=============================================\n");
    printf("ompi_predefined_datatype_t = %lu bytes\n", sizeof(ompi_predefined_datatype_t));
    printf("ompi_datatype_t = %lu bytes\n", sizeof(ompi_datatype_t));

    /* Test Predefined win sizes */
    printf("=============================================\n");
    printf("ompi_predefined_win_t = %lu bytes\n", sizeof(ompi_predefined_win_t));
    printf("ompi_win_t = %lu bytes\n", sizeof(ompi_win_t));
    GAP_CHECK("w_base", test_win, w_base, w_base, 0);
    GAP_CHECK("w_lock", test_win, w_lock, w_base, 1);
    GAP_CHECK("w_name", test_win, w_name, w_lock, 1);
    GAP_CHECK("w_group", test_win, w_group, w_name, 1);
    GAP_CHECK("w_flags", test_win, w_flags, w_group, 1);
    GAP_CHECK("w_keyhash", test_win, w_keyhash, w_flags, 1);
    GAP_CHECK("w_f_to_c_index", test_win, w_f_to_c_index, w_keyhash, 1);
    GAP_CHECK("error_handler", test_win, error_handler, w_f_to_c_index, 1);
    GAP_CHECK("errhandler_type", test_win, errhandler_type, error_handler, 1);
    GAP_CHECK("w_disp_unit", test_win, w_disp_unit, errhandler_type, 1);
    GAP_CHECK("w_baseptr", test_win, w_baseptr, w_disp_unit, 1);
    GAP_CHECK("w_size", test_win, w_size, w_baseptr, 1);
    GAP_CHECK("w_mode", test_win, w_mode, w_size, 1);
    GAP_CHECK("w_osc_module", test_win, w_osc_module, w_size, 1);

    /* Test Predefined info sizes */
    printf("=============================================\n");
    printf("ompi_predefined_info_t = %lu bytes\n", sizeof(ompi_predefined_info_t));
    printf("ompi_info_t = %lu bytes\n", sizeof(ompi_info_t));
    GAP_CHECK("super", test_info, super, super, 0);
    GAP_CHECK("i_f_to_c_index", test_info, i_f_to_c_index, super, 1);
    GAP_CHECK("i_lock", test_info, i_lock, i_f_to_c_index, 1);
    GAP_CHECK("i_freed", test_info, i_freed, i_lock, 1);

    /* Test Predefined file sizes */
    printf("=============================================\n");
    printf("ompi_predefined_file_t = %lu bytes\n", sizeof(ompi_predefined_file_t));
    printf("ompi_file_t = %lu bytes\n", sizeof(ompi_file_t));
    GAP_CHECK("super", test_file, super, super, 0);
    GAP_CHECK("f_comm", test_file, f_comm, super, 1);
    GAP_CHECK("f_filename", test_file, f_filename, f_comm, 1);
    GAP_CHECK("f_amode", test_file, f_amode,  f_filename, 1);
    GAP_CHECK("f_info", test_file, f_info, f_amode,  1);
    GAP_CHECK("f_flags", test_file, f_flags, f_info,  1);
    GAP_CHECK("f_f_to_c_index", test_file, f_f_to_c_index, f_flags, 1);
    GAP_CHECK("error_handler", test_file, error_handler, f_f_to_c_index, 1);
    GAP_CHECK("errhandler_type", test_file, errhandler_type, error_handler, 1);
    GAP_CHECK("f_io_version", test_file, f_io_version, errhandler_type, 1);
    GAP_CHECK("f_io_selected_component", test_file, f_io_selected_component, f_io_version, 1);
    GAP_CHECK("f_io_selected_module", test_file, f_io_selected_module, f_io_selected_component, 1);
    GAP_CHECK("f_io_selected_data", test_file, f_io_selected_data, f_io_selected_module, 1);
    
    return 0;
}
