/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_ompreg.h"

#include "vt_defs.h"
#include "vt_trc.h"

#include <stdlib.h>

int vt_omp_regid[VT__OMP_REGID_NUM];

void vt_omp_register()
{
  uint32_t fid;

  fid = vt_def_scl_file(VT_CURRENT_THREAD, "OpenMP");

  vt_omp_regid[VT__OMP_DESTROY_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_destroy_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_DESTROY_NEST_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_destroy_nest_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_GET_DYNAMIC] =
    vt_def_region(VT_CURRENT_THREAD, "omp_get_dynamic", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_GET_MAX_THREADS] =
    vt_def_region(VT_CURRENT_THREAD, "omp_get_max_threads", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_GET_NESTED] =
    vt_def_region(VT_CURRENT_THREAD, "omp_get_nested", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_GET_NUM_PROCS] =
    vt_def_region(VT_CURRENT_THREAD, "omp_get_num_procs", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_GET_NUM_THREADS] =
    vt_def_region(VT_CURRENT_THREAD, "omp_get_num_threads", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_GET_THREAD_NUM] =
    vt_def_region(VT_CURRENT_THREAD, "omp_get_thread_num", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_IN_PARALLEL] =
    vt_def_region(VT_CURRENT_THREAD, "omp_in_parallel", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_INIT_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_init_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_INIT_NEST_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_init_nest_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_SET_DYNAMIC] =
    vt_def_region(VT_CURRENT_THREAD, "omp_set_dynamic", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_SET_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_set_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_SET_NEST_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_set_nest_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_SET_NESTED] =
    vt_def_region(VT_CURRENT_THREAD, "omp_set_nested", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_SET_NUM_THREADS] =
    vt_def_region(VT_CURRENT_THREAD, "omp_set_num_threads", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_TEST_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_test_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_TEST_NEST_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_test_nest_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_UNSET_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_unset_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);

  vt_omp_regid[VT__OMP_UNSET_NEST_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "omp_unset_nest_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_FUNCTION);
}


