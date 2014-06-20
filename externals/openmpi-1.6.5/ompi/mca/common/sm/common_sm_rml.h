/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _COMMON_SM_RML_H_
#define _COMMON_SM_RML_H_

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/shmem/shmem.h"

#include "ompi/proc/proc.h"
#include "ompi/mca/common/sm/common_sm.h"

BEGIN_C_DECLS

/**
 * routine used to send common sm initialization information to all local
 * processes in procs.
 */
OMPI_DECLSPEC extern int
mca_common_sm_rml_info_bcast(opal_shmem_ds_t *out_ds_buf,
                             ompi_proc_t **procs,
                             size_t num_local_procs,
                             int tag,
                             bool proc0,
                             char *msg_id_str);

END_C_DECLS

#endif /* _COMMON_SM_RML_H_*/
