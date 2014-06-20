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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/communicator/communicator.h"

/*
 * This function is used to shut down a topology module 
 * on a communicator. As of now, this should do nothing
 * more than just invoke the finalize on the module which
 * was selected. There is nothing fancy which we need to
 * do as is the case with collectives.
 */ 
int mca_topo_base_comm_unselect(struct ompi_communicator_t *comm) {

   if (NULL != comm->c_topo && NULL != comm->c_topo->topo_module_finalize) {
       return comm->c_topo->topo_module_finalize(comm);
   }

   /* we fall here if there was no topolog module or the selected module 
    * did not have anything to finalize (its func pointer was NULL) */
   return OMPI_SUCCESS;
}
