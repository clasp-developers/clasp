/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file#this is a comment
# Node declaration   Node type (Free string)   Node name (Free string)
# (Reserve word)     (socket is a reserve word   (free string)
#                     for CPU socket)
#=======================================================================
  EDGE               Memory                    mem0
  EDGE               Memory                    mem1
  EDGE               Memory                    mem2
  EDGE               Memory                    mem3
#
  EDGE               socket                      socket0
  EDGE               socket                      socket1
  EDGE               socket                      socket2
  EDGE               socket                      socket3
#
  EDGE               Infiniband                mthca0
  EDGE               Infiniband                mthca1
#
  EDGE               Ethernet                  eth0
  EDGE               Ethernet                  eth1
#
#
# Connection decleration  From node   To node:weight   To node:weight   ......
# (Reserve word)          (declered   (declered        (declered
#                          above)      above)           above)
#===============================================================================================
  BRANCH              mem0        socket0:0
  BRANCH              mem3        socket3:0
#
  BRANCH              socket0       mem0:0           socket1:1           socket2:1 mthca0:1 eth0:1
  BRANCH              socket1       socket0:1        socket3:1
  BRANCH              socket2       socket1:1        socket3:1
  BRANCH              socket3       mem3:0           socket1:1           socket2:1 mthca1:1 eth1:1
#
#
  BRANCH              mthca0      socket0:1
  BRANCH              mthca1      socket3:1
#
  BRANCH              eth0        socket0:1
  BRANCH              eth1        socket3:1

#Bi-Directional connection
#
  BRANCH_BI_DIR              socket1       mem1:0
  BRANCH_BI_DIR              socket2       mem2:0
#
# end of carto file.

 *
 * The file component uses a cartograpy file to discover the
 * host cartography.
 * 
 * An example cartography file:
 * 

 * 
 * 
 * 
 * 
 * 
 */

#ifndef MCA_CARTO_FILE_H
#define MCA_CARTO_FILE_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/carto/carto.h"

BEGIN_C_DECLS

extern char *carto_file_path;

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_carto_base_component_2_0_0_t
mca_carto_file_component;


/**
 * carto query API function
 *
 * Query function for carto components.  Simply returns a priority
 * to rank it against other available carto components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
int opal_carto_file_component_query(mca_base_module_t **module, int *priority);

END_C_DECLS

#endif /* MCA_CARTO_FILE_EXPORT_H */
