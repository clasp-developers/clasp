/*
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_INSTALLDIRS_BASE_H
#define OPAL_INSTALLDIRS_BASE_H

#include "opal_config.h"

#include "opal/mca/installdirs/installdirs.h"

/*
 * Global functions for MCA overall installdirs open and close
 */
BEGIN_C_DECLS

OPAL_DECLSPEC int opal_installdirs_base_open(void);
OPAL_DECLSPEC int opal_installdirs_base_close(void);

/* Just like opal_install_dirs_expand() (see installdirs.h), but will
   also insert the value of the environment variable $OPAL_DESTDIR, if
   it exists/is set.  This function should *only* be used during the
   setup routines of installdirs. */
char * opal_install_dirs_expand_setup(const char* input);


/*
 * Globals
 */
OPAL_DECLSPEC extern opal_list_t opal_installdirs_components;

END_C_DECLS

#endif /* OPAL_BASE_INSTALLDIRS_H */
