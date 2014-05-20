/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#ifndef ROMIO_CONF_UNDEFS_H
#define ROMIO_CONF_UNDEFS_H

/* Need to add some undefs here so that we don't conflict with the
 * main ompi_config.h.  Arrgh.  Stupid autoconf not giving us the option
 * to not define these macros... grumble... 
 */
#if defined(PACKAGE_BUGREPORT)
#undef PACKAGE_BUGREPORT
#endif
#if defined(PACKAGE_NAME)
#undef PACKAGE_NAME
#endif
#if defined(PACKAGE_STRING)
#undef PACKAGE_STRING
#endif
#if defined(PACKAGE_TARNAME)
#undef PACKAGE_TARNAME
#endif
#if defined(PACKAGE_VERSION)
#undef PACKAGE_VERSION
#endif

#endif /* ROMIOCONF_UNDEFS_H */
