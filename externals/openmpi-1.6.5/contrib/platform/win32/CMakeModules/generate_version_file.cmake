
IF(${WANT_SVN})
  SET(${PROJECT_NAME}_WANT_SVN 1)
ELSE(${WANT_SVN})
  SET(${PROJECT_NAME}_WANT_SVN 0)
ENDIF(${WANT_SVN})

STRING(TOLOWER ${PROJECT_NAME} PROJECT_NAME_LOWERCASE)

FILE (WRITE "${PROJECT_BINARY_DIR}/include/${PROJECT_NAME_LOWERCASE}/version.h"
  "
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
 *
 * This file should be included by any file that needs full
 * version information for the ${PROJECT_NAME} project
 */

#ifndef ${PROJECT_NAME}_VERSIONS_H
#define ${PROJECT_NAME}_VERSIONS_H

#define ${PROJECT_NAME}_MAJOR_VERSION ${${PROJECT_NAME}_MAJOR_VERSION_STRING}
#define ${PROJECT_NAME}_MINOR_VERSION ${${PROJECT_NAME}_MINOR_VERSION_STRING}
#define ${PROJECT_NAME}_RELEASE_VERSION ${${PROJECT_NAME}_RELEASE_VERSION_STRING}
#define ${PROJECT_NAME}_GREEK_VERSION \"${${PROJECT_NAME}_GREEK_VERSION_STRING}\"
#define ${PROJECT_NAME}_WANT_SVN ${${PROJECT_NAME}_WANT_SVN}
#define ${PROJECT_NAME}_SVN_R \"${SVN_VERSION}\"
#ifdef ${PROJECT_NAME}_VERSION
/* If we included version.h, we want the real version, not the
   stripped (no-r number) verstion */
#undef ${PROJECT_NAME}_VERSION
#endif
#define ${PROJECT_NAME}_VERSION \"${${PROJECT_NAME}_VERSION_STRING}\"

#endif
")