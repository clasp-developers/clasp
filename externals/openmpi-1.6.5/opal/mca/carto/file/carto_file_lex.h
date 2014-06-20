/* -*- C -*-
 *
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

#ifndef CARTO_FILE_LEX_LEX_H_
#define CARTO_FILE_LEX_LEX_H_

#include "opal_config.h"

#ifdef malloc
#undef malloc
#endif
#ifdef realloc
#undef realloc
#endif
#ifdef free
#undef free
#endif

#include <stdio.h>

typedef union {
    int ival;
    char* sval;
} orte_rds_value_t;

extern int   carto_file_lex(void);
extern FILE *carto_file_in;
extern int   carto_file_line;
extern bool  carto_file_done;
extern orte_rds_value_t  carto_file_value;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1
#define YY_SKIP_YYWRAP 1

#define OPAL_CARTO_FILE_NEWLINE                0
#define OPAL_CARTO_FILE_ERROR                  1
#define OPAL_CARTO_FILE_NODE_DECELERATION       2
#define OPAL_CARTO_FILE_CONNECTION_DECELERATION 3
#define OPAL_CARTO_FILE_BIDIRECTION_CONNECTION  4
#define OPAL_CARTO_FILE_INT                    5
#define OPAL_CARTO_FILE_NAME                   6
#define OPAL_CARTO_FILE_NODE_CONNECTION        7

#endif

