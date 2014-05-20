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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_OPENIB_INI_LEX_H_
#define BTL_OPENIB_INI_LEX_H_

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

BEGIN_C_DECLS

int btl_openib_ini_yylex(void);
int btl_openib_ini_init_buffer(FILE *file);

extern FILE *btl_openib_ini_yyin;
extern bool btl_openib_ini_parse_done;
extern char *btl_openib_ini_yytext;
extern int btl_openib_ini_yynewlines;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1
#define YY_SKIP_YYWRAP 1

enum {
    BTL_OPENIB_INI_PARSE_DONE,
    BTL_OPENIB_INI_PARSE_ERROR,

    BTL_OPENIB_INI_PARSE_NEWLINE,
    BTL_OPENIB_INI_PARSE_SECTION,
    BTL_OPENIB_INI_PARSE_EQUAL,
    BTL_OPENIB_INI_PARSE_SINGLE_WORD,
    BTL_OPENIB_INI_PARSE_VALUE,

    BTL_OPENIB_INI_PARSE_MAX
};

END_C_DECLS

#endif
