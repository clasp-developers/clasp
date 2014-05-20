%{ /* -*- C -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "opal_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/carto/file/carto_file_lex.h"

/*
 * local functions
 */
BEGIN_C_DECLS

int carto_file_wrap(void);

END_C_DECLS

int carto_file_wrap(void)
{
    carto_file_done = true;
    return 1;
}

/*
 * global variables
 */
int carto_file_line=1;
orte_rds_value_t carto_file_value;
bool carto_file_done = false;

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { carto_file_line++; 
                     return OPAL_CARTO_FILE_NEWLINE; }
#.*\n              { carto_file_line++; 
                     return OPAL_CARTO_FILE_NEWLINE; }
"//".*\n           { carto_file_line++; 
                     return OPAL_CARTO_FILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return OPAL_CARTO_FILE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n        { carto_file_line++;
                     return OPAL_CARTO_FILE_NEWLINE; } 
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return OPAL_CARTO_FILE_NEWLINE; }

{WHITE}+           ;			/* whitespace */




EDGE               { carto_file_value.sval = yytext;
                     return OPAL_CARTO_FILE_NODE_DECELERATION; }

BRANCH             { carto_file_value.sval = yytext;
                     return OPAL_CARTO_FILE_CONNECTION_DECELERATION; }

BRANCH_BI_DIR         { carto_file_value.sval = yytext;
                     return OPAL_CARTO_FILE_BIDIRECTION_CONNECTION; }

[0-9]              { carto_file_value.ival = atol(yytext); 
                     return OPAL_CARTO_FILE_INT; }

[A-Za-z0-9_-]*     { carto_file_value.sval = yytext;
                     return OPAL_CARTO_FILE_NAME; }


([[A-Za-z0-9_\-]*)+":"([0-9]*)  { carto_file_value.sval = yytext;
                     return OPAL_CARTO_FILE_NODE_CONNECTION; }


%%

