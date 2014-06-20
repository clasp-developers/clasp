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
 * Copyright (c) 2008      Voltaire. All rights reserved
 *  
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/mca/rmaps/rank_file/rmaps_rank_file_lex.h"
#include "opal/util/output.h"

BEGIN_C_DECLS

static int finish_parsing(void);
int orte_rmaps_rank_file_yywrap(void);

END_C_DECLS

#define yyterminate() \
  return finish_parsing()

/*
 * global variables
 */
int orte_rmaps_rank_file_line=1;
orte_rmaps_rank_file_value_t orte_rmaps_rank_file_value;
bool orte_rmaps_rank_file_done = false;

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { orte_rmaps_rank_file_line++; 
                     return ORTE_RANKFILE_NEWLINE; }
#.*\n              { orte_rmaps_rank_file_line++; 
                     return ORTE_RANKFILE_NEWLINE; }
"//".*\n           { orte_rmaps_rank_file_line++; 
                     return ORTE_RANKFILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return ORTE_RANKFILE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n        { orte_rmaps_rank_file_line++;
                     return ORTE_RANKFILE_NEWLINE; } 
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return ORTE_RANKFILE_NEWLINE; }

\"[^\"]*\"         { orte_rmaps_rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_QUOTED_STRING; }

{WHITE}+           ;			

"="                { return ORTE_RANKFILE_EQUAL; }

rank                { orte_rmaps_rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_RANK; }
slot               { orte_rmaps_rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_SLOT; }
slots              { orte_rmaps_rank_file_value.sval = yytext;  
                     return ORTE_RANKFILE_SLOT; }

username           { orte_rmaps_rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_USERNAME; }
"user-name"        { orte_rmaps_rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_USERNAME; }
"user_name"        { orte_rmaps_rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_USERNAME; }

[0-9]+             { orte_rmaps_rank_file_value.ival = atol(yytext); 
                     return ORTE_RANKFILE_INT; }
%{ /* First detect hosts as standard Strings (but without ".")
    * then username@IPv4 or IPV4, then username@IPv6 or IPv6,
    * followed by username@hostname or hostname
    */
%}

[A-za-z0-9_\-,:*@]*  { orte_rmaps_rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_STRING; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([0-9]{1,3}"."){3}[0-9]{1,3} {
                      orte_rmaps_rank_file_value.sval = yytext;
                      return ORTE_RANKFILE_IPV4; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([A-Fa-f0-9]{0,4}":")+[":"]*([A-Fa-f0-9]{0,4}":")+[A-Fa-f0-9]{1,4} {
                      orte_rmaps_rank_file_value.sval = yytext;
                      return ORTE_RANKFILE_IPV6; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?[A-Za-z][A-Za-z0-9_\-\.]*  {
                     orte_rmaps_rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_HOSTNAME; }

\+n[0-9]+           { orte_rmaps_rank_file_value.sval = yytext;
					 return ORTE_RANKFILE_RELATIVE; }

.                  { orte_rmaps_rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_ERROR; }

%%

/*
 * This cleans up at the end of the parse (since, in this case, we
 * always parse the entire file) and prevents a memory leak.
 */
static int finish_parsing(void) 
{
    if (NULL != YY_CURRENT_BUFFER) {
        yy_delete_buffer(YY_CURRENT_BUFFER); 
#if defined(YY_CURRENT_BUFFER_LVALUE)
        YY_CURRENT_BUFFER_LVALUE = NULL;
#else
        YY_CURRENT_BUFFER = NULL;
#endif  /* YY_CURRENT_BUFFER_LVALUE */
    }
    return YY_NULL;
}

int orte_rmaps_rank_file_wrap(void)
{
    orte_rmaps_rank_file_done = true;
    return 1;
}
