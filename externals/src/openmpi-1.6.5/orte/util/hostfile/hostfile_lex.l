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
#include "orte_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/util/hostfile/hostfile_lex.h"

/*
 * local functions
 */
BEGIN_C_DECLS

int orte_util_hostfile_wrap(void);

END_C_DECLS

int orte_util_hostfile_wrap(void)
{
    orte_util_hostfile_done = true;
    return 1;
}

/*
 * global variables
 */
int orte_util_hostfile_line=1;
orte_hostfile_value_t orte_util_hostfile_value;
bool orte_util_hostfile_done = false;

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { orte_util_hostfile_line++; 
                     return ORTE_HOSTFILE_NEWLINE; }
#.*\n              { orte_util_hostfile_line++; 
                     return ORTE_HOSTFILE_NEWLINE; }
"//".*\n           { orte_util_hostfile_line++; 
                     return ORTE_HOSTFILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return ORTE_HOSTFILE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n        { orte_util_hostfile_line++;
                     return ORTE_HOSTFILE_NEWLINE; } 
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return ORTE_HOSTFILE_NEWLINE; }

\"[^\"]*\"         { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_QUOTED_STRING; }

{WHITE}+           ;			/* whitespace */

"="                { return ORTE_HOSTFILE_EQUAL; }

cpu                { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_CPU; }
count              { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_COUNT; }
slots              { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS; }
"slots-max"        { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
slots_max          { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
"max-slots"        { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
max_slots          { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
"cpu-max"          { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
cpu_max            { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
"max-cpu"          { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
max_cpu            { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
"count-max"        { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
count_max          { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
"max-count"        { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }
max_count          { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOTS_MAX; }

rank                { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_RANK; }
slot               { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_SLOT; }

username           { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_USERNAME; }
"user-name"        { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_USERNAME; }
"user_name"        { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_USERNAME; }

boards             { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_BOARDS; }

sockets            { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_SOCKETS_PER_BOARD; }

sockets_per_board  { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_SOCKETS_PER_BOARD; }

"sockets-per-board" { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_SOCKETS_PER_BOARD; }

cores              { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_CORES_PER_SOCKET; }

cores_per_socket   { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_CORES_PER_SOCKET; }

"cores-per-socket" { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_CORES_PER_SOCKET; }

\+n[0-9]+	   { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_RELATIVE; }
\+[eE][\:][0-9]+   { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_RELATIVE; }
\+[eE]             { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_RELATIVE; }


[0-9]+             { orte_util_hostfile_value.ival = atol(yytext); 
                     return ORTE_HOSTFILE_INT; }
%{ /* First detect hosts as standard Strings (but without ".")
    * then username@IPv4 or IPV4, then username@IPv6 or IPv6,
    * followed by username@hostname or hostname
    */
%}

[A-za-z0-9_\-,:*@]*  { orte_util_hostfile_value.sval = yytext;
                     return ORTE_HOSTFILE_STRING; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([0-9]{1,3}"."){3}[0-9]{1,3} {
                      orte_util_hostfile_value.sval = yytext;
                      return ORTE_HOSTFILE_IPV4; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([A-Fa-f0-9]{0,4}":")+[":"]*([A-Fa-f0-9]{0,4}":")+[A-Fa-f0-9]{1,4} {
                      orte_util_hostfile_value.sval = yytext;
                      return ORTE_HOSTFILE_IPV6; }

(\^?[A-Za-z0-9][A-Za-z0-9_\-]*"@")?[A-Za-z][A-Za-z0-9_\-\.]*  {
                     orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_HOSTNAME; }

.                  { orte_util_hostfile_value.sval = yytext; 
                     return ORTE_HOSTFILE_ERROR; }

%%
