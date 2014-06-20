%{ /* -*- C -*- */
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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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

#include "btl_openib_lex.h"

BEGIN_C_DECLS

/*
 * local functions
 */
static int finish_parsing(void) ;
static int btl_openib_ini_yywrap(void);

END_C_DECLS

/*
 * global variables
 */
int btl_openib_ini_yynewlines = 1;
bool btl_openib_ini_parse_done = false;
char *btl_openib_ini_string = NULL;

#define yyterminate() \
  return finish_parsing()

%}

WHITE       [\f\t\v ]
CHAR        [A-Za-z0-9_\-\.]
NAME_CHAR   [A-Za-z0-9_\-\.\\\/]

%x comment
%x section_name
%x section_end
%x value

%%

{WHITE}*\n          { ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }
#.*\n               { ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }
"//".*\n            { ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }

"/*"                { BEGIN(comment);
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n         { ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_NEWLINE; } 
<comment>"*"+"/"    { BEGIN(INITIAL); /* Done with block comment */
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }

{WHITE}*\[{WHITE}*  { BEGIN(section_name); }
<section_name>({NAME_CHAR}|{WHITE})*{NAME_CHAR}/{WHITE}*\] {
                      BEGIN(section_end); 
                      return BTL_OPENIB_INI_PARSE_SECTION; }
<section_name>\n    { ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_ERROR; }
<section_name>.     { return BTL_OPENIB_INI_PARSE_ERROR; }
<section_end>{WHITE}*\]{WHITE}*\n { BEGIN(INITIAL);
                      ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }

{WHITE}*"="{WHITE}* { BEGIN(value); 
                      return BTL_OPENIB_INI_PARSE_EQUAL; }
{WHITE}+            ; /* whitespace */
{CHAR}+             { return BTL_OPENIB_INI_PARSE_SINGLE_WORD; }

<value>{WHITE}*\n   { BEGIN(INITIAL);
                      ++btl_openib_ini_yynewlines;
                      return BTL_OPENIB_INI_PARSE_NEWLINE; }
<value>[^\n]*[^\t \n]/[\t ]* { 
                      return BTL_OPENIB_INI_PARSE_VALUE; }

.                   { return BTL_OPENIB_INI_PARSE_ERROR; }

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


static int btl_openib_ini_yywrap(void)
{
    btl_openib_ini_parse_done = true;
    return 1;
}


/*
 * Ensure that we have a valid yybuffer to use.  Specifically, if this
 * scanner is invoked a second time, finish_parsing() (above) will
 * have been executed, and the current buffer will have been freed.
 * Flex doesn't recognize this fact because as far as it's concerned,
 * its internal state was already initialized, so it thinks it should
 * have a valid buffer.  Hence, here we ensure to give it a valid
 * buffer.
 */
int btl_openib_ini_init_buffer(FILE *file)
{
    YY_BUFFER_STATE buf = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(buf);

    return 0;
}
