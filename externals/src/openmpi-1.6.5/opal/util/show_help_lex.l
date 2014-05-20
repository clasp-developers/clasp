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

#include "opal/util/show_help_lex.h"

BEGIN_C_DECLS

/*
 * public functions
 */
extern int opal_show_help_finish_parsing(void);

/*
 * local functions
 */
static int opal_show_help_yywrap(void);

END_C_DECLS

/*
 * global variables
 */
int opal_show_help_yynewlines = 1;
bool opal_show_help_parse_done = false;

#define yyterminate() \
  return opal_show_help_finish_parsing()

%}

WHITE       [\f\t\v ]
CHAR        [A-Za-z0-9_\-\.]

%x CHOMP

%%

#.*\n               ; /* comment line */

^\[.+\]/[^\]\n]*\n { BEGIN(CHOMP); return OPAL_SHOW_HELP_PARSE_TOPIC; }

<CHOMP>.*\n { BEGIN(INITIAL); }

.*/\n { BEGIN(CHOMP); return OPAL_SHOW_HELP_PARSE_MESSAGE; }

%%


/*
 * This cleans up at the end of the parse (since, in this case, we
 * always parse the entire file) and prevents a memory leak.
 */
int opal_show_help_finish_parsing(void) 
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


static int opal_show_help_yywrap(void)
{
    opal_show_help_parse_done = true;
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
int opal_show_help_init_buffer(FILE *file)
{
    YY_BUFFER_STATE buf = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(buf);

    return 0;
}
