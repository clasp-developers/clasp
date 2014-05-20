
#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/keyval/keyval_lex.h"
#include "opal/util/output.h"
#include "opal/threads/mutex.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

static const char *keyval_filename;
static opal_keyval_parse_fn_t keyval_callback;
static char *key_buffer = NULL;
static size_t key_buffer_len = 0;
static opal_mutex_t keyval_mutex;

static int parse_line(void);
static void parse_error(int num);

int opal_util_keyval_parse_init(void)
{
    OBJ_CONSTRUCT(&keyval_mutex, opal_mutex_t);

    return OPAL_SUCCESS;
}


int
opal_util_keyval_parse_finalize(void)
{
    if (NULL != key_buffer) free(key_buffer);

    OBJ_DESTRUCT(&keyval_mutex);

    return OPAL_SUCCESS;
}


int
opal_util_keyval_parse(const char *filename,
                       opal_keyval_parse_fn_t callback)
{
    int val;
    int ret = OPAL_SUCCESS;;

    OPAL_THREAD_LOCK(&keyval_mutex);

    keyval_filename = filename;
    keyval_callback = callback;

    /* Open the opal */
    opal_util_keyval_yyin = fopen(keyval_filename, "r");
    if (NULL == opal_util_keyval_yyin) {
        ret = OPAL_ERR_NOT_FOUND;
        goto cleanup;
    }

    opal_util_keyval_parse_done = false;
    opal_util_keyval_yynewlines = 1;
    opal_util_keyval_init_buffer(opal_util_keyval_yyin);
    while (!opal_util_keyval_parse_done) {
        val = opal_util_keyval_yylex();
        switch (val) {
        case OPAL_UTIL_KEYVAL_PARSE_DONE:
            /* This will also set opal_util_keyval_parse_done to true, so just
               break here */
            break;

        case OPAL_UTIL_KEYVAL_PARSE_NEWLINE:
            /* blank line!  ignore it */
            break;

        case OPAL_UTIL_KEYVAL_PARSE_SINGLE_WORD:
            parse_line();
            break;

        default:
            /* anything else is an error */
            parse_error(1);
            break;
        }
    }
    fclose(opal_util_keyval_yyin);

cleanup:
    OPAL_THREAD_UNLOCK(&keyval_mutex);
    return ret;
}



static int parse_line(void)
{
    int val;

    /* Save the name name */
    if (key_buffer_len < strlen(opal_util_keyval_yytext) + 1) {
        char *tmp;
        key_buffer_len = strlen(opal_util_keyval_yytext) + 1;
        tmp = (char*)realloc(key_buffer, key_buffer_len);
        if (NULL == tmp) {
            free(key_buffer);
            key_buffer_len = 0;
            key_buffer = NULL;
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }
        key_buffer = tmp;
    }

    strncpy(key_buffer, opal_util_keyval_yytext, key_buffer_len);

    /* The first thing we have to see is an "=" */

    val = opal_util_keyval_yylex();
    if (opal_util_keyval_parse_done || OPAL_UTIL_KEYVAL_PARSE_EQUAL != val) {
        parse_error(2);
        return OPAL_ERROR;
    }

    /* Next we get the value */

    val = opal_util_keyval_yylex();
    if (OPAL_UTIL_KEYVAL_PARSE_SINGLE_WORD == val ||
        OPAL_UTIL_KEYVAL_PARSE_VALUE == val) {
        keyval_callback(key_buffer, opal_util_keyval_yytext);

        /* Now we need to see the newline */

        val = opal_util_keyval_yylex();
        if (OPAL_UTIL_KEYVAL_PARSE_NEWLINE == val ||
            OPAL_UTIL_KEYVAL_PARSE_DONE == val) {
            return OPAL_SUCCESS;
        }
    }

    /* Did we get an EOL or EOF? */

    else if (OPAL_UTIL_KEYVAL_PARSE_DONE == val ||
             OPAL_UTIL_KEYVAL_PARSE_NEWLINE == val) {
        keyval_callback(key_buffer, NULL);
        return OPAL_SUCCESS;
    }

    /* Nope -- we got something unexpected.  Bonk! */
    parse_error(3);
    return OPAL_ERROR;
}


static void parse_error(int num)
{
    /* JMS need better error/warning message here */
    opal_output(0, "keyval parser: error %d reading file %s at line %d:\n  %s\n",
                num, keyval_filename, opal_util_keyval_yynewlines, opal_util_keyval_yytext);
}
