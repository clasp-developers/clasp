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
 * Copyright (c) 2007      Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "opal/util/error.h"
#include "opal/constants.h"

#define MAX_CONVERTERS 5
#define MAX_CONVERTER_PROJECT_LEN 10

struct converter_info_t {
    int init;
    char project[MAX_CONVERTER_PROJECT_LEN];
    int err_base;
    int err_max;
    opal_err2str_fn_t converter;
};
typedef struct converter_info_t converter_info_t;

/* all default to NULL */
converter_info_t converters[MAX_CONVERTERS];

static const char *
opal_strerror_int(int errnum)
{
    int i;
    const char *ret = NULL;

    for (i = 0 ; i < MAX_CONVERTERS ; ++i) {
        if (0 != converters[i].init) {
            ret = converters[i].converter(errnum);
            if (NULL != ret) break;
        }
    }

    return ret;
}


/* caller must free string */
static char*
opal_strerror_unknown(int errnum)
{
    int i;
    char *ret;

    for (i = 0 ; i < MAX_CONVERTERS ; ++i) {
        if (0 != converters[i].init) {
            if (errnum < converters[i].err_base && 
                errnum > converters[i].err_max) {
                asprintf(&ret, "Unknown error: %d (%s error %d)",
                         errnum, converters[i].project, 
                         errnum - converters[i].err_base);
                return ret;
            }
        }
    }

    asprintf(&ret, "Unknown error: %d", errnum);

    return ret;
}


void
opal_perror(int errnum, const char *msg)
{
    const char* errmsg = opal_strerror_int(errnum);

    if (NULL != msg && errnum != OPAL_ERR_IN_ERRNO) {
        fprintf(stderr, "%s: ", msg);
    }

    if (NULL == errmsg) {
        if (errnum == OPAL_ERR_IN_ERRNO) {
            perror(msg);
        } else {
            char *ue_msg = opal_strerror_unknown(errnum);
            fprintf(stderr, "%s\n", ue_msg);
            free(ue_msg);
        }
    } else {
        fprintf(stderr, "%s\n", errmsg);
    }

    fflush(stderr);
}

/* big enough to hold long version */
#define UNKNOWN_RETBUF_LEN 50
static char unknown_retbuf[UNKNOWN_RETBUF_LEN];

const char *
opal_strerror(int errnum)
{
    const char* errmsg;

    if (errnum == OPAL_ERR_IN_ERRNO) {
        return strerror(errno);
    }

    errmsg = opal_strerror_int(errnum);

    if (NULL == errmsg) {
        char *ue_msg = opal_strerror_unknown(errnum);
        snprintf(unknown_retbuf, UNKNOWN_RETBUF_LEN, "%s", ue_msg);
        free(ue_msg);
        errno = EINVAL;
        return (const char*) unknown_retbuf;
    } else {
        return errmsg;
    }
}


int
opal_strerror_r(int errnum, char *strerrbuf, size_t buflen)
{
    const char* errmsg = opal_strerror_int(errnum);
    int ret;

    if (NULL == errmsg) {
        if (errnum == OPAL_ERR_IN_ERRNO) {
            char *tmp = strerror(errno);
            strncpy(strerrbuf, tmp, buflen);
            return OPAL_SUCCESS;
        } else {
            char *ue_msg = opal_strerror_unknown(errnum);
            ret =  snprintf(strerrbuf, buflen, "%s", ue_msg);
            free(ue_msg);
            if (ret > (int) buflen) {
                errno = ERANGE;
                return OPAL_ERR_OUT_OF_RESOURCE;
            } else {
                errno = EINVAL;
                return OPAL_SUCCESS;
            }
        }
    } else {
        ret =  snprintf(strerrbuf, buflen, "%s", errmsg);
        if (ret > (int) buflen) {
            errno = ERANGE;
            return OPAL_ERR_OUT_OF_RESOURCE;
        } else {
            return OPAL_SUCCESS;
        }
    }
}


int
opal_error_register(const char *project, int err_base, int err_max,
                    opal_err2str_fn_t converter)
{
    int i;

    for (i = 0 ; i < MAX_CONVERTERS ; ++i) {
        if (0 == converters[i].init) {
            converters[i].init = 1;
            strncpy(converters[i].project, project, MAX_CONVERTER_PROJECT_LEN);
            converters[i].project[MAX_CONVERTER_PROJECT_LEN-1] = '\0';
            converters[i].err_base = err_base;
            converters[i].err_max = err_max;
            converters[i].converter = converter;
            return OPAL_SUCCESS;
        }
    }

    return OPAL_ERR_OUT_OF_RESOURCE;
}
