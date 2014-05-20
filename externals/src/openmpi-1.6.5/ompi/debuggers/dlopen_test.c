/*
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/libltdl/ltdl.h"

static int do_test(void);

int main(int argc, char *argv[])
{
#if OPAL_WANT_LIBLTDL
    return do_test();
#else
    /* If OPAL wasn't built with libltdl support, then skip this test */
    fprintf(stderr, "OPAL was not built with libltdl support; skipping\n");
    return 77;
#endif
}

static int do_test(void)
{
    FILE *fp;
    char filename[] = "./libompi_dbg_msgq";
    char full_filename[] = "./libompi_dbg_msgq.la";
    char line[1024];
    int happy;
    lt_dlhandle dlhandle;

#if OPAL_HAVE_LTDL_ADVISE
    lt_dladvise dladvise;
#endif

    /* Double check that the .la file is there that we expect; if it's
       not, skip this test. */
    fp = fopen(full_filename, "r");
    if (NULL == fp) {
        fprintf(stderr, 
                "File %s.la doesn't seem to exist; skipping this test\n",
                full_filename);
        exit(77);
    }
    /* We know the .la file is there, so read it, looking for the
       dlopen value.  If the dlopen value is '' (i.e., empty), then
       there's nothing to dlopen (i.e., OMPI was built with
       --enable-static --disable-shared, so return 77 to skip this
       test.  This is horrible, but I can't think of a better way to
       check it (since there is no good way to #define whether we have
       built statically or not...). */
    happy = 0;
    while (1) {
        if (0 == fgets(line, sizeof(line) - 1, fp)) {
            break;
        }
        if (0 == strncmp(line, "dlname=", 7)) {
            if (0 == strncmp(line + 7, "''", 2)) {
                happy = 0;
            } else {
                happy = 1;
            }
            break;
        }
    }
    fclose(fp);
    if (!happy) {
        fprintf(stderr, "No test file to dlopen (perhaps --enable-static?); skipping\n");
        exit(77);
    }

    /* Startup LT */
    if (lt_dlinit() != 0) {
        fprintf(stderr, "Failed to lt_dlinit\n");
        return 1;
    }

    printf("Trying to lt_dlopen file with dladvise_local: %s\n", filename);

#if OPAL_HAVE_LTDL_ADVISE
    if (lt_dladvise_init(&dladvise) ||
        lt_dladvise_ext(&dladvise) ||
        lt_dladvise_local(&dladvise)) {
        fprintf(stderr, "lt_dladvise failed to initialize properly\n");
        return 1;
    }
    dlhandle = lt_dlopenadvise(filename, dladvise);
    lt_dladvise_destroy(&dladvise);
#else
    dlhandle = lt_dlopenext(filename);
#endif
    if (NULL != dlhandle) {
        lt_dlclose(dlhandle);
	printf("File opened with dladvise_local, all passed\n");
        return 0;
    }

    printf("Failed to open with dladvise_local: %s\n", lt_dlerror());
    printf("Retrying with dladvise_global\n");

#if OPAL_HAVE_LTDL_ADVISE
    if (lt_dladvise_init(&dladvise) ||
        lt_dladvise_ext(&dladvise) ||
        lt_dladvise_global(&dladvise)) {
        fprintf(stderr, "lt_dladvise failed to initialize properly\n");
        return 1;
    }
    dlhandle = lt_dlopenadvise(filename, dladvise);
    lt_dladvise_destroy(&dladvise);
#else
    dlhandle = lt_dlopenext(filename);
#endif
    if (NULL != dlhandle) {
        lt_dlclose(dlhandle);
	printf("File opened with dladvise_global\n");
	return 0;
    }
    fprintf(stderr, "File failed to open with dladvise_global: %s\n", 
            lt_dlerror());

    return 2;
}
