/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#include "opal/constants.h"
#include "opal_stdint.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/shmem/base/base.h"

#include "opal/mca/shmem/windows/shmem_windows.h"

/* for tons of debug output: -mca shmem_base_verbose 70 */

/* ////////////////////////////////////////////////////////////////////////// */
/*local functions */
/* local functions */
static int
module_init(void);

static int
segment_create(opal_shmem_ds_t *ds_buf,
               const char *file_name,
               size_t size);

static int
ds_copy(const opal_shmem_ds_t *from,
        opal_shmem_ds_t *to);

static void *
segment_attach(opal_shmem_ds_t *ds_buf);

static int
segment_detach(opal_shmem_ds_t *ds_buf);

static int
segment_unlink(opal_shmem_ds_t *ds_buf);

static int
module_finalize(void);

/*
 * windows shmem module
 */
opal_shmem_windows_module_t opal_shmem_windows_module = {
    /* super */
    {
        module_init,
        segment_create,
        ds_copy,
        segment_attach,
        segment_detach,
        segment_unlink,
        module_finalize
    }
};

/* ////////////////////////////////////////////////////////////////////////// */
/* private utility functions */
/* ////////////////////////////////////////////////////////////////////////// */

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * completely resets the contents of *ds_buf
 */
static inline void
shmem_ds_reset(opal_shmem_ds_t *ds_buf)
{
    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: shmem_ds_resetting "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    ds_buf->opid = 0;
    ds_buf->seg_cpid = 0;
    OPAL_SHMEM_DS_RESET_FLAGS(ds_buf);
    ds_buf->seg_id = OPAL_SHMEM_DS_ID_INVALID;
    ds_buf->seg_size = 0;
    memset(ds_buf->seg_name, '\0', OPAL_PATH_MAX);
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_init(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_finalize(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
ds_copy(const opal_shmem_ds_t *from,
        opal_shmem_ds_t *to)
{
    memcpy(to, from, sizeof(opal_shmem_ds_t));

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: ds_copy complete "
         "from: (opid: %lu, id: %d, size: %"PRIsize_t", "
         "name: %s flags: 0x%02x) "
         "to: (opid: %lu, id: %d, size: %"PRIsize_t", "
         "name: %s flags: 0x%02x)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)from->opid, from->seg_id, from->seg_size,
         from->seg_name, from->flags, (unsigned long)to->opid, to->seg_id,
         to->seg_size, to->seg_name, to->flags)
    );

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_create(opal_shmem_ds_t *ds_buf,
               const char *file_name,
               size_t size)
{
    int rc = OPAL_SUCCESS;
    pid_t my_pid = getpid();
    char *temp1 = NULL, *temp2 = NULL;

    /* the real size of the shared memory segment.  this includes enough space
     * to store our segment header.
     */
    size_t real_size = size + sizeof(opal_shmem_seg_hdr_t);
    opal_shmem_seg_hdr_t *seg_hdrp = NULL;
    HANDLE hMapObject = INVALID_HANDLE_VALUE;
    LPVOID lpvMem = NULL;

    /* init the contents of opal_shmem_ds_t */
    shmem_ds_reset(ds_buf);

    /* On Windows the shared file will be created by the OS directly on the
     * system ressources. Therefore, no file get involved in the operation.
     * However, a unique key should be used as name for the shared memory object
     * in order to allow all processes to access the same unique shared memory
     * region. The key will be obtained from the original file_name by replacing
     * all path separator occurences by '/' (as '\' is not allowed on the object
     * name).
     */
    temp1 = strdup(file_name);
    if (NULL == temp1) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    temp2 = temp1;
    while (NULL != (temp2 = strchr(temp2, OPAL_PATH_SEP[0])) ) {
        *temp2 = '/';
    }
                                   /* use paging file */
    hMapObject = CreateFileMapping(INVALID_HANDLE_VALUE,
                                   /* no security attributes */
                                   NULL,
                                   /* read/write access */
                                   PAGE_READWRITE,
                                   /* size: high 32-bits */
                                   0,
                                   /* size: low 32-bits */
                                   (DWORD)real_size,
                                   /* name of map object */
                                   temp1);
    if (NULL == hMapObject) {
        rc = OPAL_ERROR;
        goto out;
    }
    /* Get a pointer to the file-mapped shared memory. */
    lpvMem = MapViewOfFile(hMapObject,          /* object to map view of */
                           FILE_MAP_WRITE,      /* read/write access */
                           0,                   /* high offset:  map from */
                           0,                   /* low offset:   beginning */
                           0);                  /* default: map entire file */
    if (NULL == lpvMem) {
        rc = OPAL_ERROR;
        goto out;
    }

    seg_hdrp = (opal_shmem_seg_hdr_t *)lpvMem;

    /* all is well */
    /* -- initialize the shared memory segment -- */
    opal_atomic_rmb();

    /* init segment lock */
    opal_atomic_init(&seg_hdrp->lock, OPAL_ATOMIC_UNLOCKED);
    /* i was the creator of this segment, so note that fact */
    seg_hdrp->cpid = my_pid;

    opal_atomic_wmb();

    /* -- initialize the contents of opal_shmem_ds_t -- */
    ds_buf->opid = my_pid;
    ds_buf->seg_cpid = my_pid;
    ds_buf->seg_size = real_size;
    ds_buf->seg_base_addr = (unsigned char *)seg_hdrp;
    /* update path change in ds_buf */
    memcpy(ds_buf->seg_name, temp1, OPAL_PATH_MAX);
    /* relase the temporary file name */
    free(temp1);

    /* set "valid" bit because setment creation was successful */
    OPAL_SHMEM_DS_SET_VALID(ds_buf);

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: create successful "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

out:
    /* an error occured, so invalidate the shmem object and munmap if needed */
    if (OPAL_SUCCESS != rc) {
        if (NULL != seg_hdrp) {
            UnmapViewOfFile((LPVOID)seg_hdrp);
        }
        shmem_ds_reset(ds_buf);
    }
    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * segment_attach can only be called after a successful call to segment_create
 */
static void *
segment_attach(opal_shmem_ds_t *ds_buf)
{
    pid_t my_pid = getpid();
    HANDLE hMapObject = INVALID_HANDLE_VALUE;
    LPVOID lpvMem = NULL;

    /* i was not the creator of the segment */
    if (my_pid != ds_buf->seg_cpid) {
                                       /* use paging file */
        hMapObject = CreateFileMapping(INVALID_HANDLE_VALUE,
                                       /* no security attributes */
                                       NULL,
                                       /* read/write access */
                                       PAGE_READWRITE,
                                       /* size: high 32-bits */
                                       0,
                                       /* size: low 32-bits */
                                       (DWORD)ds_buf->seg_size,
                                       /* name of map object */
                                       ds_buf->seg_name);
        if (NULL == hMapObject) {
            return NULL;
        }
        /* Get a pointer to the file-mapped shared memory. */
        lpvMem = MapViewOfFile(hMapObject,       /* object to map view of */
                               FILE_MAP_WRITE,   /* read/write access */
                               0,                /* high offset:  map from */
                               0,                /* low offset: beginning */
                               0);               /* default: map entire file */
        if (NULL == lpvMem) {
            return NULL;
        }
        ds_buf->seg_base_addr = (unsigned char *)lpvMem;
    }
    /* else i was the segment creator.  nothing to do here because all the hard
     * work was done in segment_create :-).
     */

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: attach successful "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    /* update returned base pointer with an offset that hides our stuff */
    return (ds_buf->seg_base_addr + sizeof(opal_shmem_seg_hdr_t));
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_detach(opal_shmem_ds_t *ds_buf)
{
    int rc = OPAL_SUCCESS;

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: detaching "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    if (0 == UnmapViewOfFile((LPVOID)ds_buf->seg_base_addr)) {
        int err = errno;
        char hn[MAXHOSTNAMELEN];
        gethostname(hn, MAXHOSTNAMELEN - 1);
        hn[MAXHOSTNAMELEN - 1] = '\0';
        opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1, hn,
                       "UnmapViewOfFile", "", strerror(err), err);
        rc = OPAL_ERROR;
    }
    /* reset the contents of the opal_shmem_ds_t associated with this
     * shared memory segment.
     */
    shmem_ds_reset(ds_buf);
    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_unlink(opal_shmem_ds_t *ds_buf)
{
    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: unlinking "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    /* don't completely reset the opal_shmem_ds_t.  in particular, only reset
     * the id and flip the invalid bit.  size and name values will remain valid
     * across unlinks. other information stored in flags will remain untouched.
     */
    ds_buf->seg_id = OPAL_SHMEM_DS_ID_INVALID;
    /* note: this is only chaning the valid bit to 0. */
    OPAL_SHMEM_DS_INVALIDATE(ds_buf);
    return OPAL_SUCCESS;
}
