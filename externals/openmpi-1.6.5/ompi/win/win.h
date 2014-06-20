/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_WIN_H
#define OMPI_WIN_H

#include "ompi_config.h"
#include "mpi.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/mca/osc/osc.h"

BEGIN_C_DECLS

/* flags */
#define OMPI_WIN_FREED        0x00000001
#define OMPI_WIN_INVALID      0x00000002
#define OMPI_WIN_NO_LOCKS     0x00000004

/* mode */
#define OMPI_WIN_ACCESS_EPOCH 0x00000001
#define OMPI_WIN_EXPOSE_EPOCH 0x00000002
#define OMPI_WIN_FENCE        0x00000010
#define OMPI_WIN_POSTED       0x00000020
#define OMPI_WIN_STARTED      0x00000040
#define OMPI_WIN_LOCK_ACCESS  0x00000080

OMPI_DECLSPEC extern opal_pointer_array_t ompi_mpi_windows;

struct ompi_win_t {
    opal_object_t w_base;

    opal_mutex_t  w_lock;

    char w_name[MPI_MAX_OBJECT_NAME];

    /* Group associated with this window. */
    ompi_group_t *w_group;

    /* Information about the state of the window.  */
    uint16_t w_flags;

    /* Attributes */
    opal_hash_table_t *w_keyhash;

    /* index in Fortran <-> C translation array */
    int w_f_to_c_index;

    /* Error handling.  This field does not have the "w_" prefix so
       that the OMPI_ERRHDL_* macros can find it, regardless of
       whether it's a comm, window, or file. */
    ompi_errhandler_t                    *error_handler;
    ompi_errhandler_type_t               errhandler_type;

    /* displacement factor */
    int w_disp_unit;

    void *w_baseptr;
    size_t w_size;

    /** Current epoch / mode (access, expose, lock, etc.).  Checked by
        the argument checking code in the MPI layer, set by the OSC
        component.  Modified without locking w_lock. */
    volatile uint16_t w_mode;

    /* one sided interface */
    ompi_osc_base_module_t *w_osc_module;
};
typedef struct ompi_win_t ompi_win_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_win_t);

/**
 * Padded struct to maintain back compatibiltiy.
 * See ompi/communicator/communicator.h comments with struct ompi_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */
#define PREDEFINED_WIN_PAD (sizeof(void*) * 64)

struct ompi_predefined_win_t {
    struct ompi_win_t win;
    char padding[PREDEFINED_WIN_PAD - sizeof(ompi_win_t)];
};
typedef struct ompi_predefined_win_t ompi_predefined_win_t;

OMPI_DECLSPEC extern ompi_predefined_win_t ompi_mpi_win_null;

int ompi_win_init(void);
int ompi_win_finalize(void);

int ompi_win_create(void *base, size_t size, int disp_unit, 
                    ompi_communicator_t *comm, ompi_info_t *info,
                    ompi_win_t **newwin);

int ompi_win_free(ompi_win_t *win);

OMPI_DECLSPEC int ompi_win_set_name(ompi_win_t *win, char *win_name);
OMPI_DECLSPEC int ompi_win_get_name(ompi_win_t *win, char *win_name, int *length);

OMPI_DECLSPEC int ompi_win_group(ompi_win_t *win, ompi_group_t **group);

/* Note that the defintion of an "invalid" window is closely related
   to the defintion of an "invalid" communicator.  See a big comment
   in ompi/communicator/communicator.h about this. */
static inline int ompi_win_invalid(ompi_win_t *win) {
    if (NULL == win || 
        MPI_WIN_NULL == win ||
        (OMPI_WIN_INVALID & win->w_flags) ||
        (OMPI_WIN_FREED & win->w_flags)) {
        return true;
    } else {
        return false;
    }
}

static inline int ompi_win_peer_invalid(ompi_win_t *win, int peer) {
    if (win->w_group->grp_proc_count <= peer) return true;
    return false;
}

static inline int ompi_win_rank(ompi_win_t *win) {
    return win->w_group->grp_my_rank;
}

static inline bool ompi_win_allow_locks(ompi_win_t *win) {
    return (0 == (win->w_flags & OMPI_WIN_NO_LOCKS));
}

static inline int16_t ompi_win_get_mode(ompi_win_t *win) {
    int16_t mode = win->w_mode;
    opal_atomic_rmb();
    return mode;
}

static inline void ompi_win_set_mode(ompi_win_t *win, int16_t mode) {
    win->w_mode = mode;
    opal_atomic_wmb();
}

static inline void ompi_win_append_mode(ompi_win_t *win, int16_t mode) {
    win->w_mode |= mode;
    opal_atomic_wmb();
}

static inline void ompi_win_remove_mode(ompi_win_t *win,
                                        int16_t mode)
{
    win->w_mode &= ~mode;
    opal_atomic_wmb();
}

/* already in an access epoch */
static inline bool ompi_win_access_epoch(ompi_win_t *win) {
    int16_t mode = ompi_win_get_mode(win);
    return (0 != (OMPI_WIN_ACCESS_EPOCH & mode) ? true : false);
}

/* already in an exposure epoch */
static inline bool ompi_win_exposure_epoch(ompi_win_t *win) {
    int16_t mode = ompi_win_get_mode(win);
    return (0 != (OMPI_WIN_EXPOSE_EPOCH & mode) ? true : false);
}

/* we're either already in an access epoch or can easily start one
   (stupid fence rule). Either way, it's ok to be the origin of a
   communication call. */
static inline bool ompi_win_comm_allowed(ompi_win_t *win) {
    int16_t mode = ompi_win_get_mode(win);
    return (0 != (OMPI_WIN_ACCESS_EPOCH & mode || OMPI_WIN_FENCE & mode) ? true : false);
}

END_C_DECLS
#endif
