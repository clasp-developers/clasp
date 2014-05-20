/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_object.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/mpi/f77/datarep.h"
#include "ompi/mpi/f77/f77_strings.h"
#include "ompi/mpi/f77/fint_2_int.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/file/file.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_REGISTER_DATAREP = mpi_register_datarep_f
#pragma weak pmpi_register_datarep = mpi_register_datarep_f
#pragma weak pmpi_register_datarep_ = mpi_register_datarep_f
#pragma weak pmpi_register_datarep__ = mpi_register_datarep_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REGISTER_DATAREP,
                           pmpi_register_datarep,
                           pmpi_register_datarep_,
                           pmpi_register_datarep__,
                           pmpi_register_datarep_f,
                           (char *datarep, ompi_mpi2_fortran_datarep_conversion_fn_t *read_conversion_fn, ompi_mpi2_fortran_datarep_conversion_fn_t *write_conversion_fn, ompi_mpi2_fortran_datarep_extent_fn_t *dtype_file_extent_fn, MPI_Aint *extra_state, MPI_Fint *ierr, int datarep_len),
                           (datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierr, datarep_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REGISTER_DATAREP = mpi_register_datarep_f
#pragma weak mpi_register_datarep = mpi_register_datarep_f
#pragma weak mpi_register_datarep_ = mpi_register_datarep_f
#pragma weak mpi_register_datarep__ = mpi_register_datarep_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REGISTER_DATAREP,
                           mpi_register_datarep,
                           mpi_register_datarep_,
                           mpi_register_datarep__,
                           mpi_register_datarep_f,
                           (char *datarep, ompi_mpi2_fortran_datarep_conversion_fn_t *read_conversion_fn, ompi_mpi2_fortran_datarep_conversion_fn_t *write_conversion_fn, ompi_mpi2_fortran_datarep_extent_fn_t *dtype_file_extent_fn, MPI_Aint *extra_state, MPI_Fint *ierr, int datarep_len),
                           (datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierr, datarep_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_REGISTER_DATAREP";

/* Intercept functions used below (see below for explanations in
   comments) */
static int read_intercept_fn(void *userbuf, MPI_Datatype type_c, int count_c,
                             void *filebuf, MPI_Offset position, 
                             void *extra_state);
static int write_intercept_fn(void *userbuf, MPI_Datatype type_c, int count_c,
                             void *filebuf, MPI_Offset position, 
                              void *extra_state);
static int extent_intercept_fn(MPI_Datatype type_c, MPI_Aint *file_extent, 
                               void *extra_state);

/* Data structure passed to the intercepts (see below).  It is an OPAL
   list_item_t so that we can clean this memory up during
   MPI_FINALIZE.  */
typedef struct intercept_extra_state {
    opal_list_item_t base;
    ompi_mpi2_fortran_datarep_conversion_fn_t *read_fn_f77;
    ompi_mpi2_fortran_datarep_conversion_fn_t *write_fn_f77;
    ompi_mpi2_fortran_datarep_extent_fn_t *extent_fn_f77;
    MPI_Aint *extra_state_f77;
} intercept_extra_state_t;

OBJ_CLASS_DECLARATION(intercept_extra_state_t);

#if !OMPI_PROFILE_LAYER || OPAL_HAVE_WEAK_SYMBOLS
static void intercept_extra_state_constructor(intercept_extra_state_t *obj)
{
    obj->read_fn_f77 = NULL;
    obj->write_fn_f77 = NULL;
    obj->extent_fn_f77 = NULL;
    obj->extra_state_f77 = NULL;
}

OBJ_CLASS_INSTANCE(intercept_extra_state_t,
                   opal_list_item_t,
                   intercept_extra_state_constructor, NULL);
#endif  /* !OMPI_PROFILE_LAYER */

/*
 * This function works by calling the C version of
 * MPI_Register_datarep (like most other MPI API functions).  To do
 * that, however, we need to call the C MPI_Register_datarep with *C*
 * callback functions -- the callback functions passed in to this
 * function are Fortran functions, and expect Fortran argument passing
 * conventions.
 *
 * So we have 3 C intercept functions that are passed to the back-end
 * MPI_Register_datarep.  Hence, when/if this datarep is ever used,
 * the intercept function(s) are invoked, who then translate the
 * arguments to Fortran and then invoke the registered callback
 * function.
 */
void mpi_register_datarep_f(char *datarep, 
                            ompi_mpi2_fortran_datarep_conversion_fn_t *read_fn_f77,
                            ompi_mpi2_fortran_datarep_conversion_fn_t *write_fn_f77,
                            ompi_mpi2_fortran_datarep_extent_fn_t *extent_fn_f77, 
                            MPI_Aint *extra_state_f77,
                            MPI_Fint *ierr, int datarep_len)
{
    char *c_datarep;
    int c_err, ret;
    MPI_Datarep_conversion_function *read_fn_c, *write_fn_c;
    intercept_extra_state_t *intercept;
    
    /* Malloc space for the intercept callback data */
    intercept = OBJ_NEW(intercept_extra_state_t);
    if (NULL == intercept) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, 
                                       OMPI_ERR_OUT_OF_RESOURCE, FUNC_NAME);
        *ierr = OMPI_INT_2_FINT(c_err);
        return;
    }
    /* Save the new object on a global list because per MPI-2:9.5.3,
       there are no ways for the user to deregister datareps once
       they've been created.  Hece, this is a memory leak.  So we
       track these extra resources in a global list so that they can
       be freed during MPI_FINALIZE (so that memory-tracking debuggers
       won't show MPI as leaking memory). */
    opal_list_append(&ompi_registered_datareps, &(intercept->base));

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(datarep, datarep_len,
                                                       &c_datarep))) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, ret, FUNC_NAME);
        *ierr = OMPI_INT_2_FINT(c_err);
        return;
    }
    
    /* Convert the Fortran function callbacks to C equivalents.  Use
       local intercepts if they're not MPI_CONVERSION_FN_NULL so that
       we can just call the C MPI API MPI_Register_datarep().  If they
       *are* MPI_CONVERSION_FN_NULL, then just pass that to
       MPI_Register_datarep so that it becomes a no-op (i.e., no
       callback is ever triggered). */
    if (OMPI_IS_FORTRAN_CONVERSION_FN_NULL(read_fn_f77)) {
        /* Can't use the MPI_CONVERSION_FN_NULL macro here because it
           is specifically not defined when compiling this file so
           that we can prototype an all-caps Fortran function */
        read_fn_c = (MPI_Datarep_conversion_function*) 0;
    } else {
        intercept->read_fn_f77 = read_fn_f77;
        read_fn_c = read_intercept_fn;
    }
    if (OMPI_IS_FORTRAN_CONVERSION_FN_NULL(write_fn_f77)) {
        /* Can't use the MPI_CONVERSION_FN_NULL macro here because it
           is specifically not defined when compiling this file so
           that we can prototype an all-caps Fortran function */
        write_fn_c = (MPI_Datarep_conversion_function*) 0;
    } else {
        intercept->write_fn_f77 = write_fn_f77;
        write_fn_c = write_intercept_fn;
    }
    intercept->extent_fn_f77 = extent_fn_f77;
    intercept->extra_state_f77 = extra_state_f77;

    /* Now that the intercept data has been setup, call the C function
       with the setup intercept routines and the intercept-specific
       data/extra state. */
    *ierr = OMPI_INT_2_FINT(MPI_Register_datarep(c_datarep, 
                                                 read_fn_c, write_fn_c, 
                                                 extent_intercept_fn,
                                                 intercept));
    free(c_datarep);
}

/*
 * C->Fortran intercept for the read conversion.
 */
static int read_intercept_fn(void *userbuf, MPI_Datatype type_c, int count_c,
                             void *filebuf, MPI_Offset position, 
                             void *extra_state)
{
    MPI_Fint ierr, count_f77 = OMPI_FINT_2_INT(count_c);
    MPI_Fint type_f77 = MPI_Type_c2f(type_c);
    intercept_extra_state_t *intercept_data = 
        (intercept_extra_state_t*) extra_state;

    intercept_data->read_fn_f77((char *) userbuf, &type_f77, &count_f77, (char *) filebuf, 
                                &position, intercept_data->extra_state_f77, 
                                &ierr);
    return OMPI_FINT_2_INT(ierr);
}

/*
 * C->Fortran intercept for the write conversion.
 */
static int write_intercept_fn(void *userbuf, MPI_Datatype type_c, int count_c,
                             void *filebuf, MPI_Offset position, 
                             void *extra_state)
{
    MPI_Fint ierr, count_f77 = OMPI_FINT_2_INT(count_c);
    MPI_Fint type_f77 = MPI_Type_c2f(type_c);
    intercept_extra_state_t *intercept_data = 
        (intercept_extra_state_t*) extra_state;

    intercept_data->write_fn_f77((char *) userbuf, &type_f77, &count_f77, (char *) filebuf, 
                                 &position, intercept_data->extra_state_f77, 
                                 &ierr);
    return OMPI_FINT_2_INT(ierr);
}

/*
 * C->Fortran intercept for the extent calculation.
 */
static int extent_intercept_fn(MPI_Datatype type_c, MPI_Aint *file_extent_f77, 
                               void *extra_state)
{
    MPI_Fint ierr, type_f77 = MPI_Type_c2f(type_c);
    intercept_extra_state_t *intercept_data = 
        (intercept_extra_state_t*) extra_state;

    intercept_data->extent_fn_f77(&type_f77, file_extent_f77, 
                                 intercept_data->extra_state_f77, &ierr);
    return OMPI_FINT_2_INT(ierr);
}

