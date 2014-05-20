// -*- c++ -*-
// 
// Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
//                         reserved. 
// Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

// Do not include ompi_config.h before mpi.h because it causes
// malloc/free problems due to setting OMPI_BUILDING to 1
#include "mpi.h"

#include "ompi/constants.h"
#include "ompi/mpi/cxx/mpicxx.h"
#include "opal/class/opal_list.h"
#include "ompi/file/file.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/runtime/mpiruntime.h"

void 
MPI::File::Close() 
{
    (void) MPI_File_close(&mpi_file);
}

  
MPI::Errhandler 
MPI::File::Create_errhandler(MPI::File::Errhandler_function* function)
{
    MPI_Errhandler c_errhandler = 
        ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_FILE,
                               (ompi_errhandler_generic_handler_fn_t*) function,
                               OMPI_ERRHANDLER_LANG_CXX);
    c_errhandler->eh_cxx_dispatch_fn = 
        (ompi_errhandler_cxx_dispatch_fn_t*)
        ompi_mpi_cxx_file_errhandler_invoke;
    return c_errhandler;
}


//
// Infrastructure for MPI_REGISTER_DATAREP
//
// Similar to what we have to do in the F77 bindings: call the C
// MPI_Register_datarep function with "intercept" callback functions
// that conform to the C bindings.  In these intercepts, convert the
// arguments to C++ calling convertions, and then invoke the actual
// C++ callbacks.

// Data structure passed to the intercepts (see below).  It is an OPAL
// list_item_t so that we can clean this memory up during
// MPI_FINALIZE.
typedef struct intercept_extra_state {
    opal_list_item_t base;
    MPI::Datarep_conversion_function *read_fn_cxx;
    MPI::Datarep_conversion_function *write_fn_cxx;
    MPI::Datarep_extent_function *extent_fn_cxx;
    void *extra_state_cxx;
} intercept_extra_state_t;

static void intercept_extra_state_constructor(intercept_extra_state_t *obj)
{
    obj->read_fn_cxx = NULL;
    obj->write_fn_cxx = NULL;
    obj->extent_fn_cxx = NULL;
    obj->extra_state_cxx = NULL;
}

OBJ_CLASS_DECLARATION(intercept_extra_state_t);
OBJ_CLASS_INSTANCE(intercept_extra_state_t,
                   opal_list_item_t,
                   intercept_extra_state_constructor, NULL);

// Intercept function for read conversions
static int read_intercept_fn(void *userbuf, MPI_Datatype type_c, int count_c,
                             void *filebuf, MPI_Offset position_c, 
                             void *extra_state)
{
    MPI::Datatype type_cxx(type_c);
    MPI::Offset position_cxx(position_c);
    intercept_extra_state_t *intercept_data = 
        (intercept_extra_state_t*) extra_state;

    intercept_data->read_fn_cxx(userbuf, type_cxx, count_c, filebuf,
                                position_cxx, intercept_data->extra_state_cxx);
    return MPI_SUCCESS;
}

// Intercept function for write conversions
static int write_intercept_fn(void *userbuf, MPI_Datatype type_c, int count_c,
                             void *filebuf, MPI_Offset position_c, 
                              void *extra_state)
{
    MPI::Datatype type_cxx(type_c);
    MPI::Offset position_cxx(position_c);
    intercept_extra_state_t *intercept_data = 
        (intercept_extra_state_t*) extra_state;

    intercept_data->write_fn_cxx(userbuf, type_cxx, count_c, filebuf,
                                 position_cxx, intercept_data->extra_state_cxx);
    return MPI_SUCCESS;
}

// Intercept function for extent calculations
static int extent_intercept_fn(MPI_Datatype type_c, MPI_Aint *file_extent_c, 
                               void *extra_state)
{
    MPI::Datatype type_cxx(type_c);
    MPI::Aint file_extent_cxx(*file_extent_c);
    intercept_extra_state_t *intercept_data = 
        (intercept_extra_state_t*) extra_state;

    intercept_data->extent_fn_cxx(type_cxx, file_extent_cxx, 
                                  intercept_data->extra_state_cxx);
    *file_extent_c = file_extent_cxx;
    return MPI_SUCCESS;
}

// C++ bindings for MPI::Register_datarep
void 
MPI::Register_datarep(const char* datarep, 
                      Datarep_conversion_function* read_fn_cxx, 
                      Datarep_conversion_function* write_fn_cxx, 
                      Datarep_extent_function* extent_fn_cxx, 
                      void* extra_state_cxx)
{
    intercept_extra_state_t *intercept;

    intercept = OBJ_NEW(intercept_extra_state_t);
    if (NULL == intercept) {
        OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, OMPI_ERR_OUT_OF_RESOURCE, 
                               "MPI::Register_datarep");
        return;
    }
    opal_list_append(&ompi_registered_datareps, &(intercept->base));
    intercept->read_fn_cxx = read_fn_cxx;
    intercept->write_fn_cxx = write_fn_cxx;
    intercept->extent_fn_cxx = extent_fn_cxx;
    intercept->extra_state_cxx = extra_state_cxx;

    (void)MPI_Register_datarep(const_cast<char*>(datarep), read_intercept_fn, 
                               write_intercept_fn,
                               extent_intercept_fn, intercept);
}


void 
MPI::Register_datarep(const char* datarep, 
                      MPI_Datarep_conversion_function* read_fn_c,
                      Datarep_conversion_function* write_fn_cxx, 
                      Datarep_extent_function* extent_fn_cxx, 
                      void* extra_state_cxx)
{
    intercept_extra_state_t *intercept;

    intercept = OBJ_NEW(intercept_extra_state_t);
    if (NULL == intercept) {
        OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, OMPI_ERR_OUT_OF_RESOURCE, 
                               "MPI::Register_datarep");
        return;
    }
    opal_list_append(&ompi_registered_datareps, &(intercept->base));
    intercept->write_fn_cxx = write_fn_cxx;
    intercept->extent_fn_cxx = extent_fn_cxx;
    intercept->extra_state_cxx = extra_state_cxx;

    (void)MPI_Register_datarep(const_cast<char*>(datarep), read_fn_c, 
                               write_intercept_fn,
                               extent_intercept_fn, intercept);
}


void 
MPI::Register_datarep(const char* datarep, 
                      Datarep_conversion_function* read_fn_cxx, 
                      MPI_Datarep_conversion_function* write_fn_c, 
                      Datarep_extent_function* extent_fn_cxx, 
                      void* extra_state_cxx)
{
    intercept_extra_state_t *intercept;

    intercept = OBJ_NEW(intercept_extra_state_t);
    if (NULL == intercept) {
        OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, OMPI_ERR_OUT_OF_RESOURCE, 
                               "MPI::Register_datarep");
        return;
    }
    opal_list_append(&ompi_registered_datareps, &(intercept->base));
    intercept->read_fn_cxx = read_fn_cxx;
    intercept->extent_fn_cxx = extent_fn_cxx;
    intercept->extra_state_cxx = extra_state_cxx;

    (void)MPI_Register_datarep(const_cast<char*>(datarep), read_intercept_fn,
                               write_fn_c,
                               extent_intercept_fn, intercept);
}


void 
MPI::Register_datarep(const char* datarep, 
                      MPI_Datarep_conversion_function* read_fn_c,
                      MPI_Datarep_conversion_function* write_fn_c, 
                      Datarep_extent_function* extent_fn_cxx, 
                      void* extra_state_cxx)
{
    intercept_extra_state_t *intercept;

    intercept = OBJ_NEW(intercept_extra_state_t);
    if (NULL == intercept) {
        OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, OMPI_ERR_OUT_OF_RESOURCE, 
                               "MPI::Register_datarep");
        return;
    }
    opal_list_append(&ompi_registered_datareps, &(intercept->base));
    intercept->extent_fn_cxx = extent_fn_cxx;
    intercept->extra_state_cxx = extra_state_cxx;

    (void)MPI_Register_datarep(const_cast<char*>(datarep), read_fn_c, 
                               write_fn_c,
                               extent_intercept_fn, intercept);
}


