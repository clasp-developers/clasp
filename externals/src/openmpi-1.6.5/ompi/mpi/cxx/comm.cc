// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2005 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

// do not include ompi_config.h because it kills the free/malloc defines
#include "mpi.h"
#include "ompi/mpi/cxx/mpicxx.h"

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif


#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/errhandler/errhandler.h"


//
// These functions are all not inlined because they need to use locks to
// protect the handle maps and it would be bad to have those in headers
// because that would require that we always install the lock headers.
// Instead we take the function call hit (we're locking - who cares about
// a function call.  And these aren't exactly the performance critical
// functions) and make everyone's life easier.
//


// construction
MPI::Comm::Comm()
{
}

// copy
MPI::Comm::Comm(const Comm_Null& data) : Comm_Null(data) 
{
}

// This function needs some internal OMPI types, so it's not inlined
MPI::Errhandler
MPI::Comm::Create_errhandler(MPI::Comm::_MPI2CPP_ERRHANDLERFN_* function)
{
    MPI_Errhandler c_errhandler = 
        ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_COMM,
                               (ompi_errhandler_generic_handler_fn_t*) function,
                               OMPI_ERRHANDLER_LANG_CXX);
    c_errhandler->eh_cxx_dispatch_fn = 
        (ompi_errhandler_cxx_dispatch_fn_t*)
        ompi_mpi_cxx_comm_errhandler_invoke;
    return c_errhandler;
}


//JGS I took the const out because it causes problems when trying to
//call this function with the predefined NULL_COPY_FN etc.
int
MPI::Comm::do_create_keyval(MPI_Comm_copy_attr_function* c_copy_fn,
                            MPI_Comm_delete_attr_function* c_delete_fn,
                            Copy_attr_function* cxx_copy_fn,
                            Delete_attr_function* cxx_delete_fn,
                            void* extra_state, int &keyval)
{
    int ret, count = 0;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t delete_fn;
    keyval_intercept_data_t *cxx_extra_state;

    // If both the callbacks are C, then do the simple thing -- no
    // need for all the C++ machinery.
    if (NULL != c_copy_fn && NULL != c_delete_fn) {
        copy_fn.attr_communicator_copy_fn = 
            (MPI_Comm_internal_copy_attr_function*) c_copy_fn;
        delete_fn.attr_communicator_delete_fn = c_delete_fn;
        ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, delete_fn,
                                      &keyval, extra_state, 0, NULL);
        if (MPI_SUCCESS != ret) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
                                          "MPI::Comm::Create_keyval");
        }
    }

    // If either callback is C++, then we have to use the C++
    // callbacks for both, because we have to generate a new
    // extra_state.  And since we only get one extra_state (i.e., we
    // don't get one extra_state for the copy callback and another
    // extra_state for the delete callback), we have to use the C++
    // callbacks for both (and therefore translate the C++-special
    // extra_state into the user's original extra_state).  Ensure to
    // malloc() the struct here (vs new) so that it can be free()'ed
    // by the C attribute base.
    cxx_extra_state = (keyval_intercept_data_t*) 
        malloc(sizeof(keyval_intercept_data_t));
    if (NULL == cxx_extra_state) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM, 
                                      "MPI::Comm::Create_keyval");
    }
    cxx_extra_state->c_copy_fn = c_copy_fn;
    cxx_extra_state->cxx_copy_fn = cxx_copy_fn;
    cxx_extra_state->c_delete_fn = c_delete_fn;
    cxx_extra_state->cxx_delete_fn = cxx_delete_fn;
    cxx_extra_state->extra_state = extra_state;

    // Error check.  Must have exactly 2 non-NULL function pointers.
    if (NULL != c_copy_fn) {
        ++count;
    }
    if (NULL != c_delete_fn) {
        ++count;
    }
    if (NULL != cxx_copy_fn) {
        ++count;
    }
    if (NULL != cxx_delete_fn) {
        ++count;
    }
    if (2 != count) {
        free(cxx_extra_state);
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                      "MPI::Comm::Create_keyval");
    }

    // We do not call MPI_Comm_create_keyval() here because we need to
    // pass in the cxx_extra_state to the backend keyval creation so
    // that when the keyval is destroyed (i.e., when its refcount goes
    // to 0), the cxx_extra_state is free()'ed.

    copy_fn.attr_communicator_copy_fn =
        (MPI_Comm_internal_copy_attr_function*) 
        ompi_mpi_cxx_comm_copy_attr_intercept;
    delete_fn.attr_communicator_delete_fn =
        ompi_mpi_cxx_comm_delete_attr_intercept;
    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, delete_fn,
                                  &keyval, cxx_extra_state, 0,
                                  cxx_extra_state);
    if (OMPI_SUCCESS != ret) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
                                      "MPI::Comm::Create_keyval");
    }

    return MPI_SUCCESS;
}

