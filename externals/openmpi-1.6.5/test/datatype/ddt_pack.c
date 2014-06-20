/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sun Microsystems Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/proc/proc.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif


int
main(int argc, char* argv[])
{
    size_t packed_ddt_len;
    const void *packed_ddt;
    void *payload, *ptr;
    struct ompi_datatype_t *unpacked_dt;
    int ret = 0;
    int         blen[2];
    MPI_Aint    disp[2];
    MPI_Datatype newType, types[2], struct_type;

    MPI_Init(&argc, &argv);

    /* Basic test... */
    printf("---> Basic test with MPI_INT\n");

    packed_ddt_len = ompi_datatype_pack_description_length(MPI_INT);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(MPI_INT, &packed_ddt);
    if (ret != 0) goto cleanup;
    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                          ompi_proc_local());
    free(ptr);
    if (unpacked_dt == MPI_INT) {
        printf("\tPASSED\n");
    } else {
        printf("\tFAILED: datatypes don't match\n");
        ret = 1;
        goto cleanup;
    }

    printf("---> Advanced test with hindexed\n");
    
    blen[0] = 10;
    blen[1] = 10;
    disp[0] = 0;
    disp[1] = 20*sizeof(double);

    ret = MPI_Type_create_hindexed(2, blen, disp, MPI_DOUBLE,
                &newType);
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&newType);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(newType);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(newType, &packed_ddt);
    if (ret != 0) goto cleanup;
    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                          ompi_proc_local());
    free(ptr);
    if (unpacked_dt != NULL) {
        printf("\tPASSED\n");
    } else {
        printf("\tFAILED: datatypes don't match\n");
        ret = 1;
        goto cleanup;
    }

    printf("---> Even more advanced test using the previous type and struct\n");
    blen[0] = 11;
    blen[1] = 2;
    disp[0] = 0;
    disp[1] = 64;
    types[0] = MPI_INT;
    types[1] = newType;
    MPI_Type_create_struct( 2, blen, disp, types, &struct_type );
    if (ret != 0) goto cleanup;

    ret = MPI_Type_commit(&struct_type);
    if (ret != 0) goto cleanup;

    packed_ddt_len = ompi_datatype_pack_description_length(struct_type);
    ptr = payload = malloc(packed_ddt_len);
    ret = ompi_datatype_get_pack_description(struct_type, &packed_ddt);
    if (ret != 0) goto cleanup;
    memcpy(payload, packed_ddt, packed_ddt_len);
    unpacked_dt = ompi_datatype_create_from_packed_description(&payload,
                                                          ompi_proc_local());
    free(ptr);
    if (unpacked_dt != NULL) {
        printf("\tPASSED\n");
    } else {
        printf("\tFAILED: datatypes don't match\n");
        ret = 1;
        goto cleanup;
    }

 cleanup:
    MPI_Finalize();

    return ret;
}
