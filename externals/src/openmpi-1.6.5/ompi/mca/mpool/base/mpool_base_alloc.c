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
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include "ompi/mca/mpool/mpool.h"
#include "base.h"
#include "mpool_base_tree.h"
#include "mpool_base_mem_cb.h"

/**
 * Memory Pool Registration
 */

static void mca_mpool_base_registration_constructor( mca_mpool_base_registration_t * reg )
{
    reg->mpool = NULL;
    reg->base = NULL;
    reg->bound = NULL;
    reg->alloc_base = NULL;
    reg->ref_count = 0;
    reg->flags = 0;
}

static void mca_mpool_base_registration_destructor( mca_mpool_base_registration_t * reg )
{
    
}

OBJ_CLASS_INSTANCE(
    mca_mpool_base_registration_t,
    ompi_free_list_item_t,
    mca_mpool_base_registration_constructor,
    mca_mpool_base_registration_destructor);

static void unregister_tree_item(mca_mpool_base_tree_item_t *mpool_tree_item)
{
    mca_mpool_base_module_t *mpool;
    mca_mpool_base_registration_t *reg;
    int i;

    for(i = 1; i < mpool_tree_item->count; i++) {
        mpool = mpool_tree_item->mpools[i];
        reg = mpool_tree_item->regs[i];
        if(mpool && mpool->mpool_deregister) {
            mpool->mpool_deregister(mpool, reg); 
        }
    }
    
    mpool = mpool_tree_item->mpools[0];
    reg =  mpool_tree_item->regs[0];
    mpool->mpool_free(mpool, mpool_tree_item->key, reg);
}

/**
 * Function to allocate special memory according to what the user requests in
 * the info object.
 *
 * If the user passes in a valid info structure then the function will
 * try to allocate the memory and register it with every mpool that there is a
 * key for it in the info struct. If it fails at registering the memory with 
 * one of the requested mpools, an error will be returned. Also, if there is a 
 * key in info that does not match any mpool, an error will be returned.
 *
 * If the info parameter is MPI_INFO_NULL, then this function will try to allocate
 * the memory and register it with as many mpools as possible. However, 
 * if any of the registratons fail the mpool will simply be ignored.
 *
 * @param size the size of the memory area to allocate
 * @param info an info object which tells us what kind of memory to allocate
 *
 * @retval pointer to the allocated memory
 * @retval NULL on failure
 */
void *mca_mpool_base_alloc(size_t size, ompi_info_t *info)
{
    opal_list_item_t * item;
    int num_modules = opal_list_get_size(&mca_mpool_base_modules);
    int reg_module_num = 0;
    int i;
    mca_mpool_base_selected_module_t * current;
    mca_mpool_base_selected_module_t * no_reg_function = NULL;
    mca_mpool_base_selected_module_t ** has_reg_function = NULL;
    mca_mpool_base_registration_t * registration;
    mca_mpool_base_tree_item_t* mpool_tree_item = NULL;
    mca_mpool_base_module_t *mpool;
    void * mem = NULL;
    int flag = 0;
    bool match_found = false;
    bool mpool_requested = false;

    if(num_modules > 0) {
        has_reg_function = (mca_mpool_base_selected_module_t **)
            malloc(num_modules * sizeof(mca_mpool_base_module_t *));
        if(!has_reg_function)
            goto out;
    }

    mpool_tree_item = mca_mpool_base_tree_item_get();
    
    if(!mpool_tree_item)
        goto out;

    mpool_tree_item->num_bytes = size;
    mpool_tree_item->count = 0;
    
    if(&ompi_mpi_info_null.info == info)
    {
        for(item = opal_list_get_first(&mca_mpool_base_modules);
            item != opal_list_get_end(&mca_mpool_base_modules);
            item = opal_list_get_next(item)) {
            current = ((mca_mpool_base_selected_module_t *) item);
            if(current->mpool_module->flags & MCA_MPOOL_FLAGS_MPI_ALLOC_MEM) {
                if(NULL == current->mpool_module->mpool_register){
                    no_reg_function = current;
                }
                else {
                    has_reg_function[reg_module_num++] = current;
                }
            }
        }
    }
    else
    {
        int num_keys;
        char key[MPI_MAX_INFO_KEY + 1];
        char value[MPI_MAX_INFO_VAL + 1];

        ompi_info_get_nkeys(info, &num_keys);
        for(i = 0; i < num_keys; i++)
        {
            ompi_info_get_nthkey(info, i, key);
            if ( 0 != strcmp(key, "mpool") ) {
                continue;
            }
            mpool_requested = true;
            ompi_info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
            if ( !flag ) {
                continue;
            }

            match_found = false;
            for(item = opal_list_get_first(&mca_mpool_base_modules);
                item != opal_list_get_end(&mca_mpool_base_modules);
                item = opal_list_get_next(item)) 
            {
                current = ((mca_mpool_base_selected_module_t *)item);
                if(0 == strcmp(value, 
                       current->mpool_module->mpool_component->mpool_version.mca_component_name))
                {
                    match_found = true;
                    if(NULL == current->mpool_module->mpool_register)
                    {
                        if(NULL != no_reg_function)
                        {
                           /* there was more than one requested mpool that lacks 
                            * a registration function, so return failure */
                            goto out;
                        }
                        no_reg_function = current;
                    }
                    else
                    {
                        has_reg_function[reg_module_num++] = current;
                    }
                }
            }
            if(!match_found)
            {
                /* one of the keys given to us by the user did not match any
                 * mpools, so return an error */
                goto out;
            }
        }
    }
    
    if(NULL == no_reg_function && 0 == reg_module_num)
    {
        if(!mpool_requested)
        {
            /* if the info argument was NULL and there were no useable mpools
             * or there user provided info object but did not specifiy a "mpool" key,
             * just malloc the memory and return it */
            mem = malloc(size);
            goto out;
        }
        
        /* the user passed info but we were not able to use any of the mpools 
         * specified */
        goto out;
    }
   
    for(i = -1; i < reg_module_num; i++) {
        if(-1 == i) {
            if(NULL != no_reg_function)
                mpool = no_reg_function->mpool_module;
            else
                continue;
        } else {
            mpool = has_reg_function[i]->mpool_module;
        }

        if(NULL == mem) {
            mem = mpool->mpool_alloc(mpool, size, 0, MCA_MPOOL_FLAGS_PERSIST,
                    &registration);
            if(NULL == mem) {
                if(mpool_requested)
                    goto out;
                continue;
            }
            mpool_tree_item->key = mem;
            mpool_tree_item->mpools[mpool_tree_item->count] = mpool;
            mpool_tree_item->regs[mpool_tree_item->count++] = registration;
        } else {
            if(mpool->mpool_register(mpool, mem, size, MCA_MPOOL_FLAGS_PERSIST,
                        &registration) != OMPI_SUCCESS) {
                if(mpool_requested) {
                    unregister_tree_item(mpool_tree_item);
                    goto out;
                }
                continue;
            }
            mpool_tree_item->mpools[mpool_tree_item->count] = mpool;
            mpool_tree_item->regs[mpool_tree_item->count++] = registration;
        }
    }

    if(NULL == mem) {
        mem = malloc(size);
        goto out;
    }

    mca_mpool_base_tree_insert(mpool_tree_item);
    mpool_tree_item = NULL; /* prevent it to be deleted below */
out:
    if(mpool_tree_item)
        mca_mpool_base_tree_item_put(mpool_tree_item);

    if(has_reg_function)
        free(has_reg_function);

    return mem;
}

/**
 * Function to free memory previously allocated by mca_mpool_base_alloc
 *
 * @param base pointer to the memory to free
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERR_BAD_PARAM if the passed base pointer was invalid
 */
int mca_mpool_base_free(void *base)
{
    mca_mpool_base_tree_item_t *mpool_tree_item = NULL;
    int rc;

    if(!base) {
        return OMPI_ERROR;
    }

    mpool_tree_item = mca_mpool_base_tree_find(base);

    if(!mpool_tree_item) { 
        /* nothing in the tree this was just plain old malloc'd memory */
        free(base);
        return OMPI_SUCCESS;
    }

    rc = mca_mpool_base_tree_delete(mpool_tree_item);
    if(OMPI_SUCCESS == rc) {
        unregister_tree_item(mpool_tree_item);
        mca_mpool_base_tree_item_put(mpool_tree_item);
    }
    
    return rc;
}
