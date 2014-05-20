/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>

#include "orte/runtime/data_type_support/orte_dt_support.h"

int orte_dt_compare_std_cntr(orte_std_cntr_t *value1, orte_std_cntr_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int orte_dt_compare_name(orte_process_name_t *value1,
                         orte_process_name_t *value2,
                         opal_data_type_t type)
{
    if (NULL == value1 && NULL == value2) {
        return OPAL_EQUAL;
    } else if (NULL == value1) {
        return OPAL_VALUE2_GREATER;
    } else if (NULL == value2) {
        return OPAL_VALUE1_GREATER;
    }
    
    /* If any of the fields are wildcard,
    * then we want to just ignore that one field. In the case
    * of ORTE_NAME_WILDCARD (where ALL of the fields are wildcard), this
    * will automatically result in OPAL_EQUAL for any name in the other
    * value - a totally useless result, but consistent in behavior.
    */
    
    /** check the jobids - if one of them is WILDCARD, then ignore
    * this field since anything is okay
    */
    if (value1->jobid != ORTE_JOBID_WILDCARD &&
        value2->jobid != ORTE_JOBID_WILDCARD) {
        if (value1->jobid < value2->jobid) {
            return OPAL_VALUE2_GREATER;
        } else if (value1->jobid > value2->jobid) {
            return OPAL_VALUE1_GREATER;
        }
    }
    
    /** check the vpids - if one of them is WILDCARD, then ignore
    * this field since anything is okay
    */
    if (value1->vpid != ORTE_VPID_WILDCARD &&
        value2->vpid != ORTE_VPID_WILDCARD) {
        if (value1->vpid < value2->vpid) {
            return OPAL_VALUE2_GREATER;
        } else if (value1->vpid > value2->vpid) {
            return OPAL_VALUE1_GREATER;
        }
    }
    
    /** only way to get here is if all fields are equal or WILDCARD */
    return OPAL_EQUAL;
}

int orte_dt_compare_vpid(orte_vpid_t *value1,
                         orte_vpid_t *value2,
                         opal_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORTE_VPID_WILDCARD ||
        *value2 == ORTE_VPID_WILDCARD) return OPAL_EQUAL;
    
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

int orte_dt_compare_jobid(orte_jobid_t *value1,
                          orte_jobid_t *value2,
                          opal_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORTE_JOBID_WILDCARD ||
        *value2 == ORTE_JOBID_WILDCARD) return OPAL_EQUAL;
    
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

#if !ORTE_DISABLE_FULL_SUPPORT
/**
 * JOB
 */
int orte_dt_compare_job(orte_job_t *value1, orte_job_t *value2, opal_data_type_t type)
{
    /** check jobids */
    if (value1->jobid > value2->jobid) return OPAL_VALUE1_GREATER;
    if (value1->jobid < value2->jobid) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

/**
* NODE
 */
int orte_dt_compare_node(orte_node_t *value1, orte_node_t *value2, opal_data_type_t type)
{
    int test;
    
    /** check node names */
    test = strcmp(value1->name, value2->name);
    if (0 == test) return OPAL_EQUAL;
    if (0 < test) return OPAL_VALUE2_GREATER;
    
    return OPAL_VALUE1_GREATER;
}

/**
* PROC
 */
int orte_dt_compare_proc(orte_proc_t *value1, orte_proc_t *value2, opal_data_type_t type)
{
    /** check vpids */
    if (value1->name.vpid > value2->name.vpid) return OPAL_VALUE1_GREATER;
    if (value1->name.vpid < value2->name.vpid) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/*
 * APP CONTEXT
 */
int orte_dt_compare_app_context(orte_app_context_t *value1, orte_app_context_t *value2, opal_data_type_t type)
{
    if (value1->idx > value2->idx) return OPAL_VALUE1_GREATER;
    if (value2->idx > value1->idx) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/*
 * EXIT CODE
 */
int orte_dt_compare_exit_code(orte_exit_code_t *value1,
                                    orte_exit_code_t *value2,
                                    opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/*
 * NODE STATE
 */
int orte_dt_compare_node_state(orte_node_state_t *value1,
                                     orte_node_state_t *value2,
                                     orte_node_state_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/*
 * PROC STATE
 */
int orte_dt_compare_proc_state(orte_proc_state_t *value1,
                                     orte_proc_state_t *value2,
                                     orte_proc_state_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/*
 * JOB STATE
 */
int orte_dt_compare_job_state(orte_job_state_t *value1,
                                    orte_job_state_t *value2,
                                    orte_job_state_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/*
 * JOB_MAP
 */
int orte_dt_compare_map(orte_job_map_t *value1, orte_job_map_t *value2, opal_data_type_t type)
{
    return OPAL_EQUAL;
}

/*
 * RML tags
 */
int orte_dt_compare_tags(orte_rml_tag_t *value1, orte_rml_tag_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) {
        return OPAL_VALUE1_GREATER;
    } else if (*value1 < *value2) {
        return OPAL_VALUE2_GREATER;
    } else {
        return OPAL_EQUAL;
    }
}

/* ORTE_DAEMON_CMD */
int orte_dt_compare_daemon_cmd(orte_daemon_cmd_flag_t *value1, orte_daemon_cmd_flag_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/* ORTE_GRPCOMM_MODE */
int orte_dt_compare_grpcomm_mode(orte_grpcomm_mode_t *value1, orte_grpcomm_mode_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

/* ORTE_IOF_TAG */
int orte_dt_compare_iof_tag(orte_iof_tag_t *value1, orte_iof_tag_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;
    
    if (*value2 > *value1) return OPAL_VALUE2_GREATER;
    
    return OPAL_EQUAL;
}

#endif
