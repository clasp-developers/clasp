/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/process/plm_process.h"
#include "orte/util/show_help.h" 

/*
 * Local function
 */
static char **search(const char* agent_list);


/*
 * Public string showing the plm ompi_process component version number
 */
const char *mca_plm_process_component_version_string =
  "Open MPI process plm MCA component version " ORTE_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_process_component_t mca_plm_process_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        ORTE_PLM_BASE_VERSION_2_0_0,

        /* Component name and version */
        "process",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_plm_process_component_open,
        orte_plm_process_component_close,
        orte_plm_process_component_query
    },
    {
        /* This component is not checkpointable */
        MCA_BASE_METADATA_PARAM_NONE
    }
    }
};



int orte_plm_process_component_open(void)
{
    int tmp, value;
    mca_base_component_t *c = &mca_plm_process_component.super.base_version;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_plm_process_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_plm_process_component.cond, opal_condition_t);
    mca_plm_process_component.num_children = 0;

    /* lookup parameters */
    mca_base_param_reg_int(c, "num_concurrent",
                           "How many plm_process_agent instances to invoke concurrently (must be > 0)",
                           false, false, 128, &tmp);
    if (tmp <= 0) {
        orte_show_help("help-plm-process.txt", "concurrency-less-than-zero",
                       true, tmp);
        tmp = 1;
    }
    mca_plm_process_component.num_concurrent = tmp;

    mca_base_param_reg_int(c, "force_process",
                           "Force the launcher to always use process, even for local daemons",
                           false, false, false, &tmp);
    mca_plm_process_component.force_process = OPAL_INT_TO_BOOL(tmp);
        
    tmp = mca_base_param_reg_int_name("orte", "timing",
                                      "Request that critical timing loops be measured",
                                      false, false, 0, &value);
    if (value != 0) {
        mca_plm_process_component.timing = true;
    } else {
        mca_plm_process_component.timing = false;
    }

    mca_base_param_reg_string(c, "orted",
                              "The command name that the process plm component will invoke for the ORTE daemon",
                              false, false, "orted.exe", 
                              &mca_plm_process_component.orted);
    
    mca_base_param_reg_int(c, "priority",
                           "Priority of the process plm component",
                           false, false, 10,
                           &mca_plm_process_component.priority);
    mca_base_param_reg_int(c, "delay",
                           "Delay (in seconds) between invocations of the remote agent, but only used when the \"debug\" MCA parameter is true, or the top-level MCA debugging is enabled (otherwise this value is ignored)",
                           false, false, 1,
                           &mca_plm_process_component.delay);
    mca_base_param_reg_int(c, "reap",
                           "If set to 1, wait for all the processes to complete before exiting.  Otherwise, quit immediately -- without waiting for confirmation that all other processes in the job have completed.",
                           false, false, 1, &tmp);
    mca_plm_process_component.reap = OPAL_INT_TO_BOOL(tmp);
    mca_base_param_reg_int(c, "assume_same_shell",
                           "If set to 1, assume that the shell on the remote node is the same as the shell on the local node.  Otherwise, probe for what the remote shell.",
                           false, false, 1, &tmp);
    mca_plm_process_component.assume_same_shell = OPAL_INT_TO_BOOL(tmp);
    mca_base_param_reg_int(c, "use_gui_prompt",
                           "If set to 1, use Windows standard GUI to input user name and password for connecting to remote node. Otherwise, use command line prompt.",
                           false, false, 0, &tmp);
    mca_plm_process_component.use_gui_prompt = OPAL_INT_TO_BOOL(tmp);
    mca_base_param_reg_int(c, "remote_reg_prefix",
                           "If set to 1, the process module will first try to read OPAL_PREFIX registry entry on remote computer to get the orte daemon execute path. If the read failed, it will use the default prefix.",
                           false, false, 1, &tmp);
    mca_plm_process_component.remote_reg_prefix = OPAL_INT_TO_BOOL(tmp);
    mca_base_param_reg_int(c, "remote_env_prefix",
                           "If set to 1, the process module will first try to read OPENMPI_HOME user env on remote computer to get the orte daemon execute path. If the read failed, it will try to read remote registry, and then default prefix.",
                           false, false, 1, &tmp);
    mca_plm_process_component.remote_env_prefix = OPAL_INT_TO_BOOL(tmp);

    return ORTE_SUCCESS;
}


int orte_plm_process_component_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_plm_process_component.priority;
    *module = (mca_base_module_t *) &orte_plm_process_module;
    return ORTE_SUCCESS;
}


int orte_plm_process_component_close(void)
{
    /* cleanup state */
    OBJ_DESTRUCT(&mca_plm_process_component.lock);
    OBJ_DESTRUCT(&mca_plm_process_component.cond);
    if (NULL != mca_plm_process_component.orted) {
        free(mca_plm_process_component.orted);
    }
    return ORTE_SUCCESS;
}


/*
 * Take a colon-delimited list of agents and locate the first one that
 * we are able to find in the PATH.  Split that one into argv and
 * return it.  If nothing found, then return NULL.
 */
static char **search(const char* agent_list)
{
    int i, j;
    char *line, **lines = opal_argv_split(agent_list, ':');
    char **tokens, *tmp;
    char cwd[PATH_MAX];

    getcwd(cwd, PATH_MAX);
    for (i = 0; NULL != lines[i]; ++i) {
        line = lines[i];

        /* Trim whitespace at the beginning and end of the line */
        for (j = 0; '\0' != line[j] && isspace(line[j]); ++line) {
            continue;
        }
        for (j = (int)strlen(line) - 2; j > 0 && isspace(line[j]); ++j) {
            line[j] = '\0';
        }
        if (strlen(line) <= 0) {
            continue;
        }

        /* Split it */
        tokens = opal_argv_split(line, ' ');

        /* Look for the first token in the PATH */
        tmp = opal_path_findv(tokens[0], X_OK, environ, cwd);
        if (NULL != tmp) {
            free(tokens[0]);
            tokens[0] = tmp;
            opal_argv_free(lines);
            return tokens;
        }

        /* Didn't find it */
        opal_argv_free(tokens);
    }

    /* Doh -- didn't find anything */
    opal_argv_free(lines);
    return NULL;
}
