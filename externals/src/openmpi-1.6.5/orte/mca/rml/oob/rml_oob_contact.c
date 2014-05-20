/*
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "rml_oob.h"

char* 
orte_rml_oob_get_uri(void)
{
    char *proc_name = NULL;
    char *proc_addr = NULL;
    char *contact_info = NULL;
    int rc;

    proc_addr = orte_rml_oob_module.active_oob->oob_get_addr();
    if (NULL == proc_addr) return NULL;

    if (ORTE_SUCCESS != (rc = orte_util_convert_process_name_to_string(&proc_name,
                                            ORTE_PROC_MY_NAME))) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }
    if (0 > asprintf(&contact_info, "%s;%s", proc_name, proc_addr)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
    }
    free(proc_name);
    free(proc_addr);
    return contact_info;
}


int
orte_rml_oob_set_uri(const char* uri)
{
    orte_process_name_t name;
    char** uris;
    char** ptr;
    int rc = orte_rml_base_parse_uris(uri, &name, &uris);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    for(ptr = uris; ptr != NULL && *ptr != NULL; ptr++) {
        orte_rml_oob_module.active_oob->oob_set_addr(&name, *ptr);
    }

    if(uris != NULL) {
        opal_argv_free(uris);
    }
    return ORTE_SUCCESS;
}


int
orte_rml_oob_get_new_name(orte_process_name_t *name)
{
    if (NULL != ORTE_PROC_MY_NAME) {
        return ORTE_ERR_NOT_SUPPORTED;
    }

    return orte_rml_oob_module.active_oob->oob_get_new_name(name);

}
