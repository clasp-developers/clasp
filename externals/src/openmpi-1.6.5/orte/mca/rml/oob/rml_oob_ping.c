/*
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "rml_oob.h"

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rml/base/rml_contact.h"

int
orte_rml_oob_ping(const char* uri, 
                  const struct timeval* tv)
{
    orte_process_name_t name;
    char** uris;
    char** ptr;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(uri, &name, &uris))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
 
    ptr = uris;
    while(ptr && *ptr) {
        if(ORTE_SUCCESS == (rc = orte_rml_oob_module.active_oob->oob_ping(&name, *ptr, tv)))
            break;
        ptr++;
    }
    opal_argv_free(uris);
    return rc;
}
