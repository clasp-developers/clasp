/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"

int opal_hwloc_pack(opal_buffer_t *buffer, const void *src,
                    int32_t num_vals,
                    opal_data_type_t type)
{
    /* NOTE: hwloc defines topology_t as a pointer to a struct! */
    hwloc_topology_t t, *tarray  = (hwloc_topology_t*)src;
    int rc, i;
    char *xmlbuffer=NULL;
    int len;

    for (i=0; i < num_vals; i++) {
        t = tarray[i];


        /* extract an xml-buffer representation of the tree */
        if (0 != hwloc_topology_export_xmlbuffer(t, &xmlbuffer, &len)) {
            return OPAL_ERROR;
        }

        /* add to buffer */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &xmlbuffer, 1, OPAL_STRING))) {
            return rc;
        }

        /* cleanup */
        if (NULL != xmlbuffer) {
            free(xmlbuffer);
        }
    }
    return OPAL_SUCCESS;
}

int opal_hwloc_unpack(opal_buffer_t *buffer, void *dest,
                      int32_t *num_vals,
                      opal_data_type_t type)
{
    /* NOTE: hwloc defines topology_t as a pointer to a struct! */
    hwloc_topology_t t, *tarray  = (hwloc_topology_t*)dest;
    int rc=OPAL_SUCCESS, i, cnt, j;
    char *xmlbuffer=NULL;

    for (i=0, j=0; i < *num_vals; i++) {
        /* unpack the xml string */
        cnt=1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &xmlbuffer, &cnt, OPAL_STRING))) {
            goto cleanup;
        }

        /* convert the xml */
        if (0 != hwloc_topology_init(&t)) {
            rc = OPAL_ERROR;
            goto cleanup;
        }
        if (0 != hwloc_topology_set_xmlbuffer(t, xmlbuffer, strlen(xmlbuffer))) {
            rc = OPAL_ERROR;
            free(xmlbuffer);
            hwloc_topology_destroy(t);
            goto cleanup;
        }
        /* since we are loading this from an external source, we have to
         * explicitly set a flag so hwloc sets things up correctly
         */
        if (0 != hwloc_topology_set_flags(t, HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM)) {
            free(xmlbuffer);
            rc = OPAL_ERROR;
            goto cleanup;
        }
        /* now load the topology */
        if (0 != hwloc_topology_load(t)) {
            free(xmlbuffer);
            rc = OPAL_ERROR;
            goto cleanup;
        }
        if (NULL != xmlbuffer) {
            free(xmlbuffer);
        }

        /* pass it back */
        tarray[i] = t;

        /* track the number added */
        j++;
    }

 cleanup:
    *num_vals = j;
    return rc;
}

int opal_hwloc_copy(hwloc_topology_t *dest, hwloc_topology_t src, opal_data_type_t type)
{
    char *xml;
    int len;

    if (0 != hwloc_topology_export_xmlbuffer(src, &xml, &len)) {
        return OPAL_ERROR;
    }
    if (0 != hwloc_topology_init(dest)) {
        free(xml);
        return OPAL_ERROR;
    }
    if (0 != hwloc_topology_set_xmlbuffer(*dest, xml, len)) {
        hwloc_topology_destroy(*dest);
        free(xml);
        return OPAL_ERROR;
    }
    if (0 != hwloc_topology_load(*dest)) {
        hwloc_topology_destroy(*dest);
        free(xml);
        return OPAL_ERROR;
    }

    free(xml);
    return OPAL_SUCCESS;
}

int opal_hwloc_compare(const hwloc_topology_t topo1,
                       const hwloc_topology_t topo2,
                       opal_data_type_t type)
{
    hwloc_topology_t t1, t2;
    unsigned d1, d2;
    char *x1=NULL, *x2=NULL;
    int l1, l2;
    int s;

    /* stop stupid compiler warnings */
    t1 = (hwloc_topology_t)topo1;
    t2 = (hwloc_topology_t)topo2;

    /* do something quick first */
    d1 = hwloc_topology_get_depth(t1);
    d2 = hwloc_topology_get_depth(t2);
    if (d1 > d2) {
        return OPAL_VALUE1_GREATER;
    } else if (d2 > d1) {
        return OPAL_VALUE2_GREATER;
    }


    /* do the comparison the "cheat" way - get an xml representation
     * of each tree, and strcmp!
     */
    if (0 != hwloc_topology_export_xmlbuffer(t1, &x1, &l1)) {
        return OPAL_EQUAL;
    }
    if (0 != hwloc_topology_export_xmlbuffer(t2, &x2, &l2)) {
        free(x1);
        return OPAL_EQUAL;
    }

    s = strcmp(x1, x2);
    free(x1);
    free(x2);
    if (s > 0) {
        return OPAL_VALUE1_GREATER;
    } else if (s < 0) {
        return OPAL_VALUE2_GREATER;
    }

    return OPAL_EQUAL;
}

static void print_hwloc_obj(char **output, char *prefix, hwloc_obj_t obj)
{
    hwloc_obj_t obj2;
    char string[1024], *tmp, *tmp2, *pfx;
    unsigned i;

    /* print the object type */
    hwloc_obj_type_snprintf(string, 1024, obj, 1);
    asprintf(&pfx, "\n%s\t", (NULL == prefix) ? "" : prefix);
    asprintf(&tmp, "%sType: %s Number of child objects: %u\n%s\tName=%s%s",
             (NULL == prefix) ? "" : prefix, string, obj->arity,
             (NULL == prefix) ? "" : prefix, (NULL == obj->name) ? "NULL" : obj->name, pfx);
    /* print the attributes */
    if (0 < hwloc_obj_attr_snprintf(string, 1024, obj, pfx, 1)) {
        asprintf(&tmp2, "%s%s", tmp, string);
        free(tmp);
        tmp = tmp2;
        /* print the cpuset */
        hwloc_obj_cpuset_snprintf(string, 1024, 1, &obj);
        asprintf(&tmp2, "%s%sCpuset: %s\n", tmp, pfx, string);
    } else {
    /* print the cpuset */
        hwloc_obj_cpuset_snprintf(string, 1024, 1, &obj);
        asprintf(&tmp2, "%sCpuset: %s\n", tmp, string);
    }
    free(tmp);
    tmp = tmp2;
    asprintf(&tmp2, "%s%s", (NULL == *output) ? "" : *output, tmp);
    free(tmp);
    free(pfx);
    asprintf(&pfx, "%s\t", (NULL == prefix) ? "" : prefix);
    for (i=0; i < obj->arity; i++) {
        obj2 = obj->children[i];
        /* print the object */
        print_hwloc_obj(&tmp2, pfx, obj2);
    }
    free(pfx);
    if (NULL != *output) {
        free(*output);
    }
    *output = tmp2;
}

int opal_hwloc_print(char **output, char *prefix, hwloc_topology_t src, opal_data_type_t type)
{
    hwloc_obj_t obj;
    char *tmp=NULL;

    /* get root object */
    obj = hwloc_get_root_obj(src);
    /* print it */
    print_hwloc_obj(&tmp, prefix, obj);
    *output = tmp;
    return OPAL_SUCCESS;
}

int opal_hwloc_size(size_t *size, hwloc_topology_t src, opal_data_type_t type)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

void opal_hwloc_release(opal_dss_value_t *value)
{
}
