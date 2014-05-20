/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/version.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>

#include "opal/dss/dss.h"
#include "opal/class/opal_list.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal.h"
#include "opal/util/cmd_line.h"
#include "opal/mca/base/base.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"

/*
 * Globals
 */
typedef struct {
    opal_list_item_t item;
    char *name;
    opal_list_t frameworks;
} orte_profile_node_t;
static void node_constructor(orte_profile_node_t *ptr)
{
    ptr->name = NULL;
    OBJ_CONSTRUCT(&ptr->frameworks, opal_list_t);
}
static void node_destructor(orte_profile_node_t *ptr)
{
    if (NULL != ptr->name) {
        free(ptr->name);
    }
    OBJ_DESTRUCT(&ptr->frameworks);
}
OBJ_CLASS_INSTANCE(orte_profile_node_t,
                   opal_list_item_t,
                   node_constructor,
                   node_destructor);

typedef struct {
    opal_list_item_t item;
    int num_nodes;
    char *framework;
    char *component;
    char *params;
} orte_profile_t;
static void profile_constructor(orte_profile_t *ptr)
{
    ptr->num_nodes = 0;
    ptr->framework = NULL;
    ptr->component = NULL;
    ptr->params = NULL;
}
static void profile_destructor(orte_profile_t *ptr)
{
    if (NULL != ptr->framework) {
        free(ptr->framework);
    }
    if (NULL != ptr->component) {
        free(ptr->component);
    }
    if (NULL != ptr->params) {
        free(ptr->params);
    }
}
OBJ_CLASS_INSTANCE(orte_profile_t,
                   opal_list_item_t,
                   profile_constructor,
                   profile_destructor);

static void read_file(opal_list_t *nodes, FILE *fp);

/* global variables */
static bool help = false;
static bool version = false;
static bool verbose = false;
static bool configout = false;
static char *profilefile = NULL;
static bool report = false;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', "help", "help", 0,
      &help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 'V', "version", "version", 0,
      &version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },

    { NULL, NULL, NULL, '\0', "verbose", "verbose", 0,
      &verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },

    { NULL, NULL, NULL, '\0', "config", "config", 0,
      &configout, OPAL_CMD_LINE_TYPE_BOOL,
      "Print framework/component usage" },

    { NULL, NULL, NULL, '\0', "profile", "profile", 1,
      &profilefile, OPAL_CMD_LINE_TYPE_STRING,
      "File to update with system profile parameters" },

    { NULL, NULL, NULL, '\0', "report", "report", 0,
      &report, OPAL_CMD_LINE_TYPE_BOOL,
      "Print out a report of the data in the given profile file" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int main(int argc, char *argv[])
{
    opal_list_t nodes, frames;
    opal_list_item_t *item, *item2, *itemold;
    orte_profile_node_t *node;
    orte_profile_t *profile, *frame;
    int ret;
    int len;
    opal_cmd_line_t cmd_line;
    char *args = NULL;
    char *configfile=NULL;
    char *cmd;
    
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (OPAL_SUCCESS != (ret = opal_cmd_line_parse(&cmd_line, true,
                                                   argc, argv)) ) {
        return ret;
    }

    /* init enough of opal to use a few utilities */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        fprintf(stderr, "OPAL failed to initialize -- ompi-profiler aborting\n");
        exit(1);
    }
    
    /* check for some simple options */
    if (help) {
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-ompi-profiler.txt", "ompi-profiler:usage", false,
                       OPAL_VERSION, args, PACKAGE_BUGREPORT);
        free(args);
        
        /* If someone asks for help, that should be all we do */
        exit(0);
        
    }
    
    if (version) {
        opal_show_help("help-ompi-profiler.txt", "ompi-profiler:version", false,
                       OPAL_VERSION, PACKAGE_BUGREPORT);
        exit(0);        
    }
    
    if (report) {
        int fd;
        int32_t num_bytes, cnt;
        char *nodename, *attr;
        opal_byte_object_t bo;
        opal_buffer_t buf;
        uint8_t *dump;
        
        /* just read the given file and print out a report */
        if (NULL == profilefile) {
            opal_show_help("help-ompi-profiler.txt", "ompi-profiler:report-wo-file", false);
            exit(1);
        }
        
        fd = open(profilefile, O_RDONLY);
        if (fd < 0) {
            opal_show_help("help-ompi-profiler.txt", "ompi-profiler:report-file-not-found", false);
            exit(1);
        }
        
        /* loop through file until end */
        while (0 < read(fd, &bo.size, sizeof(bo.size))) {
            /* this is the number of bytes in the byte object */
            bo.bytes = malloc(bo.size);
            if (0 > read(fd, bo.bytes, bo.size)) {
                fprintf(stderr, "ompi-profiler: unable to read file\n");
                close(fd);
                exit(1);
            }
            /* setup to unpack the object */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.load(&buf, bo.bytes, bo.size);
            /* unpack the nodename */
            cnt = 1;
            if (OPAL_SUCCESS != opal_dss.unpack(&buf, &nodename, &cnt, OPAL_STRING)) {
                fprintf(stderr, "ompi-profiler: could not unpack node name\n");
                close(fd);
                exit(1);
            }
            /* loop through the rest of the object to unpack the attr's themselves */
            while (OPAL_SUCCESS == opal_dss.unpack(&buf, &attr, &cnt, OPAL_STRING)) {
                /* read the number of bytes in the blob */
                cnt = 1;
                if (OPAL_SUCCESS != opal_dss.unpack(&buf, &num_bytes, &cnt, OPAL_INT32)) {
                    fprintf(stderr, "ompi-profiler: data size not found\n");
                    close(fd);
                    exit(0);
                }
                /* unpack the bytes just so we can dump them */
                dump = malloc(num_bytes);
                if (OPAL_SUCCESS != opal_dss.unpack(&buf, dump, &num_bytes, OPAL_BYTE)) {
                    fprintf(stderr, "ompi-profiler: data not found\n");
                    close(fd);
                    exit(0);
                }
                free(dump);
                /* report the results */
                fprintf(stdout, "Node %s reported %d bytes for attribute %s\n",
                        nodename, num_bytes, attr);
                free(attr);
            }
            free(nodename);
            OBJ_DESTRUCT(&buf);
        }
        close(fd);
        exit(0);
    }
    
    /* do a quick sanity check - since they didn't want a report, see if they don't
     * want -anything-
     */
    if (!configout && NULL == profilefile) {
        /* save us the annoyance - you have to want -something-! */
        fprintf(stderr, "ompi-profiler: no options specified - aborting\n");
        exit(1);
    }
    
    /* setup the cmd to execute */
    if (configout) {
        asprintf(&configfile, "profiler.%d", getpid());
    } else {
        configfile = strdup("/dev/null");
    }
    if (NULL != profilefile) {
        asprintf(&cmd, "mpirun -pernode -mca opal_profile 1 -mca opal_profile_file %s -mca grpcomm basic ompi-probe >& %s",
                 profilefile, configfile);
    } else {
        asprintf(&cmd, "mpirun -pernode -mca opal_profile 1 -mca grpcomm basic ompi-probe >& %s",
                 configfile);
    }
    
    if (verbose) {
        fprintf(stderr, "ompi-profiler: executing %s\n", cmd);
    }
    
    /* execute it */
    if (0 > system(cmd)) {
        fprintf(stderr, "ompi-profiler: could not execute cmd %s\n", cmd);
        free(cmd);
        goto CLEANUP;
    }
    free(cmd);
    
    /* did they want the configuration output? */
    if (configout) {
        FILE *fp;
        struct stat buf;
        
        /* does the file already exist? */
        if (0 != stat(configfile, &buf)) {
            /* file must not have been created */
            fprintf(stderr, "Temporary output file %s could not be found - config report cannot be generated\n", configfile);
            goto CLEANUP;
        }
        
        /* yes - read the info so we can output it */
        fp = fopen(configfile, "r");
        if (NULL == fp) {
            fprintf(stderr, "Impossible to open the file %s in read mode\n", configfile );
            goto CLEANUP;
        }
        OBJ_CONSTRUCT(&nodes, opal_list_t);
        read_file(&nodes, fp);
        fclose(fp);
        
        /* setup a list of framework info */
        OBJ_CONSTRUCT(&frames, opal_list_t);
        len = opal_list_get_size(&nodes);
        /* convert the results over to the new list */
        while (NULL != (item = opal_list_remove_first(&nodes))) {
            node = (orte_profile_node_t*)item;
            /* loop through this node's frameworks */
            item2 = opal_list_get_first(&node->frameworks);
            while (item2 != opal_list_get_end(&node->frameworks)) {
                profile = (orte_profile_t*)item2;
                /* is this framework already in our list? */
                for (itemold = opal_list_get_first(&frames);
                     itemold != opal_list_get_end(&frames);
                     itemold = opal_list_get_next(itemold)) {
                    frame = (orte_profile_t*)itemold;
                    if (0 == strcmp(profile->framework, frame->framework) &&
                        0 == strcmp(profile->component, frame->component)) {
                        /* all matches - increment # matches */
                        frame->num_nodes++;
                        goto COMPLETE;
                    }
                }
                /* get here if the framework/component is new */
                frame = OBJ_NEW(orte_profile_t);
                frame->num_nodes++;
                frame->framework = strdup(profile->framework);
                frame->component = strdup(profile->component);
                opal_list_append(&frames, &frame->item);
            COMPLETE:
                item2 = opal_list_get_next(item2);
            }
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&nodes);
        /* output the list */
        while (NULL != (itemold = opal_list_remove_first(&frames))) {
            frame = (orte_profile_t*)itemold;
            if (len == frame->num_nodes) {
                fprintf(stderr, "All nodes use framework %s component %s\n", frame->framework, frame->component);
            } else {
                fprintf(stderr, "%d nodes use framework %s component %s\n", frame->num_nodes, frame->framework, frame->component);
            }
            OBJ_RELEASE(frame);
        }
        OBJ_DESTRUCT(&frames);
    }
    
CLEANUP:
    /* remove the file */
    if (NULL != configfile) {
        if (0 != strcmp("/dev/null", configfile)) {
            unlink(configfile);
        }
        free(configfile);
    }
    return 0;
}

static void read_file(opal_list_t *nodes, FILE *fp)
{
    char line[1024];
    char *endprefix, *endnodename, *data, *nodename;
    char **inputs;
    opal_list_item_t *item;
    orte_profile_node_t *node;
    orte_profile_t *profile;
    int len;
    
    memset(line, 0, sizeof(line));
    while (NULL != fgets(line, sizeof(line), fp)) {
        /* get the length of the line */
        len = strlen(line);
        /* remove any trailing newline */
        if (line[len-1] == '\n') {
            line[len-1] = '\0';
        }
        if ('[' != line[0]) {
            /* indicates empty line - ignore it */
            continue;
        }
        if (NULL == (endprefix = strchr(line, ']'))) {
            fprintf(stderr, "ompi-profiler: read bad input for ] %s\n", line);
            continue;
        }
        *endprefix = '\0';
        /* break the prefix at the colon - we don't need the pid */
        if (NULL == (endnodename = strchr(line, ':'))) {
            fprintf(stderr, "ompi-profiler: read bad input for : %s\n", line);
            continue;
        }
        *endnodename = '\0';
        nodename = &line[1];
        /* is this node already in our list */
        for (item = opal_list_get_first(nodes);
             item != opal_list_get_end(nodes);
             item = opal_list_get_next(item)) {
            node = (orte_profile_node_t*)item;
            if (0 == strcmp(node->name, nodename)) {
                /* already present - just add to it */
                goto PROCESS;
            }
        }
        /* if we got here, then it wasn't found */
        node = OBJ_NEW(orte_profile_node_t);
        node->name = strdup(nodename);
        opal_list_append(nodes, &node->item);
    PROCESS:
        /* point to the rest of the data */
        data = endprefix;
        data += 2; /* get past space */
        /* use an opal utility to parse it */
        if ((NULL == (inputs = opal_argv_split(data, ':'))) ||
            opal_argv_count(inputs) < 2) {
            fprintf(stderr, "ompi-profiler: read bad input for second : %s\n", data);
            opal_argv_free(inputs);
            continue;
        }
        /* first entry must be the framework - see if we already have it */
        for (item = opal_list_get_first(&node->frameworks);
             item != opal_list_get_end(&node->frameworks);
             item = opal_list_get_next(item)) {
            profile = (orte_profile_t*)item;
            if (0 == strcmp(inputs[0], profile->framework)) {
                /* this will happen if mpirun outputs some of the
                 * frameworks AND a proc is local to mpirun
                 */
                goto SKIP;
            }
        }
        profile = OBJ_NEW(orte_profile_t);
        profile->framework = strdup(inputs[0]);
        /* second entry is component */
        profile->component = strdup(inputs[1]);
        /* if there is anything more, just save it */
        if (NULL != inputs[2]) {
            profile->params = opal_argv_join(&inputs[2], ':');
        }
        opal_list_append(&node->frameworks, &profile->item);
    SKIP:
        opal_argv_free(inputs);
        
        memset(line, 0, sizeof(line));
    }
}

