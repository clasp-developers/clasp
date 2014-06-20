/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <alps/apInfo.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ras_alps.h"
#include "orte/mca/ras/base/ras_private.h"


/*
 * Local functions
 */
static int orte_ras_alps_allocate(opal_list_t *nodes);
static int orte_ras_alps_finalize(void);
static char *ras_alps_getline(FILE *fp);
static int orte_ras_alps_read_appinfo_file(opal_list_t *nodes, char *filename, unsigned int *uMe);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_alps_module = {
    orte_ras_alps_allocate,
    orte_ras_alps_finalize
};

/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int orte_ras_alps_allocate(opal_list_t *nodes)
{
    const char  alps_sysconfig[] = "/etc/sysconfig/alps"; /** Get ALPS scheduler information
                                                              file pathname from system configuration. **/
    unsigned int alps_res_id;
    int         ret;
    FILE        *fp;
    char        *alps_batch_id;
    char        *endptr;
    char        *str;
    char        *alps_config_str;
    
    alps_batch_id = getenv("OMPI_ALPS_RESID");
    if (NULL == alps_batch_id) {
        orte_show_help("help-ras-alps.txt", "alps-env-var-not-found", 1,
                       "OMPI_ALPS_RESID");
        return ORTE_ERR_NOT_FOUND;
    }

    alps_res_id=(unsigned int)strtol(alps_batch_id, &endptr, 10);

    if (alps_batch_id[0] == '\0' || endptr[0] != '\0') {
        orte_show_help("help-ras-alps.txt", "alps-env-var-invalid", 1,
                       alps_batch_id);
        return ORTE_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, orte_ras_base.ras_output,
                        "ras:alps:allocate: Using ALPS configuration file: \"%s\"",
                        alps_sysconfig);

    fp = fopen(alps_sysconfig, "r");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }

    while( (alps_config_str=ras_alps_getline(fp)) ) {

        char    *cpq;
        char    *cpr;

        cpq=strchr( alps_config_str, '#' ); /* Parse comments, actually ANY # */
        cpr=strchr( alps_config_str, '=' ); /* Parse for variables            */
        if( !cpr ||                         /* Skip if not definition         */
            (cpq && cpq<cpr) ) {            /* Skip if commented              */

            free(alps_config_str);
            continue;
        }
        for( cpr--;                         /* Kill trailing whitespace       */
             (*cpr==' ' || *cpr=='\t'); cpr-- ) ;
        for( cpq=alps_config_str;           /* Kill leading whitespace        */
             (*cpq==' ' || *cpq=='\t'); cpq++ ) ;

/*      Filter to needed variable.                                            */
        if(strncmp( cpq, "ALPS_SHARED_DIR_PATH", 20 )) {

            free(alps_config_str);
            continue;
        }

        if( !(cpq=strchr( cpr, '"' )) ) {   /* Can't find pathname start      */

            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        if( !(cpr=strchr( ++cpq, '"' )) ) { /* Can't find pathname end        */

            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        *cpr='\0';
        if( strlen(cpq)+8 > PATH_MAX ) {      /* Bad configuration            */

            errno=ENAMETOOLONG;
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        asprintf(&str, "%s/appinfo", cpq);
        if (NULL == str) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        free(alps_config_str);
        break;
    }
    fclose(fp);
    
    opal_output_verbose(1, orte_ras_base.ras_output,
                        "ras:alps:allocate: Located ALPS scheduler file: \"%s\"", str);

/*  Parse ALPS scheduler information file (appinfo) for node list.            */
    if (ORTE_SUCCESS != (ret = orte_ras_alps_read_appinfo_file(nodes, str, &alps_res_id))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    free(str);

cleanup:

    /* All done */
    if (ORTE_SUCCESS == ret) {
        opal_output_verbose(1, orte_ras_base.ras_output,
                             "ras:alps:allocate: success");
    } else {
        opal_output_verbose(1, orte_ras_base.ras_output,
                             "ras:alps:allocate: failure (base_allocate_nodes=%d)", ret);
    }
    return ret;
}


#define RAS_BASE_FILE_MAX_LINE_LENGTH   PATH_MAX*2

static char *ras_alps_getline(FILE *fp)
{
    char *ret, *buff = NULL;
    char input[RAS_BASE_FILE_MAX_LINE_LENGTH+1];
    
    ret = fgets(input, RAS_BASE_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
    }
    
    return buff;
}


static int orte_ras_alps_read_appinfo_file(opal_list_t *nodes, char *filename, unsigned int *uMe)
{
    int             iq;
    int             ix;
    int             iFd;                    /* file descriptor for appinfo    */
    int             iTrips;                 /* counter appinfo read attempts  */
    int             max_appinfo_read_attempts;
    struct stat     ssBuf;                  /* stat buffer                    */
    size_t          szLen;                  /* size of appinfo (file)         */
    off_t           oNow;                   /* current appinfo data offset    */
    off_t           oInfo=sizeof(appInfoHdr_t);
    off_t           oDet=sizeof(appInfo_t);
    off_t           oSlots;
    off_t           oEntry;
    int32_t         sNodes=0;
    char            *cpBuf;
    char            *hostname;
    orte_node_t     *node = NULL;
    appInfoHdr_t    *apHdr;                 /* ALPS header structure          */
    appInfo_t       *apInfo;                /* ALPS table info structure      */
    cmdDetail_t     *apDet;                 /* ALPS command details           */
    placeList_t     *apSlots;               /* ALPS node specific info        */

    orte_ras_alps_get_appinfo_attempts(&max_appinfo_read_attempts);
    oNow=0;
    iTrips=0;
    while(!oNow) {                          /* Until appinfo read is complete */
        iTrips++;                           /* Increment trip count           */

        iFd=open( filename, O_RDONLY );
        if( iFd==-1 ) {                     /* If file absent, ALPS is down   */
            opal_output_verbose(1, orte_ras_base.ras_output,
                                "ras:alps:allocate: ALPS information open failure");
            usleep(iTrips*50000);           /* Increasing delays, .05 s/try   */

/*          Fail only when number of attempts have been exhausted.            */
            if( iTrips <= max_appinfo_read_attempts ) continue;
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        if( fstat( iFd, &ssBuf )==-1 ) {    /* If stat fails, access denied   */

            ORTE_ERROR_LOG(ORTE_ERR_NOT_AVAILABLE);
            return ORTE_ERR_NOT_AVAILABLE;
        }

        szLen=ssBuf.st_size;                /* Get buffer size                */
        cpBuf=malloc(szLen+1);              /* Allocate buffer                */
        if (NULL == cpBuf) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

/*      Repeated attempts to read appinfo, with an increasing delay between   *
 *      successive attempts to allow scheduler I/O a chance to complete.      */
        if( (oNow=read( iFd, cpBuf, szLen ))!=(off_t)szLen ) {

/*          This is where apstat fails; we will record it and try again.      */
            opal_output_verbose(1, orte_ras_base.ras_output,
                         "ras:alps:allocate: ALPS information read failure: %ld bytes", oNow);

            free(cpBuf);                    /* Free (old) buffer              */
            close(iFd);                     /* Close (old) descriptor         */
            oNow=0;                         /* Reset byte count               */
            usleep(iTrips*50000);           /* Increasing delays, .05 s/try   */

/*          Fail only when number of attempts have been exhausted.            */
            if( iTrips<=max_appinfo_read_attempts ) continue;
            ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
            return ORTE_ERR_FILE_READ_FAILURE;
        }
    }
    close(iFd);

/*  Now that we have the scheduler information, we just have to parse it for  *
 *  the data that we seek.                                                    */
    oNow=0;
    apHdr=(appInfoHdr_t *)cpBuf;

/*  Header info (apHdr) tells us how many entries are in the file:            *
 *                                                                            *
 *      apHdr->apNum                                                          */

    for( iq=0; iq<apHdr->apNum; iq++ ) {    /*  Parse all entries in file     */

/*      Just at this level, a lot of information is available:                *
 *                                                                            *
 *          apInfo->apid         ... ALPS job ID                              *
 *          apInfo->resId        ... ALPS reservation ID                      *
 *          apInfo->numCmds      ... Number of executables                    *
 *          apInfo->numPlaces    ... Number of PEs                            */
        apInfo=(appInfo_t *)(cpBuf+oNow+oInfo);

/*      Calculate the dependent offsets.                                      */
        oSlots=sizeof(cmdDetail_t)*apInfo->numCmds;
        oEntry=sizeof(placeList_t)*apInfo->numPlaces;

/*      Also, we can extract details of commands currently running on nodes:  *
 *                                                                            *
 *          apDet[].fixedPerNode ... PEs per node                             *
 *          apDet[].nodeCnt      ... number of nodes in use                   *
 *          apDet[].memory       ... MB/PE memory limit                       *
 *          apDet[].cmd          ... command being run                        */
        apDet=(cmdDetail_t *)(cpBuf+oNow+oInfo+oDet);

/*      Finally, we get to the actual node-specific information:              *
 *                                                                            *
 *          apSlots[ix].cmdIx    ... index of apDet[].cmd                     *
 *          apSlots[ix].nid      ... NodeID (NID)                             *
 *          apSlots[ix].procMask ... mask for processors... need 16-bit shift */
        apSlots=(placeList_t *)(cpBuf+oNow+oInfo+oDet+oSlots);

        oNow+=(oDet+oSlots+oEntry);         /* Target next slot               */

        if( apInfo->resId != *uMe ) continue; /* Filter to our reservation Id */

        for( ix=0; ix<apInfo->numPlaces; ix++ ) {

            opal_output_verbose(5, orte_ras_base.ras_output,
                             "ras:alps:read_appinfo: got NID %d", apSlots[ix].nid);

            asprintf( &hostname, "%d", apSlots[ix].nid );
            if (NULL == hostname) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

/*          If this matches the prior nodename, just add to the slot count.   */
            if( NULL!=node && !strcmp(node->name, hostname) ) {

                free(hostname);             /* free hostname since not needed */
                ++node->slots;
            } else {                        /* must be new, so add to list    */

                opal_output_verbose(1, orte_ras_base.ras_output,
                             "ras:alps:read_appinfo: added NID %d to list", apSlots[ix].nid);

                node = OBJ_NEW(orte_node_t);
                node->name = hostname;
                node->launch_id = sNodes;
                node->slots_inuse = 0;
                node->slots_max = 0;
                node->slots = 1;
                opal_list_append(nodes, &node->super);
                sNodes++;                   /* Increment the node count       */
            }
        }
        break;                              /* Extended details ignored       */
    }
    free(cpBuf);                            /* Free the buffer                */

    return ORTE_SUCCESS;
}

/*
 * There's really nothing to do here
 */
static int orte_ras_alps_finalize(void)
{
    opal_output_verbose(1, orte_ras_base.ras_output,
                         "ras:alps:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}

