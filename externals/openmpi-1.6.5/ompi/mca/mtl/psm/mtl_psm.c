/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "ompi/proc/proc.h"

#include "mtl_psm.h"
#include "mtl_psm_types.h"
#include "mtl_psm_endpoint.h"
#include "mtl_psm_request.h"

mca_mtl_psm_module_t ompi_mtl_psm = {
    {
        8191,         /* max cid - 2^13 - 1 */
        (1UL << 30),  /* max tag value - must allow negatives */
        0,            /* request reserve space */
        0,            /* flags */
        
        ompi_mtl_psm_add_procs,
        ompi_mtl_psm_del_procs,
        ompi_mtl_psm_finalize,
        
        ompi_mtl_psm_send,
        ompi_mtl_psm_isend,
        
        ompi_mtl_psm_irecv,
        ompi_mtl_psm_iprobe,
        
        ompi_mtl_psm_cancel,
        NULL,
        NULL
    }    
};

static
psm_error_t
ompi_mtl_psm_errhandler(psm_ep_t ep, const psm_error_t error, 
			const char *error_string, psm_error_token_t token)
{
    switch (error) {
	/* We don't want PSM to default to exiting when the following errors occur */
	case PSM_EP_DEVICE_FAILURE:
	case PSM_EP_NO_DEVICE:
	case PSM_EP_NO_PORTS_AVAIL:
	case PSM_EP_NO_NETWORK:
	case PSM_EP_INVALID_UUID_KEY:
	  orte_show_help("help-mtl-psm.txt",
			 "unable to open endpoint", true,
			 psm_error_get_string(error));
	    break;

	/* We can't handle any other errors than the ones above */
	default:
	    opal_output(0, "Open MPI detected an unexpected PSM error in opening "
			"an endpoint: %s\n", error_string);
	    return psm_error_defer(token);
	    break;
    }
    return error;
}

int ompi_mtl_psm_progress( void );

int ompi_mtl_psm_module_init(int local_rank, int num_local_procs) { 
    psm_error_t err;
    psm_ep_t	ep; /* endpoint handle */
    psm_mq_t	mq;
    psm_epid_t	epid; /* unique lid+port identifier */
    psm_uuid_t  unique_job_key;
    struct psm_ep_open_opts ep_opt;
    unsigned long long *uu = (unsigned long long *) unique_job_key;
    char *generated_key;
    char env_string[256];
    
    generated_key = getenv("OMPI_MCA_orte_precondition_transports");
    memset(uu, 0, sizeof(psm_uuid_t));
    
    if (!generated_key || (strlen(generated_key) != 33) ||
        sscanf(generated_key, "%016llx-%016llx", &uu[0], &uu[1]) != 2)
    {
      orte_show_help("help-mtl-psm.txt",
		     "no uuid present", true,
		     generated_key ? "could not be parsed from" :
		     "not present in", orte_process_info.nodename);
      return OMPI_ERROR;
      
    }

    /* Handle our own errors for opening endpoints */
    psm_error_register_handler(ompi_mtl_psm.ep, ompi_mtl_psm_errhandler);

    /* Setup MPI_LOCALRANKID and MPI_LOCALNRANKS so PSM can allocate hardware
     * contexts correctly.
     */
    snprintf(env_string, sizeof(env_string), "%d", local_rank);
    setenv("MPI_LOCALRANKID", env_string, 0);
    snprintf(env_string, sizeof(env_string), "%d", num_local_procs);
    setenv("MPI_LOCALNRANKS", env_string, 0);
    
    /* Setup the endpoint options. */
    bzero((void*) &ep_opt, sizeof(ep_opt));
    ep_opt.timeout = ompi_mtl_psm.connect_timeout * 1e9;
    ep_opt.unit = ompi_mtl_psm.ib_unit;
    ep_opt.affinity = PSM_EP_OPEN_AFFINITY_SKIP; /* do not let PSM set affinity */
    ep_opt.shm_mbytes = -1; /* Choose PSM defaults */
    ep_opt.sendbufs_num = -1; /* Choose PSM defaults */

#if PSM_VERNO >= 0x0101   
    ep_opt.network_pkey = ompi_mtl_psm.ib_pkey;
#endif
    
#if PSM_VERNO >= 0x0107
    ep_opt.port = ompi_mtl_psm.ib_port;
    ep_opt.outsl = ompi_mtl_psm.ib_service_level;
#endif

#if PSM_VERNO >= 0x010d
    ep_opt.service_id = ompi_mtl_psm.ib_service_id;
    ep_opt.path_res_type = ompi_mtl_psm.path_res_type;
#endif

    /* Open PSM endpoint */
    err = psm_ep_open(unique_job_key, &ep_opt, &ep, &epid);
    if (err) {
      orte_show_help("help-mtl-psm.txt",
		     "unable to open endpoint", true,
		     psm_error_get_string(err));
      return OMPI_ERROR;
    }

    /* Future errors are handled by the default error handler */
    psm_error_register_handler(ompi_mtl_psm.ep, PSM_ERRHANDLER_DEFAULT);
    
    err = psm_mq_init(ep, 
		      0xffff000000000000ULL, 
		      NULL,
		      0,
		      &mq);
    if (err) {
      orte_show_help("help-mtl-psm.txt",
		     "psm init", true,
		     psm_error_get_string(err));
      return OMPI_ERROR;
    }

    ompi_mtl_psm.ep   = ep;
    ompi_mtl_psm.epid = epid;
    ompi_mtl_psm.mq   = mq;

    if (OMPI_SUCCESS != 
	ompi_modex_send( &mca_mtl_psm_component.super.mtl_version, 
                             &ompi_mtl_psm.epid, 
			     sizeof(psm_epid_t))) {
	opal_output(0, "Open MPI couldn't send PSM epid to head node process"); 
	return OMPI_ERROR;
    }

    /* register the psm progress function */
    opal_progress_register(ompi_mtl_psm_progress);
        
    return OMPI_SUCCESS;
}

int
ompi_mtl_psm_finalize(struct mca_mtl_base_module_t* mtl) { 
    psm_error_t err;

    opal_progress_unregister(ompi_mtl_psm_progress);

    /* free resources */
    err = psm_mq_finalize(ompi_mtl_psm.mq);
    if (err) {
        opal_output(0, "Error in psm_mq_finalize (error %s)\n", 
		    psm_error_get_string(err));
        return OMPI_ERROR;
    }

    err = psm_ep_close(ompi_mtl_psm.ep, PSM_EP_CLOSE_GRACEFUL, 1*1e9);
    if (err) {
        opal_output(0, "Error in psm_ep_close (error %s)\n", 
		    psm_error_get_string(err));
        return OMPI_ERROR;
    }

    err = psm_finalize();
    if (err) {
        opal_output(0, "Error in psm_finalize (error %s)\n", 
		    psm_error_get_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static
const char *
ompi_mtl_psm_connect_error_msg(psm_error_t err)
{
    switch (err) { /* See if we expect the error */
	case PSM_EPID_UNREACHABLE:
	case PSM_EPID_INVALID_NODE:
	case PSM_EPID_INVALID_MTU:
	case PSM_EPID_INVALID_UUID_KEY:
	case PSM_EPID_INVALID_VERSION:
	case PSM_EPID_INVALID_CONNECT:
	    return psm_error_get_string(err);
	    break;
	case PSM_EPID_UNKNOWN:
	    return "Connect status could not be determined "
		   "because of other errors";
	default:
	    return NULL;
    }
}

#ifndef min
#  define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef max
#  define max(a,b) ((a) > (b) ? (a) : (b))
#endif

int
ompi_mtl_psm_add_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs, /*const*/
                      struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    int i,j; 
    int rc;
    psm_epid_t   *epids_in = NULL;
    psm_epid_t	 *epid;
    psm_epaddr_t *epaddrs_out = NULL;
    psm_error_t  *errs_out = NULL, err;
    size_t size;
    int proc_errors[PSM_ERROR_LAST] = { 0 };
    int timeout_in_secs;
    
    assert(mtl == &ompi_mtl_psm.super);
    rc = OMPI_ERR_OUT_OF_RESOURCE;

    errs_out = (psm_error_t *) malloc(nprocs * sizeof(psm_error_t));
    if (errs_out == NULL) {
	goto bail;
    }
    epids_in = (psm_epid_t *) malloc(nprocs * sizeof(psm_epid_t));
    if (epids_in == NULL) {
	goto bail;
    }
    epaddrs_out = (psm_epaddr_t *) malloc(nprocs * sizeof(psm_epaddr_t));
    if (epaddrs_out == NULL) {
	goto bail;
    }
    rc = OMPI_SUCCESS;

    /* Get the epids for all the processes from modex */
    for (i = 0; i < (int) nprocs; i++) {
	rc = ompi_modex_recv(&mca_mtl_psm_component.super.mtl_version, 
				     procs[i], (void**)&epid, &size);
	if (rc != OMPI_SUCCESS || size != sizeof(psm_epid_t)) {
	  return OMPI_ERROR;
	}
	epids_in[i] = *epid;
    }

    timeout_in_secs = max(ompi_mtl_psm.connect_timeout, 0.5 * nprocs);

    psm_error_register_handler(ompi_mtl_psm.ep, PSM_ERRHANDLER_NOP);

    err = psm_ep_connect(ompi_mtl_psm.ep,
			 nprocs,
			 epids_in,
			 NULL, /* connect all */
			 errs_out,
			 epaddrs_out,
			 timeout_in_secs * 1e9);
    if (err) {
	char *errstr = (char *) ompi_mtl_psm_connect_error_msg(err);
	if (errstr == NULL) {
	    opal_output(0, "PSM returned unhandled/unknown connect error: %s\n",
			psm_error_get_string(err));
	}
	for (i = 0; i < (int) nprocs; i++) {
	    psm_error_t thiserr = errs_out[i];
	    errstr = (char *) ompi_mtl_psm_connect_error_msg(thiserr);
	    if (proc_errors[thiserr] == 0) {
		proc_errors[thiserr] = 1;
		opal_output(0, "PSM EP connect error (%s):", 
			    errstr ? errstr : "unknown connect error");
		for (j = 0; j < (int) nprocs; j++) {
		  if (errs_out[j] == thiserr) {
		    opal_output(0, " %s", procs[j]->proc_hostname);
		  }
		}
		opal_output(0, "\n");
	    }
	}

	rc = OMPI_ERROR;
    }
    else {
	/* Default error handling is enabled, errors will not be returned to
	 * user.  PSM prints the error and the offending endpoint's hostname
	 * and exits with -1 */
	psm_error_register_handler(ompi_mtl_psm.ep, PSM_ERRHANDLER_DEFAULT);
		
	/* Fill in endpoint data */
	for (i = 0; i < (int) nprocs; i++) { 
	    mtl_peer_data[i] =
		(mca_mtl_psm_endpoint_t *) OBJ_NEW(mca_mtl_psm_endpoint_t);
	    mtl_peer_data[i]->peer_epid = epids_in[i];
	    mtl_peer_data[i]->peer_addr = epaddrs_out[i];
	}

	rc = OMPI_SUCCESS;
    }
    
bail:
    if (epids_in != NULL) {
	free(epids_in);
    }
    if (errs_out != NULL) {
	free(errs_out);
    }
    if (epaddrs_out != NULL) {
	free(epaddrs_out);
    }

    return rc;
}

int
ompi_mtl_psm_del_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs, 
                      struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    return OMPI_SUCCESS;
}


int ompi_mtl_psm_progress( void ) { 
    psm_error_t err;
    mca_mtl_psm_request_t* mtl_psm_request;
    psm_mq_status_t psm_status;
    psm_mq_req_t req;
    int completed = 1;

    do {
        err = psm_mq_ipeek(ompi_mtl_psm.mq, &req, NULL);
	if (err == PSM_MQ_INCOMPLETE) {
	    return completed;
	} else if (err != PSM_OK) {
	    goto error;
	}
	
	completed++;

	err = psm_mq_test(&req, &psm_status);
	if (err != PSM_OK) {
	    goto error;
	}

        mtl_psm_request = (mca_mtl_psm_request_t*) psm_status.context;

	if (mtl_psm_request->type == OMPI_MTL_PSM_IRECV) {
            ompi_mtl_datatype_unpack(mtl_psm_request->convertor, 
                                     mtl_psm_request->buf,
                                     psm_status.msg_length);

	    mtl_psm_request->super.ompi_req->req_status.MPI_SOURCE =
		    PSM_GET_MQRANK(psm_status.msg_tag);
	    mtl_psm_request->super.ompi_req->req_status.MPI_TAG =
		    PSM_GET_MQUTAG(psm_status.msg_tag);
        mtl_psm_request->super.ompi_req->req_status._ucount = 
            psm_status.nbytes;
	}
	
	if(mtl_psm_request->type == OMPI_MTL_PSM_ISEND) { 
	  if (mtl_psm_request->free_after) {
	    free(mtl_psm_request->buf);
	  }
	}

	switch (psm_status.error_code) {
	    case PSM_OK:
		mtl_psm_request->super.ompi_req->req_status.MPI_ERROR = 
		    OMPI_SUCCESS;
		break;
	    case PSM_MQ_TRUNCATION:
		mtl_psm_request->super.ompi_req->req_status.MPI_ERROR = 
		    MPI_ERR_TRUNCATE;
		break;
	    default:
		mtl_psm_request->super.ompi_req->req_status.MPI_ERROR = 
                        MPI_ERR_INTERN;
	}

	mtl_psm_request->super.completion_callback(&mtl_psm_request->super);

    }
    while (1);

 error: 
    orte_show_help("help-mtl-psm.txt",
		   "error polling network", true,
		   psm_error_get_string(err));
    return 1;
}

