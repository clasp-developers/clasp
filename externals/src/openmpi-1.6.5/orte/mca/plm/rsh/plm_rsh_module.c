/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011      IBM Corporation.  All rights reserved.
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
#include <errno.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/bit_ops.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/rsh/plm_rsh.h"

#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS
static int orte_plm_rsh_launch_threaded(orte_job_t *jdata);
#endif

static int remote_spawn(opal_buffer_t *launch);

orte_plm_base_module_t orte_plm_rsh_module = {
    orte_plm_rsh_init,
    orte_plm_base_set_hnp_name,
#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS
    orte_plm_rsh_launch_threaded,
#else
    orte_plm_rsh_launch,
#endif
    remote_spawn,
    orte_plm_base_orted_terminate_job,
    orte_plm_rsh_terminate_orteds,
    orte_plm_base_orted_kill_local_procs,
    orte_plm_rsh_signal_job,
    orte_plm_rsh_finalize
};

typedef enum {
    ORTE_PLM_RSH_SHELL_BASH = 0,
    ORTE_PLM_RSH_SHELL_ZSH,
    ORTE_PLM_RSH_SHELL_TCSH,
    ORTE_PLM_RSH_SHELL_CSH,
    ORTE_PLM_RSH_SHELL_KSH,
    ORTE_PLM_RSH_SHELL_SH,
    ORTE_PLM_RSH_SHELL_UNKNOWN
} orte_plm_rsh_shell_t;

/* These strings *must* follow the same order as the enum
   ORTE_PLM_RSH_SHELL_* */
static const char * orte_plm_rsh_shell_name[] = {
    "bash",
    "zsh",
    "tcsh",       /* tcsh has to be first otherwise strstr finds csh */
    "csh",
    "ksh",
    "sh",
    "unknown"
};

/*
 * Local functions
 */
static void set_handler_default(int sig);
static orte_plm_rsh_shell_t find_shell(char *shell);
static int find_children(int rank, int parent, int me, int num_procs);

/* local global storage of timing variables */
static struct timeval joblaunchstart, joblaunchstop;

/* local global storage */
static orte_jobid_t active_job=ORTE_JOBID_INVALID;
static orte_jobid_t local_slaves;

/**
 * Init the module
 */
int orte_plm_rsh_init(void)
{
    char *tmp;
    int rc;
    
    /* we were selected, so setup the launch agent */
    if (mca_plm_rsh_component.using_qrsh) {
        /* perform base setup for qrsh */
        asprintf(&tmp, "%s/bin/%s", getenv("SGE_ROOT"), getenv("ARC"));
        if (ORTE_SUCCESS != (rc = orte_plm_base_rsh_launch_agent_setup("qrsh", tmp))) {
            ORTE_ERROR_LOG(rc);
            free(tmp);
            return rc;
        }
        free(tmp);
        /* automatically add -inherit and grid engine PE related flags */
        opal_argv_append_nosize(&orte_plm_globals.rsh_agent_argv, "-inherit");
        /* Don't use the "-noshell" flag as qrsh would have a problem 
         * swallowing a long command */
        opal_argv_append_nosize(&orte_plm_globals.rsh_agent_argv, "-nostdin");
        opal_argv_append_nosize(&orte_plm_globals.rsh_agent_argv, "-V");
        if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
            opal_argv_append_nosize(&orte_plm_globals.rsh_agent_argv, "-verbose");
            tmp = opal_argv_join(orte_plm_globals.rsh_agent_argv, ' ');
            opal_output_verbose(1, orte_plm_globals.output,
                                "%s plm:rsh: using \"%s\" for launching\n",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), tmp);
            free(tmp);
        }
    } else if(mca_plm_rsh_component.using_llspawn) {
        /* perform base setup for llspawn */
        if (ORTE_SUCCESS != (rc = orte_plm_base_rsh_launch_agent_setup("llspawn", NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_output_verbose(1, orte_plm_globals.output,
                            "%s plm:rsh: using \"%s\" for launching\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            orte_plm_globals.rsh_agent_path);
    } else {
        /* not using qrsh or llspawn - use MCA-specified agent */
        if (ORTE_SUCCESS != (rc = orte_plm_base_rsh_launch_agent_setup(NULL, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* we set the local slaves up to have a job family of zero.
     * this provides a convenient way of checking whether or
     * not a process is a local slave
     */
    local_slaves = 0;
    
    return rc;
}


/**
 * Check the Shell variable on the specified node
 */

static int orte_plm_rsh_probe(char *nodename, 
                              orte_plm_rsh_shell_t *shell)
{
    char ** argv;
    int argc, rc = ORTE_SUCCESS, i;
    int fd[2];
    pid_t pid;
    char outbuf[4096];

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: going to check SHELL variable on node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         nodename));

    *shell = ORTE_PLM_RSH_SHELL_UNKNOWN;
    if (pipe(fd)) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: pipe failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }
    if ((pid = fork()) < 0) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: fork failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }
    else if (pid == 0) {          /* child */
        if (dup2(fd[1], 1) < 0) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: dup2 failed with errno=%d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 errno));
            exit(01);
        }
        /* Build argv array */
        argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
        argc = opal_argv_count(orte_plm_globals.rsh_agent_argv);
        opal_argv_append(&argc, &argv, nodename);
        opal_argv_append(&argc, &argv, "echo $SHELL");

        execvp(argv[0], argv);
        exit(errno);
    }
    if (close(fd[1])) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: close failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }

    {
        ssize_t ret = 1;
        char* ptr = outbuf;
        size_t outbufsize = sizeof(outbuf);

        do {
            ret = read (fd[0], ptr, outbufsize-1);
            if (ret < 0) {
                if (errno == EINTR)
                    continue;
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:rsh: Unable to detect the remote shell (error %s)",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     strerror(errno)));
                rc = ORTE_ERR_IN_ERRNO;
                break;
            }
            if( outbufsize > 1 ) {
                outbufsize -= ret;
                ptr += ret;
            }
        } while( 0 != ret );
        *ptr = '\0';
    }
    close(fd[0]);

    if( outbuf[0] != '\0' ) {
        char *sh_name = rindex(outbuf, '/');
        if( NULL != sh_name ) {
            sh_name++; /* skip '/' */
            /* We cannot use "echo -n $SHELL" because -n is not portable. Therefore
             * we have to remove the "\n" */
            if ( sh_name[strlen(sh_name)-1] == '\n' ) {
                sh_name[strlen(sh_name)-1] = '\0';
            }
            /* Search for the substring of known shell-names */
            for (i = 0; i < (int)(sizeof (orte_plm_rsh_shell_name)/
                                  sizeof(orte_plm_rsh_shell_name[0])); i++) {
                if ( 0 == strcmp(sh_name, orte_plm_rsh_shell_name[i]) ) {
                    *shell = (orte_plm_rsh_shell_t)i;
                    break;
                }
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: node %s has SHELL: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         nodename,
                         (ORTE_PLM_RSH_SHELL_UNKNOWN == *shell) ? "UNHANDLED" : (char*)orte_plm_rsh_shell_name[*shell]));

    return rc;
}


/**
 * Callback on daemon exit.
 */

static void orte_plm_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
    orte_std_cntr_t cnt=1;
    uint8_t flag;
    orte_job_t *jdata;
    
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) { /* if abnormal exit */
        /* if we are not the HNP, send a message to the HNP alerting it
         * to the failure
         */
        if (!ORTE_PROC_IS_HNP) {
            opal_buffer_t buf;
            orte_vpid_t *vpid=(orte_vpid_t*)cbdata;
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s daemon %d failed with status %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)*vpid, WEXITSTATUS(status)));
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.pack(&buf, &cnt, 1, ORTE_STD_CNTR);
            flag = 1;
            opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
            opal_dss.pack(&buf, vpid, 1, ORTE_VPID);
            orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
            OBJ_DESTRUCT(&buf);
        } else {
            orte_proc_t *daemon=(orte_proc_t*)cbdata;
            jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s daemon %d failed with status %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)daemon->name.vpid, WEXITSTATUS(status)));
            /* note that this daemon failed */
            daemon->state = ORTE_PROC_STATE_FAILED_TO_START;
            /* increment the #daemons terminated so we will exit properly */
            jdata->num_terminated++;
            /* report that the daemon has failed so we can exit */
            orte_plm_base_launch_failed(ORTE_PROC_MY_NAME->jobid, pid, status, ORTE_JOB_STATE_FAILED_TO_START);
        }
    }

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_plm_rsh_component.lock);

    if (mca_plm_rsh_component.num_children-- >=
        mca_plm_rsh_component.num_concurrent ||
        mca_plm_rsh_component.num_children == 0) {
        opal_condition_signal(&mca_plm_rsh_component.cond);
    }

    OPAL_THREAD_UNLOCK(&mca_plm_rsh_component.lock);

}

static int setup_shell(orte_plm_rsh_shell_t *rshell,
                       orte_plm_rsh_shell_t *lshell,
                       char *nodename, int *argc, char ***argv)
{
    orte_plm_rsh_shell_t remote_shell, local_shell;
    struct passwd *p;
    char *param;
    int rc;

    /* What is our local shell? */
    local_shell = ORTE_PLM_RSH_SHELL_UNKNOWN;
    p = getpwuid(getuid());
    if( NULL == p ) {
        /* This user is unknown to the system. Therefore, there is no reason we
         * spawn whatsoever in his name. Give up with a HUGE error message.
         */
        orte_show_help( "help-plm-rsh.txt", "unknown-user", true, (int)getuid() );
        return ORTE_ERR_FATAL;
    }
    param = p->pw_shell;
    local_shell = find_shell(p->pw_shell);

    /* If we didn't find it in getpwuid(), try looking at the $SHELL
     environment variable (see https://svn.open-mpi.org/trac/ompi/ticket/1060)
     */
    if (ORTE_PLM_RSH_SHELL_UNKNOWN == local_shell && 
        NULL != (param = getenv("SHELL"))) {
        local_shell = find_shell(param);
    }
    
    if (ORTE_PLM_RSH_SHELL_UNKNOWN == local_shell) {
        opal_output(0, "WARNING: local probe returned unhandled shell:%s assuming bash\n",
                    (NULL != param) ? param : "unknown");
        local_shell = ORTE_PLM_RSH_SHELL_BASH;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: local shell: %d (%s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         local_shell, orte_plm_rsh_shell_name[local_shell]));
    
    /* What is our remote shell? */
    if (orte_assume_same_shell) {
        remote_shell = local_shell;
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: assuming same remote shell as local shell",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    } else {
        rc = orte_plm_rsh_probe(nodename, &remote_shell);
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        if (ORTE_PLM_RSH_SHELL_UNKNOWN == remote_shell) {
            opal_output(0, "WARNING: rsh probe returned unhandled shell; assuming bash\n");
            remote_shell = ORTE_PLM_RSH_SHELL_BASH;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: remote shell: %d (%s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         remote_shell, orte_plm_rsh_shell_name[remote_shell]));
    
    /* Do we need to source .profile on the remote side?
     - sh: yes (see bash(1))
     - ksh: yes (see ksh(1))
     - bash: no (see bash(1))
     - [t]csh: no (see csh(1) and tcsh(1))
     - zsh: no (see http://zsh.sourceforge.net/FAQ/zshfaq03.html#l19)
     */
    
    if (ORTE_PLM_RSH_SHELL_SH == remote_shell ||
        ORTE_PLM_RSH_SHELL_KSH == remote_shell) {
        int i;
        char **tmp;
        tmp = opal_argv_split("( test ! -r ./.profile || . ./.profile;", ' ');
        if (NULL == tmp) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (i = 0; NULL != tmp[i]; ++i) {
            opal_argv_append(argc, argv, tmp[i]);
        }
        opal_argv_free(tmp);
    }
    
    /* pass results back */
    *rshell = remote_shell;
    *lshell = local_shell;
    
    return ORTE_SUCCESS;
}

static int setup_launch(int *argcptr, char ***argvptr,
                        char *nodename,
                        int *node_name_index1,
                        int *proc_vpid_index, char *prefix_dir)
{
    int argc;
    char **argv;
    char *param;
    orte_plm_rsh_shell_t remote_shell, local_shell;
    char *lib_base, *bin_base;
    int orted_argc;
    char **orted_argv;
    char *orted_cmd, *orted_prefix, *final_cmd;
    int orted_index;
    int rc;

    
    /* Figure out the basenames for the libdir and bindir.  This
     requires some explanation:
     
     - Use opal_install_dirs.libdir and opal_install_dirs.bindir.
     
     - After a discussion on the devel-core mailing list, the
     developers decided that we should use the local directory
     basenames as the basis for the prefix on the remote note.
     This does not handle a few notable cases (e.g., if the
     libdir/bindir is not simply a subdir under the prefix, if the
     libdir/bindir basename is not the same on the remote node as
     it is here on the local node, etc.), but we decided that
     --prefix was meant to handle "the common case".  If you need
     something more complex than this, a) edit your shell startup
     files to set PATH/LD_LIBRARY_PATH properly on the remove
     node, or b) use some new/to-be-defined options that
     explicitly allow setting the bindir/libdir on the remote
     node.  We decided to implement these options (e.g.,
     --remote-bindir and --remote-libdir) to orterun when it
     actually becomes a problem for someone (vs. a hypothetical
     situation).
     
     Hence, for now, we simply take the basename of this install's
     libdir and bindir and use it to append this install's prefix
     and use that on the remote node.
     */
    
    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);
    
    /*
     * Build argv array
     */
    argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
    argc = opal_argv_count(orte_plm_globals.rsh_agent_argv);
    *node_name_index1 = argc;
    opal_argv_append(&argc, &argv, "<template>");
    
    /* setup the correct shell info */
    if (ORTE_SUCCESS != (rc = setup_shell(&remote_shell, &local_shell,
                                          nodename, &argc, &argv))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* now get the orted cmd - as specified by user - into our tmp array.
     * The function returns the location where the actual orted command is
     * located - usually in the final spot, but someone could
     * have added options. For example, it should be legal for them to use
     * "orted --debug-devel" so they get debug output from the orteds, but
     * not from mpirun. Also, they may have a customized version of orted
     * that takes arguments in addition to the std ones we already support
     */
    orted_argc = 0;
    orted_argv = NULL;
    orted_index = orte_plm_base_setup_orted_cmd(&orted_argc, &orted_argv);
    
    /* look at the returned orted cmd argv to check several cases:
     *
     * - only "orted" was given. This is the default and thus most common
     *   case. In this situation, there is nothing we need to do
     *
     * - something was given that doesn't include "orted" - i.e., someone
     *   has substituted their own daemon. There isn't anything we can
     *   do here, so we want to avoid adding prefixes to the cmd
     *
     * - something was given that precedes "orted". For example, someone
     *   may have specified "valgrind [options] orted". In this case, we
     *   need to separate out that "orted_prefix" section so it can be
     *   treated separately below
     *
     * - something was given that follows "orted". An example was given above.
     *   In this case, we need to construct the effective "orted_cmd" so it
     *   can be treated properly below
     *
     * Obviously, the latter two cases can be combined - just to make it
     * even more interesting! Gotta love rsh/ssh...
     */
    if (0 == orted_index) {
        /* single word cmd - this is the default scenario, but there could
         * be options specified so we need to account for that possibility.
         * However, we don't need/want a prefix as nothing precedes the orted
         * cmd itself
         */
        orted_cmd = opal_argv_join(orted_argv, ' ');
        orted_prefix = NULL;
    } else {
        /* okay, so the "orted" cmd is somewhere in this array, with
         * something preceding it and perhaps things following it.
         */
        orted_prefix = opal_argv_join_range(orted_argv, 0, orted_index, ' ');
        orted_cmd = opal_argv_join_range(orted_argv, orted_index, opal_argv_count(orted_argv), ' ');
    }
    opal_argv_free(orted_argv);  /* done with this */
    
    /* we now need to assemble the actual cmd that will be executed - this depends
     * upon whether or not a prefix directory is being used
     */
    if (NULL != prefix_dir) {
        /* if we have a prefix directory, we need to set the PATH and
         * LD_LIBRARY_PATH on the remote node, and prepend just the orted_cmd
         * with the prefix directory
         */
        char *opal_prefix = getenv("OPAL_PREFIX");
        char* full_orted_cmd = NULL;

        if( NULL != orted_cmd ) {
            if (0 == strcmp(orted_cmd, "orted")) {
                /* if the cmd is our standard one, then add the prefix */
                asprintf(&full_orted_cmd, "%s/%s/%s", prefix_dir, bin_base, orted_cmd);
            } else {
                /* someone specified something different, so don't prefix it */
                full_orted_cmd = strdup(orted_cmd);
            }
        }

        if (ORTE_PLM_RSH_SHELL_SH == remote_shell ||
            ORTE_PLM_RSH_SHELL_KSH == remote_shell ||
            ORTE_PLM_RSH_SHELL_ZSH == remote_shell ||
            ORTE_PLM_RSH_SHELL_BASH == remote_shell) {
            /* if there is nothing preceding orted, then we can just
             * assemble the cmd with the orted_cmd at the end. Otherwise,
             * we have to insert the orted_prefix in the right place
             */
            asprintf (&final_cmd,
                      "%s%s%s PATH=%s/%s:$PATH ; export PATH ; "
                      "LD_LIBRARY_PATH=%s/%s:$LD_LIBRARY_PATH ; export LD_LIBRARY_PATH ; "
                      "DYLD_LIBRARY_PATH=%s/%s:$DYLD_LIBRARY_PATH ; export DYLD_LIBRARY_PATH ; "
                      "%s %s",
                      (opal_prefix != NULL ? "OPAL_PREFIX=" : " "),
                      (opal_prefix != NULL ? opal_prefix : " "),
                      (opal_prefix != NULL ? " ; export OPAL_PREFIX;" : " "),
                      prefix_dir, bin_base,
                      prefix_dir, lib_base,
                      prefix_dir, lib_base,
                      (orted_prefix != NULL ? orted_prefix : " "),
                      (full_orted_cmd != NULL ? full_orted_cmd : " "));
        } else if (ORTE_PLM_RSH_SHELL_TCSH == remote_shell ||
                   ORTE_PLM_RSH_SHELL_CSH == remote_shell) {
            /* [t]csh is a bit more challenging -- we
             have to check whether LD_LIBRARY_PATH
             is already set before we try to set it.
             Must be very careful about obeying
             [t]csh's order of evaluation and not
             using a variable before it is defined.
             See this thread for more details:
             http://www.open-mpi.org/community/lists/users/2006/01/0517.php. */
            /* if there is nothing preceding orted, then we can just
             * assemble the cmd with the orted_cmd at the end. Otherwise,
             * we have to insert the orted_prefix in the right place
             */
            asprintf (&final_cmd,
                      "%s%s%s set path = ( %s/%s $path ) ; "
                      "if ( $?LD_LIBRARY_PATH == 1 ) "
                      "set OMPI_have_llp ; "
                      "if ( $?LD_LIBRARY_PATH == 0 ) "
                      "setenv LD_LIBRARY_PATH %s/%s ; "
                      "if ( $?OMPI_have_llp == 1 ) "
                      "setenv LD_LIBRARY_PATH %s/%s:$LD_LIBRARY_PATH ; "
                      "if ( $?DYLD_LIBRARY_PATH == 1 ) "
                      "set OMPI_have_dllp ; "
                      "if ( $?DYLD_LIBRARY_PATH == 0 ) "
                      "setenv DYLD_LIBRARY_PATH %s/%s ; "
                      "if ( $?OMPI_have_dllp == 1 ) "
                      "setenv DYLD_LIBRARY_PATH %s/%s:$DYLD_LIBRARY_PATH ; "
                      "%s %s",
                      (opal_prefix != NULL ? "setenv OPAL_PREFIX " : " "),
                      (opal_prefix != NULL ? opal_prefix : " "),
                      (opal_prefix != NULL ? " ;" : " "),
                      prefix_dir, bin_base,
                      prefix_dir, lib_base,
                      prefix_dir, lib_base,
                      prefix_dir, lib_base,
                      prefix_dir, lib_base,
                      (orted_prefix != NULL ? orted_prefix : " "),
                      (full_orted_cmd != NULL ? full_orted_cmd : " "));
        } else {
            orte_show_help("help-plm-rsh.txt", "cannot-resolve-shell-with-prefix", true,
                           (NULL == opal_prefix) ? "NULL" : opal_prefix,
                           prefix_dir);
            return ORTE_ERR_SILENT;
        }
        if( NULL != full_orted_cmd ) {
            free(full_orted_cmd);
        }
    } else {
        /* no prefix directory, so just aggregate the result */
        asprintf(&final_cmd, "%s %s",
                 (orted_prefix != NULL ? orted_prefix : ""),
                 (orted_cmd != NULL ? orted_cmd : ""));
    }
    /* now add the final cmd to the argv array */
    opal_argv_append(&argc, &argv, final_cmd);
    free(final_cmd);  /* done with this */
    if (NULL != orted_prefix) free(orted_prefix);
    if (NULL != orted_cmd) free(orted_cmd);
    
    /* if we are not tree launching or debugging, tell the daemon
     * to daemonize so we can launch the next group
     */
    if (!mca_plm_rsh_component.tree_spawn &&
        !orte_debug_flag &&
        !orte_debug_daemons_flag &&
        !orte_debug_daemons_file_flag &&
        !orte_leave_session_attached &&
        /* Daemonize when not using qrsh.  Or, if using qrsh, only
         * daemonize if told to by user with daemonize_qrsh flag. */
        ((!mca_plm_rsh_component.using_qrsh) ||
        (mca_plm_rsh_component.using_qrsh && mca_plm_rsh_component.daemonize_qrsh)) &&
        ((!mca_plm_rsh_component.using_llspawn) ||
        (mca_plm_rsh_component.using_llspawn && mca_plm_rsh_component.daemonize_llspawn))) {
        opal_argv_append(&argc, &argv, "--daemonize");
    }
    
    /*
     * Add the basic arguments to the orted command line, including
     * all debug options
     */
    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          "env",
                                          proc_vpid_index,
                                          true, NULL);
    
    /* ensure that only the ssh plm is selected on the remote daemon */
    opal_argv_append_nosize(&argv, "-mca");
    opal_argv_append_nosize(&argv, "plm");
    opal_argv_append_nosize(&argv, "rsh");
    
    /* in the rsh environment, we can append multi-word arguments
     * by enclosing them in quotes. Check for any multi-word
     * mca params passed to mpirun and include them
     */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        int cnt, i;
        cnt = opal_argv_count(orted_cmd_line);    
        for (i=0; i < cnt; i+=3) {
            /* check if the specified option is more than one word - all
             * others have already been passed
             */
            if (NULL != strchr(orted_cmd_line[i+2], ' ')) {
                /* must add quotes around it */
                asprintf(&param, "\"%s\"", orted_cmd_line[i+2]);
                /* now pass it along */
                opal_argv_append(&argc, &argv, orted_cmd_line[i]);
                opal_argv_append(&argc, &argv, orted_cmd_line[i+1]);
                opal_argv_append(&argc, &argv, param);
                free(param);
            }
        }
    }

    if (ORTE_PLM_RSH_SHELL_SH == remote_shell ||
        ORTE_PLM_RSH_SHELL_KSH == remote_shell) {
        opal_argv_append(&argc, &argv, ")");
    }

    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: final template argv:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == param) ? "NULL" : param));
        if (NULL != param) free(param);
    }
    
    /* all done */
    *argcptr = argc;
    *argvptr = argv;
    return ORTE_SUCCESS;
}

/* actually ssh the child */
static void ssh_child(int argc, char **argv,
                      orte_vpid_t vpid, int proc_vpid_index)
{
    char** env;
    char* var;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    int rc;
    char *exec_path;
    char **exec_argv;
    int fdin;
    sigset_t sigs;

    /* setup environment */
    env = opal_argv_copy(orte_launch_environ);
    
    /* We don't need to sense an oversubscribed condition and set the sched_yield
     * for the node as we are only launching the daemons at this time. The daemons
     * are now smart enough to set the oversubscribed condition themselves when
     * they launch the local procs.
     */
    
    /* We cannot launch locally as this would cause multiple daemons to
     * exist on a node (HNP counts as a daemon). This is taken care of
     * by the earlier check for daemon_preexists, so we only have to worry
     * about remote launches here
     */
    exec_argv = argv;
    exec_path = strdup(orte_plm_globals.rsh_agent_path);
    
    /* pass the vpid */
    rc = orte_util_convert_vpid_to_string(&var, vpid);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "orte_plm_rsh: unable to get daemon vpid as string");
        exit(-1);
    }
    free(argv[proc_vpid_index]);
    argv[proc_vpid_index] = strdup(var);
    free(var);
    
    /* Don't let ssh slurp all of our stdin! */
    fdin = open("/dev/null", O_RDWR);
    dup2(fdin, 0);
    close(fdin);
    
    /* close all file descriptors w/ exception of stdin/stdout/stderr */
    for(fd=3; fd<fdmax; fd++)
        close(fd);
    
    /* Set signal handlers back to the default.  Do this close
     to the execve() because the event library may (and likely
     will) reset them.  If we don't do this, the event
     library may have left some set that, at least on some
     OS's, don't get reset via fork() or exec().  Hence, the
     orted could be unkillable (for example). */
    
    set_handler_default(SIGTERM);
    set_handler_default(SIGINT);
    set_handler_default(SIGHUP);
    set_handler_default(SIGPIPE);
    set_handler_default(SIGCHLD);
    
    /* Unblock all signals, for many of the same reasons that
     we set the default handlers, above.  This is noticable
     on Linux where the event library blocks SIGTERM, but we
     don't want that blocked by the orted (or, more
     specifically, we don't want it to be blocked by the
     orted and then inherited by the ORTE processes that it
     forks, making them unkillable by SIGTERM). */
    sigprocmask(0, 0, &sigs);
    sigprocmask(SIG_UNBLOCK, &sigs, 0);
    
    /* exec the daemon */
    var = opal_argv_join(argv, ' ');
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: executing: (%s) [%s]",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         exec_path, (NULL == var) ? "NULL" : var));
    if (NULL != var) free(var);
    
    execve(exec_path, exec_argv, env);
    opal_output(0, "plm:rsh: execv of %s failed with errno=%s(%d)\n",
                exec_path, strerror(errno), errno);
    exit(-1);
}

static opal_buffer_t collected_uris;

/*
 * launch a set of daemons from a remote daemon
 */
static int remote_spawn(opal_buffer_t *launch)
{
    opal_list_item_t *item;
    orte_vpid_t vpid;
    int node_name_index1;
    int proc_vpid_index;
    char **argv = NULL;
    char *prefix, *hostname;
    int argc;
    int rc;
    bool failed_launch = true;
    pid_t pid;
    orte_std_cntr_t n;
    opal_byte_object_t *bo;

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: remote spawn called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* extract the prefix from the launch buffer */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(launch, &prefix, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }            
    
    /* extract the byte object holding the nidmap */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(launch, &bo, &n, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    /* update our nidmap - this will free data in the byte object */
    if (ORTE_SUCCESS != (rc = orte_ess.update_nidmap(bo))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* clear out any previous child info */
    while (NULL != (item = opal_list_remove_first(&mca_plm_rsh_component.children))) {
        OBJ_RELEASE(item);
    }
    /* reconstruct the child list */
    find_children(0, 0, ORTE_PROC_MY_NAME->vpid, orte_process_info.num_procs);
    
    /* if I have no children, just return */
    if (opal_list_is_empty(&mca_plm_rsh_component.children)) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: remote spawn - have no children!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        failed_launch = false;
        rc = ORTE_SUCCESS;
        goto cleanup;
    }
    
    /* setup the launch */
    if (ORTE_SUCCESS != (rc = setup_launch(&argc, &argv, orte_process_info.nodename, &node_name_index1,
                                           &proc_vpid_index, prefix))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* setup the collection buffer so I can report all the URI's back
     * to the HNP when the launch completes
     */
    OBJ_CONSTRUCT(&collected_uris, opal_buffer_t);
    
    for (item = opal_list_get_first(&mca_plm_rsh_component.children);
         item != opal_list_get_end(&mca_plm_rsh_component.children);
         item = opal_list_get_next(item)) {
        orte_namelist_t *child = (orte_namelist_t*)item;
        vpid = child->name.vpid;
        
        /* get the host where this daemon resides */
        if (NULL == (hostname = orte_ess.proc_get_hostname(&child->name))) {
            opal_output(0, "%s unable to get hostname for daemon %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_VPID_PRINT(vpid));
            rc = ORTE_ERR_NOT_FOUND;
            goto cleanup;
        }
        
        free(argv[node_name_index1]);
        argv[node_name_index1] = strdup(hostname);
        
        /* fork a child to exec the rsh/ssh session */
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }
        
        /* child */
        if (pid == 0) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: launching on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 hostname));
            
            /* do the ssh launch - this will exit if it fails */
            ssh_child(argc, argv, vpid, proc_vpid_index);
            
        }
        /* father */
        OPAL_THREAD_LOCK(&mca_plm_rsh_component.lock);
        /* This situation can lead to a deadlock if '--debug-daemons' is set.
         * However, the deadlock condition is tested at the begining of this
         * function, so we're quite confident it should not happens here.
         */
        if (mca_plm_rsh_component.num_children++ >=
            mca_plm_rsh_component.num_concurrent) {
            opal_condition_wait(&mca_plm_rsh_component.cond, &mca_plm_rsh_component.lock);
        }
        OPAL_THREAD_UNLOCK(&mca_plm_rsh_component.lock);
        
        /* setup callback on sigchild - wait until setup above is complete
         * as the callback can occur in the call to orte_wait_cb
         */
        orte_wait_cb(pid, orte_plm_rsh_wait_daemon, (void*)&vpid);
    }

    failed_launch = false;
    
cleanup:    
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    
    /* check for failed launch */
    if (failed_launch) {
        /* report cannot launch this daemon to HNP */
        opal_buffer_t buf;
        orte_std_cntr_t cnt=1;
        uint8_t flag=1;
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &cnt, 1, ORTE_STD_CNTR);
        opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
        opal_dss.pack(&buf, &vpid, 1, ORTE_VPID);
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
        OBJ_DESTRUCT(&buf);
    }
    
    return rc;
}

static int orted_num_callback = 0;
static bool orted_failed_launch = false;
static void
plm_rsh_report_orted_launch(int status, orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag, void *cbdata)
{
    orte_process_name_t peer;
    char *rml_uri = NULL;
    int rc, idx;
    orte_proc_t *daemon=NULL;
    orte_job_t *jdatorted;
    
    orted_failed_launch = true;
    /* unpack its contact info */
    idx = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &idx, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* set the contact info into the hash table */
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    rc = orte_rml_base_parse_uris(rml_uri, &peer, NULL );
    if( ORTE_SUCCESS != rc ) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch from daemon %s via %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer),
                         ORTE_NAME_PRINT(sender)));
    
    if (NULL == (jdatorted = orte_get_job_data_object(peer.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto CLEANUP;
    }

    /* update state and record for this daemon contact info */
    if (NULL == (daemon = (orte_proc_t*)opal_pointer_array_get_item(jdatorted->procs, peer.vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto CLEANUP;
    }
    daemon->state = ORTE_PROC_STATE_RUNNING;
    daemon->rml_uri = rml_uri;

    /* This is now considered as correctly started even if we fail to decrypt
     * the timing information.
     */
    orted_failed_launch = false;

    /* if we are doing a timing test, unload the start and setup times of the daemon */
    if (orte_timing) {
        /* Deal with the timing if this information is considered useful */
    }
    
    /* if a tree-launch is underway, send the cmd back */
    if (NULL != orte_tree_launch_cmd) {
        orte_rml.send_buffer(&peer, orte_tree_launch_cmd, ORTE_RML_TAG_DAEMON, 0);
    }
    
CLEANUP:

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch %s for daemon %s (via %s) at contact %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orted_failed_launch ? "failed" : "completed",
                         ORTE_NAME_PRINT(&peer),
                         ORTE_NAME_PRINT(sender),
                         (NULL == daemon) ? "UNKNOWN" : daemon->rml_uri));

    if (orted_failed_launch) {
        if( NULL != rml_uri ) free(rml_uri);
        orte_errmgr.incomplete_start(peer.jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
    } else {
        orted_num_callback++;
    }
}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
int orte_plm_rsh_launch(orte_job_t *jdata)
{
    orte_job_map_t *map = NULL;
    int node_name_index1;
    int proc_vpid_index;
    char **argv = NULL;
    char *prefix_dir;
    int argc;
    int rc;
    bool failed_launch = true;
    orte_app_context_t *app;
    orte_node_t *node;
    orte_std_cntr_t nnode;
    orte_jobid_t failed_job;
    orte_job_state_t job_state = ORTE_JOB_NEVER_LAUNCHED;
    bool recv_issued = false;
    
    /* wait for the launch to complete */
    OPAL_THREAD_LOCK(&orte_plm_globals.spawn_lock);
    while (orte_plm_globals.spawn_in_progress) {
        opal_condition_wait(&orte_plm_globals.spawn_in_progress_cond, &orte_plm_globals.spawn_lock);
    }
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output, "released to spawn"));
    orte_plm_globals.spawn_in_progress = true;
    orte_plm_globals.spawn_status = ORTE_ERR_FATAL;
    OPAL_THREAD_UNLOCK(&orte_plm_globals.spawn_lock);
    
    if (NULL == jdata) {
	/* just launching debugger daemons */
	active_job = ORTE_JOBID_INVALID;
	goto launch_apps;
    }

    if (jdata->controls & ORTE_JOB_CONTROL_DEBUGGER_DAEMON) {
        /* debugger daemons */
        active_job = jdata->jobid;
        goto launch_apps;
    }

    if (jdata->controls & ORTE_JOB_CONTROL_LOCAL_SLAVE) {
        /* if this is a request to launch a local slave,
         * then we will not be launching an orted - we will
         * directly ssh the slave process itself. No mapping
         * is performed to support this - the caller must
         * provide all the info required to launch the job,
         * including the target hosts
         */
        rc = orte_plm_base_local_slave_launch(jdata);
        OPAL_THREAD_LOCK(&orte_plm_globals.spawn_lock);
        orte_plm_globals.spawn_in_progress = false;
        OPAL_THREAD_UNLOCK(&orte_plm_globals.spawn_lock);
        return rc;
    }

    /* if we are timing, record the start time */
    if (orte_timing) {
        gettimeofday(&orte_plm_globals.daemonlaunchstart, NULL);
    }

    /* default to declaring the daemon launch as having failed */
    failed_job = ORTE_PROC_MY_NAME->jobid;
    
    if (orte_timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "plm_rsh: could not obtain start time");
            joblaunchstart.tv_sec = 0;
            joblaunchstart.tv_usec = 0;
        }        
    }
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* set the active jobid */
    active_job = jdata->jobid;

    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(active_job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    
    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }
    
    if ((0 < opal_output_get_verbosity(orte_plm_globals.output) ||
         orte_leave_session_attached) &&
        mca_plm_rsh_component.num_concurrent < map->num_new_daemons) {
        /**
        * If we are in '--debug-daemons' we keep the ssh connection 
         * alive for the span of the run. If we use this option 
         * AND we launch on more than "num_concurrent" machines
         * then we will deadlock. No connections are terminated 
         * until the job is complete, no job is started
         * since all the orteds are waiting for all the others
         * to come online, and the others ore not launched because
         * we are waiting on those that have started to terminate
         * their ssh tunnels. :(
         * As we cannot run in this situation, pretty print the error
         * and return an error code.
         */
        orte_show_help("help-plm-rsh.txt", "deadlock-params",
                       true, mca_plm_rsh_component.num_concurrent, map->num_new_daemons);
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }
    
    /*
     * After a discussion between Ralph & Jeff, we concluded that we
     * really are handling the prefix dir option incorrectly. It currently
     * is associated with an app_context, yet it really refers to the
     * location where OpenRTE/Open MPI is installed on a NODE. Fixing
     * this right now would involve significant change to orterun as well
     * as elsewhere, so we will intentionally leave this incorrect at this
     * point. The error, however, is identical to that seen in all prior
     * releases of OpenRTE/Open MPI, so our behavior is no worse than before.
     *
     * A note to fix this, along with ideas on how to do so, has been filed
     * on the project's Trac system under "feature enhancement".
     *
     * For now, default to the prefix_dir provided in the first app_context.
     * Since there always MUST be at least one app_context, we are safe in
     * doing this.
     */
    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0);
    /* we also need at least one node name so we can check what shell is
     * being used, if we have to
     */
    node = NULL;
    for (nnode = 0; nnode < map->nodes->size; nnode++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, nnode))) {
            break;
        }
    }
    if (NULL == node) {
        /* well, if there isn't even one node in the map, then we are hammered */
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }
    prefix_dir = app->prefix_dir;
    
    /* setup the launch */
    if (ORTE_SUCCESS != (rc = setup_launch(&argc, &argv, node->name, &node_name_index1,
                                           &proc_vpid_index, prefix_dir))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if we are tree launching, find our children, get the launch cmd,
     * and setup the recv to hear of any remote failures
     */
    if (mca_plm_rsh_component.tree_spawn) {
        orte_daemon_cmd_flag_t command = ORTE_DAEMON_TREE_SPAWN;
        opal_byte_object_t bo, *boptr;
        orte_job_t *jdatorted;
        
        orte_tree_launch_cmd= OBJ_NEW(opal_buffer_t);
        /* insert the tree_spawn cmd */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(orte_tree_launch_cmd, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(orte_tree_launch_cmd);
            goto cleanup;
        }
        /* pack the prefix since this will be needed by the next wave */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(orte_tree_launch_cmd, &prefix_dir, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(orte_tree_launch_cmd);
            goto cleanup;
        }
        /* construct a nodemap */
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(orte_tree_launch_cmd);
            goto cleanup;
        }
        /* store it */
        boptr = &bo;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(orte_tree_launch_cmd, &boptr, 1, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(orte_tree_launch_cmd);
            free(bo.bytes);
            goto cleanup;
        }
        /* release the data since it has now been copied into our buffer */
        free(bo.bytes);
        /* get the orted job data object */
        if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto cleanup;
        }
        find_children(0, 0, 0, jdatorted->num_procs);
    }
    
    /* set the job state to indicate we attempted to launch */
    job_state = ORTE_JOB_STATE_FAILED_TO_START;

    /* Register a callback to listen for our daemons */
    orted_num_callback = 0;
    orted_failed_launch = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_PERSISTENT, plm_rsh_report_orted_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    recv_issued = true;
    
    /*
     * Iterate through each of the nodes
     */
    for (nnode=0; nnode < map->nodes->size; nnode++) {
        pid_t pid;
        opal_list_item_t *item;
        
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, nnode))) {
            continue;
        }
        
        /* if we are tree launching, only launch our own children */
        if (mca_plm_rsh_component.tree_spawn) {
            for (item = opal_list_get_first(&mca_plm_rsh_component.children);
                 item != opal_list_get_end(&mca_plm_rsh_component.children);
                 item = opal_list_get_next(item)) {
                orte_namelist_t *child = (orte_namelist_t*)item;
                if (child->name.vpid == node->daemon->name.vpid) {
                    goto launch;
                }
            }
            /* didn't find it - ignore this node */
           continue;
        }
    
launch:
        /* if this daemon already exists, don't launch it! */
        if (node->daemon_launched) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh:launch daemon already exists on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            continue;
        }
        
        /* if the node's daemon has not been defined, then we
         * have an error!
         */
        if (NULL == node->daemon) {
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh:launch daemon failed to be defined on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            rc = ORTE_ERR_FATAL;
            goto cleanup;
        }
        
        /* setup node name */
        free(argv[node_name_index1]);
        if (NULL != node->username &&
            0 != strlen (node->username)) {
            asprintf (&argv[node_name_index1], "%s@%s",
                      node->username, node->name);
        } else {
            argv[node_name_index1] = strdup(node->name);
        }

        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: launching on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             node->name));

        /* fork a child to exec the rsh/ssh session */
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }

        /* child */
        if (pid == 0) {
            
            /* do the ssh launch - this will exit if it fails */
            ssh_child(argc, argv, node->daemon->name.vpid, proc_vpid_index);
            
            
        } else { /* father */
            /* indicate this daemon has been launched */
            node->daemon->state = ORTE_PROC_STATE_LAUNCHED;
            /* record the pid */
            node->daemon->pid = pid;
            
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: recording launch of daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&node->daemon->name)));

            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_plm_rsh_wait_daemon, (void*)node->daemon);

            OPAL_THREAD_LOCK(&mca_plm_rsh_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (mca_plm_rsh_component.num_children++ >=
                mca_plm_rsh_component.num_concurrent) {
                opal_condition_wait(&mca_plm_rsh_component.cond, &mca_plm_rsh_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_plm_rsh_component.lock);
            
            /* if required - add delay to avoid problems w/ X11 authentication */
            if (0 < opal_output_get_verbosity(orte_plm_globals.output)
                && mca_plm_rsh_component.delay) {
                sleep(mca_plm_rsh_component.delay);
            }
        }
    }

    /* wait for daemons to callback */
    ORTE_PROGRESSED_WAIT(orted_failed_launch, orted_num_callback, map->num_new_daemons);

launch_apps:
    /* if we get here, then the daemons succeeded, so any failure would now be
     * for the application job
     */
    failed_job = active_job;
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(active_job))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }

    /* wait for the launch to complete */
    OPAL_THREAD_LOCK(&orte_plm_globals.spawn_lock);
    while (!orte_plm_globals.spawn_complete) {
        opal_condition_wait(&orte_plm_globals.spawn_cond, &orte_plm_globals.spawn_lock);
    }
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "completed spawn for job %s", ORTE_JOBID_PRINT(jdata->jobid)));
    orte_plm_globals.spawn_in_progress = false;
    opal_condition_broadcast(&orte_plm_globals.spawn_in_progress_cond);
    OPAL_THREAD_UNLOCK(&orte_plm_globals.spawn_lock);
    
    /* get here if launch went okay */
    failed_launch = false;
    
    if (orte_timing ) {
        if (0 != gettimeofday(&joblaunchstop, NULL)) {
            opal_output(0, "plm_rsh: could not obtain job launch stop time");
        } else {
            opal_output(0, "plm_rsh: total job launch time is %ld usec",
                        (joblaunchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                        (joblaunchstop.tv_usec - joblaunchstart.tv_usec));
        }
    }

 cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(failed_job, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, job_state);
    }

    /* cancel the lingering recv */
    if (recv_issued) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        recv_issued = false;
    }
    
    /* setup a "heartbeat" timer to periodically check on
     * the state-of-health of the orteds, if requested AND
     * we actually launched some daemons!
     */
    if ((NULL != map) && (0 < map->num_new_daemons)) {
        orte_plm_base_start_heart();
    }
    
    return rc;
}


static int find_children(int rank, int parent, int me, int num_procs)
{
    orte_namelist_t *child;
    opal_list_t my_children;
    orte_routed_tree_t* routed_child;
    opal_list_item_t* item;

    OBJ_CONSTRUCT( &my_children, opal_list_t );
    (void)orte_routed.get_routing_tree( &my_children );

    while( NULL != (item = opal_list_remove_first(&my_children)) ) {

        routed_child = (orte_routed_tree_t*)item;

        child = OBJ_NEW(orte_namelist_t);
        child->name.jobid = ORTE_PROC_MY_NAME->jobid;
        child->name.vpid = routed_child->vpid;

        OPAL_OUTPUT_VERBOSE((3, orte_plm_globals.output,
                             "%s plm:rsh find-children found child %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&child->name)));
                    
        opal_list_append(&mca_plm_rsh_component.children, &child->item);
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT( &my_children );

    return parent;
}


/**
 * Terminate the orteds for a given job
 */
int orte_plm_rsh_terminate_orteds(void)
{
    int rc;
    
    /* now tell them to die - we need them to "phone home", though,
     * so we can know that they have exited
     */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit(ORTE_DAEMON_EXIT_CMD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_plm_rsh_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_plm_rsh_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Handle threading issues.
 */

#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS

struct orte_plm_rsh_stack_t {
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_plm_rsh_stack_t orte_plm_rsh_stack_t;

static void orte_plm_rsh_stack_construct(orte_plm_rsh_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_plm_rsh_stack_destruct(orte_plm_rsh_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_plm_rsh_stack_t,
    opal_object_t,
    orte_plm_rsh_stack_construct,
    orte_plm_rsh_stack_destruct);

static void orte_plm_rsh_launch_cb(int fd, short event, void* args)
{
    orte_plm_rsh_stack_t *stack = (orte_plm_rsh_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_plm_rsh_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_plm_rsh_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_plm_rsh_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_plm_rsh_stack_t);

    stack.jobid = jobid;
    if( opal_event_progress_thread() ) {
        stack.rc = orte_plm_rsh_launch( jobid );
    } else {
        opal_evtimer_set(&event, orte_plm_rsh_launch_cb, &stack);
        opal_evtimer_add(&event, &tv);

        OPAL_THREAD_LOCK(&stack.mutex);
        while (stack.complete == false) {
            opal_condition_wait(&stack.cond, &stack.mutex);
        }
        OPAL_THREAD_UNLOCK(&stack.mutex);
    }
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif


static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}


static orte_plm_rsh_shell_t find_shell(char *shell) 
{
    int i         = 0;
    char *sh_name = NULL;

    if( (NULL == shell) || (strlen(shell) == 1) ) {
        /* Malformed shell */
        return ORTE_PLM_RSH_SHELL_UNKNOWN;
    }

    sh_name = rindex(shell, '/');
    if( NULL == sh_name ) {
        /* Malformed shell */
        return ORTE_PLM_RSH_SHELL_UNKNOWN;
    }

    /* skip the '/' */
    ++sh_name;
    for (i = 0; i < (int)(sizeof (orte_plm_rsh_shell_name) /
                          sizeof(orte_plm_rsh_shell_name[0])); ++i) {
        if (0 == strcmp(sh_name, orte_plm_rsh_shell_name[i])) {
            return (orte_plm_rsh_shell_t)i;
        }
    }

    /* We didn't find it */
    return ORTE_PLM_RSH_SHELL_UNKNOWN;
}

