/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"
#include "opal/util/if.h"

#include "opal/dss/dss.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"
#include "orte/util/dash_host/dash_host.h"

#include "orte/mca/plm/base/plm_private.h"

static char **search(const char* agent_list, const char *path);

int orte_plm_base_rsh_launch_agent_lookup(const char *agent_list, char *path)
{
    char **tmp;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:rsh_lookup on agent %s path %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == agent_list) ? orte_rsh_agent : agent_list,
                         (NULL == path) ? "NULL" : path));
    if (NULL == (tmp = search(agent_list, path))) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* if we got here, then one of the given agents could be found */
    opal_argv_free(tmp);
    return ORTE_SUCCESS;
}

int orte_plm_base_rsh_launch_agent_setup(const char *agent, char *path)
{
    char *bname;
    int i;
    
    /* if no agent was provided, then report not found */
    if (NULL == orte_rsh_agent && NULL == agent) {
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* search for the argv */
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:rsh_setup on agent %s path %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == agent) ? orte_rsh_agent : agent,
                         (NULL == path) ? "NULL" : path));
    orte_plm_globals.rsh_agent_argv = search(agent, path);
    
    if (0 == opal_argv_count(orte_plm_globals.rsh_agent_argv)) {
        /* nothing was found */
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* see if we can find the agent in the path */
    orte_plm_globals.rsh_agent_path = 
        opal_path_findv(orte_plm_globals.rsh_agent_argv[0], X_OK,
                        environ, path);

    if (NULL == orte_plm_globals.rsh_agent_path) {
        /* not an error - just report not found */
        opal_argv_free(orte_plm_globals.rsh_agent_argv);
        return ORTE_ERR_NOT_FOUND;
    }
    
    bname = opal_basename(orte_plm_globals.rsh_agent_argv[0]);
    if (NULL != bname && 0 == strcmp(bname, "ssh")) {
        /* if xterm option was given, add '-X', ensuring we don't do it twice */
        if (NULL != orte_xterm) {
            opal_argv_append_unique_nosize(&orte_plm_globals.rsh_agent_argv, "-X", false);
        } else if (0 >= opal_output_get_verbosity(orte_plm_globals.output)) {
            /* if debug was not specified, and the user didn't explicitly
             * specify X11 forwarding/non-forwarding, add "-x" if it
             * isn't already there (check either case)
             */
            for (i = 1; NULL != orte_plm_globals.rsh_agent_argv[i]; ++i) {
                if (0 == strcasecmp("-x", 
                                    orte_plm_globals.rsh_agent_argv[i])) {
                    break;
                }
            }
            if (NULL == orte_plm_globals.rsh_agent_argv[i]) {
                opal_argv_append_nosize(&orte_plm_globals.rsh_agent_argv, "-x");
            }
        }
    }
    
    /* the caller can append any additional argv's they desire */
    return ORTE_SUCCESS;
}

/****    SLAVE LAUNCH SUPPORT    ****/

static bool ack_recvd;

static void release_ack(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    ack_recvd = true;
    OBJ_RELEASE(mev);
}

static void recv_ack(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata)
{
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, release_ack);    
}

static void set_handler_default(int sig)
{
    struct sigaction act;
    
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    
    sigaction(sig, &act, (struct sigaction *)0);
}

int orte_plm_base_local_slave_launch(orte_job_t *jdata)
{
    char **argv;
    opal_list_t hosts;
    orte_node_t *node;
    char *nodename;
    char *exec_path;
    bool flag;
    orte_app_context_t *app;
    int rc;
    pid_t pid;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    sigset_t sigs;
    
    /* point to the apps array */
    if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* increment the local slave jobid */
    orte_plm_globals.local_slaves++;

    /* identify the target host - can only be one! */
    OBJ_CONSTRUCT(&hosts, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&hosts, &flag, app->dash_host))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&hosts);
        return rc;
    }
    if (1 < opal_list_get_size(&hosts)) {
        orte_show_help("help-plm-base.txt", "too-many-hosts", true, (int)opal_list_get_size(&hosts));
        return ORTE_ERROR;
    }
    node = (orte_node_t*)opal_list_remove_first(&hosts);
    nodename = strdup(node->name);
    OBJ_RELEASE(node);
    OBJ_DESTRUCT(&hosts);
    
    /* set the jobid in jdata so the caller knows what it is */
    jdata->jobid = orte_plm_globals.local_slaves;

    /* setup the launch */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_rsh_launch(nodename, app,
                                                             "orte-bootproxy.sh",
                                                             &argv, &exec_path))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* add the bootproxy cmd line options */
    if (ORTE_SUCCESS != (rc = orte_plm_base_append_bootproxy_args(app, &argv,
                                                                  jdata->jobid, 0,  /* jobid, vpid */
                                                                  1, 1,  /* #nodes, #procs */
                                                                  0, 0,  /* nrank, lrank */
                                                                  1, 1,  /* #local, #slots */
                                                                  true))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* fork a child to exec the rsh/ssh session */
    pid = fork();
    if (pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    /* child */
    if (pid == 0) {
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
        
        /* exec the slave */
        execv(exec_path, argv);
        opal_output(0, "plm:rsh: execv of %s failed with errno=%s(%d)\n",
                    exec_path, strerror(errno), errno);
        exit(-1);    
    } else {
        /* if it is an orte-job, then parent waits to hear that slave is running. if
         * it isn't an orte-job, then we can't wait because we would never hear
         * anything!
         */
        if (!(jdata->controls & ORTE_JOB_CONTROL_NON_ORTE_JOB)) {
            ack_recvd = false;
            rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH,
                                         ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
            
            ORTE_PROGRESSED_WAIT(ack_recvd, 0, 1);
            /* to release this job from the wait in plm_base_receive, we have to
             * flag it as having reported
             */
            jdata->num_reported = jdata->num_procs;
        }
        
        /* cleanup */
        free(exec_path);
        opal_argv_free(argv);
    }
    
    return ORTE_SUCCESS;
}

/*
 * Take a colon-delimited list of agents and locate the first one that
 * we are able to find in the PATH.  Split that one into argv and
 * return it.  If nothing found, then return NULL.
 */
static char **search(const char* agent_list, const char *path)
{
    int i, j;
    char *line, **lines;
    char **tokens, *tmp;
    char cwd[OPAL_PATH_MAX];
    
    if (NULL == path) {
        getcwd(cwd, OPAL_PATH_MAX);
    } else {
        strncpy(cwd, path, OPAL_PATH_MAX);
    }
    if (NULL == agent_list) {
        lines = opal_argv_split(orte_rsh_agent, ':');
    } else {
        lines = opal_argv_split(agent_list, ':');
    }
    for (i = 0; NULL != lines[i]; ++i) {
        line = lines[i];
        
        /* Trim whitespace at the beginning and end of the line */
        for (j = 0; '\0' != line[j] && isspace(line[j]); ++line) {
            continue;
        }
        for (j = strlen(line) - 2; j > 0 && isspace(line[j]); ++j) {
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

void orte_plm_base_local_slave_finalize(void)
{
    opal_list_item_t *item;
    orte_slave_files_t *slave_node;
    char *cmd, *filenm, **argv;
    int i;
    bool first;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:local:slave:finalize",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    while (NULL != (item = opal_list_remove_first(&orte_plm_globals.slave_files))) {
        slave_node = (orte_slave_files_t*)item;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:local:slave:finalize - entry for node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), slave_node->node));
        
        /* we will use the bootproxy cmd script to clean up for us. All we
         * have to do is tell it to run in CLEANUP mode, and then tell it
         * the APPS and FILES it needs to cleanup
         */
        
        if (slave_node->local) {
            /* setup the bootproxy cmd */
            argv = NULL;
            opal_argv_append_nosize(&argv, slave_node->bootproxy);
        } else {
            /* Start the argv with the rsh/ssh command */
            argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
            /* add the hostname */
            opal_argv_append_nosize(&argv, slave_node->node);
            /* add the bootproxy cmd */
            opal_argv_append_nosize(&argv, slave_node->bootproxy);
        }
        /* pass the CLEANUP mode */
        opal_argv_append_nosize(&argv, "CLEANUP");
        /* pass the name of the apps running on the node - the bootproxy will
         * send a TERM signal to each of them
         */
        first = true;
        for (i=0; i < slave_node->apps.size; i++) {
            if (NULL == (filenm = opal_pointer_array_get_item(&slave_node->apps, i))) {
                continue;
            }
            if (first) {
                opal_argv_append_nosize(&argv, "APPS");
                first = false;
            }
            opal_argv_append_nosize(&argv, filenm);
        }
        /* remove any files we positioned */
        first = true;
        for (i=0; i < slave_node->files.size; i++) {
            if (NULL == (filenm = opal_pointer_array_get_item(&slave_node->files, i))) {
                continue;
            }
            if (first) {
                opal_argv_append_nosize(&argv, "FILES");
                first = false;
            }
            opal_argv_append_nosize(&argv, filenm);
        }
        /* execute the cmd */
        cmd = opal_argv_join(argv, ' ');
        opal_argv_free(argv);
        argv = NULL;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:local:slave:finalize - removing files with cmd:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), cmd));
        system(cmd);
        free(cmd);
        /* now remove the bootproxy itself, if needed */
        if (slave_node->positioned) {
            if (slave_node->local) {
                asprintf(&cmd, "rm -f %s", slave_node->bootproxy);
            } else {
                /* Start the argv with the rsh/ssh command */
                argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
                /* add the hostname */
                opal_argv_append_nosize(&argv, slave_node->node);
                /* add the rm cmd */
                opal_argv_append_nosize(&argv, "rm -f");
                /* add the bootproxy file */
                opal_argv_append_nosize(&argv, slave_node->bootproxy);
                /* form the cmd */
                cmd = opal_argv_join(argv, ' ');
                opal_argv_free(argv);
                argv = NULL;
            }
            /* execute it */
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:local:slave:finalize - removing bootproxy with cmd:\n\t%s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), cmd));
            system(cmd);
            free(cmd);
        }
        OBJ_RELEASE(item);
    }
}

typedef enum {
    ORTE_PLM_SHELL_BASH = 0,
    ORTE_PLM_SHELL_ZSH,
    ORTE_PLM_SHELL_TCSH,
    ORTE_PLM_SHELL_CSH,
    ORTE_PLM_SHELL_KSH,
    ORTE_PLM_SHELL_SH,
    ORTE_PLM_SHELL_UNKNOWN
} orte_plm_shell_t;

/* These strings *must* follow the same order as the enum ORTE_PLM_SHELL_* */
static const char * orte_plm_shell_name[] = {
    "bash",
    "zsh",
    "tcsh",       /* tcsh has to be first otherwise strstr finds csh */
    "csh",
    "ksh",
    "sh",
    "unknown"
};

static orte_plm_shell_t find_shell(char *shell) 
{
    int i         = 0;
    char *sh_name = NULL;
    
    if( (NULL == shell) || (strlen(shell) == 1) ) {
        /* Malformed shell */
        return ORTE_PLM_SHELL_UNKNOWN;
    }
    
    sh_name = rindex(shell, '/');
    if( NULL == sh_name ) {
        /* Malformed shell */
        return ORTE_PLM_SHELL_UNKNOWN;
    }
    
    /* skip the '/' */
    ++sh_name;
    for (i = 0; i < (int)(sizeof (orte_plm_shell_name) /
                          sizeof(orte_plm_shell_name[0])); ++i) {
        if (0 == strcmp(sh_name, orte_plm_shell_name[i])) {
            return (orte_plm_shell_t)i;
        }
    }
    
    /* We didn't find it */
    return ORTE_PLM_SHELL_UNKNOWN;
}

/**
 * Check the Shell variable on the specified node
 */

static int shell_probe(char *nodename, orte_plm_shell_t *shell)
{
    char ** argv;
    int argc, rc = ORTE_SUCCESS, i;
    int fd[2];
    pid_t pid;
    char outbuf[4096];
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:base: going to check SHELL variable on node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         nodename));
    
    *shell = ORTE_PLM_SHELL_UNKNOWN;
    if (pipe(fd)) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: pipe failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }
    if ((pid = fork()) < 0) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:base: fork failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }
    else if (pid == 0) {          /* child */
        if (dup2(fd[1], 1) < 0) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:base: dup2 failed with errno=%d",
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
                             "%s plm:base: close failed with errno=%d",
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
                                     "%s plm:base: Unable to detect the remote shell (error %s)",
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
            for (i = 0; i < (int)(sizeof (orte_plm_shell_name)/
                                  sizeof(orte_plm_shell_name[0])); i++) {
                if ( 0 == strcmp(sh_name, orte_plm_shell_name[i]) ) {
                    *shell = (orte_plm_shell_t)i;
                    break;
                }
            }
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:base: node %s has SHELL: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         nodename,
                         (ORTE_PLM_SHELL_UNKNOWN == *shell) ? "UNHANDLED" : (char*)orte_plm_shell_name[*shell]));
    
    return rc;
}

static int setup_shell(orte_plm_shell_t *rshell,
                       orte_plm_shell_t *lshell,
                       char *nodename, char ***argv)
{
    orte_plm_shell_t remote_shell, local_shell;
    struct passwd *p;
    char *param;
    int rc;
    
    /* What is our local shell? */
    local_shell = ORTE_PLM_SHELL_UNKNOWN;
    p = getpwuid(getuid());
    if( NULL == p ) {
        /* This user is unknown to the system. Therefore, there is no reason we
         * spawn whatsoever in his name. Give up with a HUGE error message.
         */
        orte_show_help( "help-plm-rshd.txt", "unknown-user", true, (int)getuid() );
        return ORTE_ERR_FATAL;
    }
    param = p->pw_shell;
    local_shell = find_shell(p->pw_shell);
    
    /* If we didn't find it in getpwuid(), try looking at the $SHELL
     environment variable (see https://svn.open-mpi.org/trac/ompi/ticket/1060)
     */
    if (ORTE_PLM_SHELL_UNKNOWN == local_shell && 
        NULL != (param = getenv("SHELL"))) {
        local_shell = find_shell(param);
    }
    
    if (ORTE_PLM_SHELL_UNKNOWN == local_shell) {
        opal_output(0, "WARNING: local probe returned unhandled shell:%s assuming bash\n",
                    (NULL != param) ? param : "unknown");
        local_shell = ORTE_PLM_SHELL_BASH;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:base: local shell: %d (%s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         local_shell, orte_plm_shell_name[local_shell]));
    
    /* What is our remote shell? */
    if (orte_assume_same_shell) {
        remote_shell = local_shell;
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:base: assuming same remote shell as local shell",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    } else {
        rc = shell_probe(nodename, &remote_shell);
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        if (ORTE_PLM_SHELL_UNKNOWN == remote_shell) {
            opal_output(0, "WARNING: shell probe returned unhandled shell; assuming bash\n");
            remote_shell = ORTE_PLM_SHELL_BASH;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:base: remote shell: %d (%s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         remote_shell, orte_plm_shell_name[remote_shell]));
    
    /* Do we need to source .profile on the remote side?
     - sh: yes (see bash(1))
     - ksh: yes (see ksh(1))
     - bash: no (see bash(1))
     - [t]csh: no (see csh(1) and tcsh(1))
     - zsh: no (see http://zsh.sourceforge.net/FAQ/zshfaq03.html#l19)
     */
    
    if (ORTE_PLM_SHELL_SH == remote_shell ||
        ORTE_PLM_SHELL_KSH == remote_shell) {
        int i;
        char **tmp;
        tmp = opal_argv_split("( test ! -r ./.profile || . ./.profile;", ' ');
        if (NULL == tmp) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (i = 0; NULL != tmp[i]; ++i) {
            opal_argv_append_nosize(argv, tmp[i]);
        }
        opal_argv_free(tmp);
    }
    
    /* pass results back */
    *rshell = remote_shell;
    *lshell = local_shell;
    
    return ORTE_SUCCESS;
}

int orte_plm_base_setup_rsh_launch(char *nodename, orte_app_context_t *app,
                                   char *rcmd, char ***argv, char **exec_path)
{
    orte_slave_files_t *slave_node, *tst_node;
    opal_list_item_t *item;
    char *bootproxy, *cmd, *scp=NULL;
    char *exefile=NULL, *basename, *path=NULL;
    char *tmp, *dest, *dest_dir, *filenm;
    char **files;
    char cwd[OPAL_PATH_MAX];
    int rc, i, j;
    char *lib_base, *bin_base;
    orte_plm_shell_t rshell, lshell;
    char **tmpargv=NULL;
    char *opal_prefix;
    
    /* set default */
    *exec_path = NULL;
    *argv = NULL;
    
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
    opal_prefix = getenv("OPAL_PREFIX");

    /* have we launched anything on this node before? */
    slave_node = NULL;
    for (item = opal_list_get_first(&orte_plm_globals.slave_files);
         item != opal_list_get_end(&orte_plm_globals.slave_files);
         item = opal_list_get_next(item)) {
        tst_node = (orte_slave_files_t*)item;
        if (0 == strcmp(tst_node->node, nodename)) {
            slave_node = tst_node;
            break;
        }
    }
    if (NULL == slave_node) {
        slave_node = OBJ_NEW(orte_slave_files_t);
        slave_node->node = strdup(nodename);
        /* save the bootproxy cmd */
        slave_node->bootproxy = strdup(rcmd);
        /* is this a local operation? */
        if (0 == strcmp(orte_process_info.nodename, nodename) ||
            0 == strcmp(nodename, "localhost") ||
            opal_ifislocal(nodename)) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:local:slave: node %s is local",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodename));
            slave_node->local = true;
            /* use the prefix, if given */
            if (NULL != app->prefix_dir) {
                asprintf(&slave_node->prefix, "%s/%s", app->prefix_dir, bin_base);
            } else {
                /* use our install dirs */
                slave_node->prefix = strdup(opal_install_dirs.bindir);
            }
            /* no need to preposition the remote cmd, and no need to remove it */
            slave_node->positioned = false;
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:local:slave: setting prefix to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), slave_node->prefix));
        } else {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:local:slave: node %s is remote",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodename));
            /* setup the correct shell info */
            if (ORTE_SUCCESS != (rc = setup_shell(&rshell, &lshell,
                                                  nodename, &tmpargv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(slave_node);
                return rc;
            }
            /* we now need to assemble the actual cmd that will be executed - this depends
             * upon whether or not a prefix directory is being used
             */
            if (NULL != app->prefix_dir) {
                /* if we have a prefix directory, we need to set the PATH and
                 * LD_LIBRARY_PATH on the remote node, and prepend the eventual cmd
                 * with the prefix directory
                 */
                if (ORTE_PLM_SHELL_SH == rshell ||
                    ORTE_PLM_SHELL_KSH == rshell ||
                    ORTE_PLM_SHELL_ZSH == rshell ||
                    ORTE_PLM_SHELL_BASH == rshell) {
                    asprintf (&slave_node->prefix,
                              "%s%s%s PATH=%s/%s:$PATH ; export PATH ; "
                              "LD_LIBRARY_PATH=%s/%s:$LD_LIBRARY_PATH ; export LD_LIBRARY_PATH ; "
                              "%s/%s",
                              (opal_prefix != NULL ? "OPAL_PREFIX=" : ""),
                              (opal_prefix != NULL ? opal_prefix : ""),
                              (opal_prefix != NULL ? " ; export OPAL_PREFIX;" : ""),
                              app->prefix_dir, bin_base,
                              app->prefix_dir, lib_base,
                              app->prefix_dir, bin_base);
                } else if (ORTE_PLM_SHELL_TCSH == rshell ||
                           ORTE_PLM_SHELL_CSH == rshell) {
                    /* [t]csh is a bit more challenging -- we
                     have to check whether LD_LIBRARY_PATH
                     is already set before we try to set it.
                     Must be very careful about obeying
                     [t]csh's order of evaluation and not
                     using a variable before it is defined.
                     See this thread for more details:
                     http://www.open-mpi.org/community/lists/users/2006/01/0517.php. */
                    asprintf (&slave_node->prefix,
                              "%s%s%s set path = ( %s/%s $path ) ; "
                              "if ( $?LD_LIBRARY_PATH == 1 ) "
                              "set OMPI_have_llp ; "
                              "if ( $?LD_LIBRARY_PATH == 0 ) "
                              "setenv LD_LIBRARY_PATH %s/%s ; "
                              "if ( $?OMPI_have_llp == 1 ) "
                              "setenv LD_LIBRARY_PATH %s/%s:$LD_LIBRARY_PATH ; "
                              "%s/%s",
                              (opal_prefix != NULL ? "setenv OPAL_PREFIX " : ""),
                              (opal_prefix != NULL ? opal_prefix : ""),
                              (opal_prefix != NULL ? " ;" : ""),
                              app->prefix_dir, bin_base,
                              app->prefix_dir, lib_base,
                              app->prefix_dir, lib_base,
                              app->prefix_dir, bin_base);
                } else {
                    orte_show_help("help-plm-rshd.txt", "cannot-resolve-shell-with-prefix", true,
                                   (NULL == opal_prefix) ? "NULL" : opal_prefix,
                                   app->prefix_dir);
                    return ORTE_ERR_SILENT;
                }
                /* since we have a prefix, we don't need to preposition the bootproxy
                 * or remove it later
                 */
                slave_node->positioned = false;
            } else if (NULL != app->preload_files_dest_dir) {
                /* the prefix will be the same as the preload destination */
                slave_node->prefix = strdup(app->preload_files_dest_dir);
                /* flag to preload it, and remove it later */
                slave_node->positioned = true;
            } else if (NULL != orte_process_info.tmpdir_base) {
                /* use the tmpdir base */
                slave_node->prefix = strdup(orte_process_info.tmpdir_base);
                /* flag to preload it, and remove it later */
                slave_node->positioned = true;
            } else {
                /* we have to preposition somewhere - default to /tmp */
                slave_node->prefix = strdup("/tmp");
                /* flag to preload it, and remove it later */
                slave_node->positioned = true;
            }
            
            /* do we need to preload the bootproxy on this node? */
            if (slave_node->positioned) {
                /* find the local bootproxy */
                bootproxy = opal_find_absolute_path(rcmd);
                if (NULL == bootproxy) {
                    orte_show_help("help-plm-base.txt", "bootproxy-not-found", true, rcmd);
                    return ORTE_ERR_NOT_FOUND;
                }
                path = opal_os_path(false, slave_node->prefix, rcmd, NULL);
                /* find the scp command */
                scp = opal_find_absolute_path("scp");
                if (NULL == scp) {
                    orte_show_help("help-plm-base.txt", "cp-not-found", true, "scp", "scp");
                    return ORTE_ERROR;
                }
                /* form and execute the scp command */
                asprintf(&cmd, "%s %s %s:%s", scp, bootproxy, nodename, path);
                system(cmd);
                free(cmd);
                free(path);
                free(bootproxy);
            }
        }
        /* add this node to our list */
        opal_list_append(&orte_plm_globals.slave_files, &slave_node->super);
    }
    
    /* if we are going to position the binary or files, did they give us a dest? */
    if (NULL != app->preload_files_dest_dir) {
        /* the target location -must- be an absolute path */
        if (!opal_path_is_absolute(app->preload_files_dest_dir)) {
            orte_show_help("help-plm-base.txt", "abs-path-reqd", true, app->preload_files_dest_dir);
            return ORTE_ERROR;
        }
        dest_dir = app->preload_files_dest_dir;
        /* if this is a local op, make sure this location exists. we can't
         * do this for remote ops as there is no way to create a remote
         * directory
         */
        if (slave_node->local) {
            if (ORTE_SUCCESS != (rc = opal_os_dirpath_create(dest_dir, S_IRWXU))) {
                orte_show_help("help-plm-base.txt", "path-not-created", true, dest_dir);
                return rc;
            }
        }
    } else if (NULL != orte_process_info.tmpdir_base) {
        /* put everything in the tmpdir base */
        dest_dir = orte_process_info.tmpdir_base;
    } else {
        /* put everything in /tmp */
        dest_dir = "/tmp";
    }
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:local:slave: destination dir set to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), dest_dir));
    
    /* setup the exec_path to the bootproxy */
    if (slave_node->local) {
        /* if this is a local operation, then just set
         * the exec_path to be the bootproxy
         */
        *argv = NULL;
        asprintf(exec_path, "%s/%s", slave_node->prefix, rcmd);
        opal_argv_append_nosize(argv, *exec_path);
    } else {
        /* set the exec path to the rsh agent path */
        *exec_path = strdup(orte_plm_globals.rsh_agent_path);
        /* Start the argv with the rsh/ssh command */
        *argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
        /* add the hostname */
        opal_argv_append_nosize(argv, nodename);
        /* add the bootproxy cmd */
        if (NULL != slave_node->prefix) {
            asprintf(&tmp, "%s/%s", slave_node->prefix, rcmd);
        } else {
            tmp = strdup(rcmd);
        }
        opal_argv_append_nosize(argv, tmp);
        free(tmp);
    }
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:local:slave: exec_path set to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), *exec_path));
    
    /* do we need to preload the binary? */
    if (app->preload_binary) {
        char * src;
        /* if the binary is not given in absolute path form,
         * then convert it to one
         */
        if (!opal_path_is_absolute(app->app)) {
            /* see if a source directory was given */
            if (NULL!= app->preload_files_src_dir) {
                /* prepend the src dir to the executable name */
                path = opal_os_path(false, app->preload_files_src_dir, app->app, NULL);
                /* now check for the existence of the app */
                src = opal_find_absolute_path(path);
                if (NULL == src) {
                    orte_show_help("help-plm-base.txt", "exec-not-found", true, path);
                    return ORTE_ERROR;
                }
            } else {
                /* look for it in the cwd */
                getcwd(cwd, OPAL_PATH_MAX);
                src = opal_path_access(app->app, cwd, X_OK);
                if (NULL == src) {
                    orte_show_help("help-plm-base.txt", "exec-not-found", true, cwd);
                    return ORTE_ERROR;
                }
            }
        } else {
            src = opal_path_access(app->app, NULL, X_OK);
            if (NULL == src) {
                orte_show_help("help-plm-base.txt", "exec-not-found", true, app->app);
                return ORTE_ERROR;
            }
        }
        /* get the basename */
        basename = opal_basename(app->app);
        
        /* define the destination */
        dest = opal_os_path(false, dest_dir, basename, NULL);
        
        /*
         * We do not test for error after opal_basename -- this is fine, as opal_os_path
         * is taking a NULL terminated list -- in case of error, well dest_dir is the final dir.
         * However, we need to free basename here, before overwriting the pointer later.
         */
        if (basename != NULL) {
            free(basename);
        }
        
        /* has this binary already been positioned? */
        for (i=0; i < slave_node->apps.size; i++) {
            if (NULL != (filenm = opal_pointer_array_get_item(&slave_node->apps, i)) &&
                0 == strcmp(filenm, dest)) {
                /* this app already has been positioned on the node - skip it */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:local:slave: app %s already positioned",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), filenm));
                goto PRELOAD_FILES;
            }
        }
        /* add the app to the slave_node list */
        opal_pointer_array_add(&slave_node->apps, strdup(dest));
        /* since we are positioning the binary, add it to the list
         * of files to be cleaned up when done
         */
        opal_pointer_array_add(&slave_node->files, strdup(dest));
        
        /* if this is a local node, then we just use the cp command */
        if (slave_node->local) {
            scp = opal_find_absolute_path("cp");
            if (NULL == scp) {
                free (src);
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "cp", "cp");
                return ORTE_ERROR;
            }
            /* form and execute the cp commands */
            asprintf(&cmd, "%s %s %s", scp, src, dest);
            system(cmd);
            free(cmd);
        } else {
            /* find the scp command */
            scp = opal_find_absolute_path("scp");
            if (NULL == scp) {
                free (src);
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "scp", "scp");
                return ORTE_ERROR;
            }
            /* form and execute the scp commands */
            asprintf(&cmd, "%s %s %s:%s", scp, src, nodename, dest);
            system(cmd);
            free(cmd);
        }
        free(src);
        free(dest);
        free(scp);
    } else {
        /* we don't need to pre-position the binary, but we do need
         * to check if we should record it
         */
        for (i=0; i < slave_node->apps.size; i++) {
            if (NULL != (filenm = opal_pointer_array_get_item(&slave_node->apps, i)) &&
                0 == strcmp(filenm, app->app)) {
                /* this app already has been positioned on the node - skip it */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:local:slave: app %s already positioned",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), filenm));
                goto PRELOAD_FILES;
            }
        }
        /* add the app to the slave_node list */
        opal_pointer_array_add(&slave_node->apps, strdup(app->app));
        /* do not add it to the files to be cleaned up when done as
         * we are not positioning it!
         */
    }
    
PRELOAD_FILES:
    /* do we need to pre-position supporting files? */
    if (NULL != app->preload_files) {
        if (slave_node->local) {
            scp = opal_find_absolute_path("cp");
            if (NULL == scp) {
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "cp", "cp");
                return ORTE_ERROR;
            }
        } else {
            /* find the scp command */
            scp = opal_find_absolute_path("scp");
            if (NULL == scp) {
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "scp", "scp");
                return ORTE_ERROR;
            }
        }
        /* break apart the comma-separated list of files */
        files = opal_argv_split(app->preload_files, ',');
        /* copy each file across */
        for (i=0; i < opal_argv_count(files); i++) {
            /* if the file is not given in absolute path form,
             * then convert it to one
             */
            if (!opal_path_is_absolute(files[i])) {
                /* see if a source directory was given */
                if (NULL!= app->preload_files_src_dir) {
                    /* look for the file there */
                    exefile = opal_path_access(files[i], app->preload_files_src_dir, R_OK);
                } else {
                    /* look for it in the cwd */
                    getcwd(cwd, OPAL_PATH_MAX);
                    exefile = opal_path_access(files[i], cwd, R_OK);
                }
            } else {
                exefile = opal_path_access(files[i], NULL, R_OK);
            }
            if (NULL == exefile) {
                getcwd(cwd, OPAL_PATH_MAX);
                orte_show_help("help-plm-base.txt", "file-not-found", true, files[i],
                               (NULL == app->preload_files_src_dir) ? cwd : app->preload_files_src_dir);
                return ORTE_ERROR;
            }
            /* define the destination */
            dest = opal_os_path(false, dest_dir, files[i], NULL);
            /* has this file already been positioned? */
            for (j=0; j < slave_node->files.size; j++) {
                if (NULL != (filenm = opal_pointer_array_get_item(&slave_node->files, j)) &&
                    0 == strcmp(filenm, dest)) {
                    /* this app already has been positioned on the node - skip it */
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:local:slave: file %s already positioned",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), filenm));
                    goto SKIP;
                }
            }
            /* add the file to the slave_node list */
            opal_pointer_array_add(&slave_node->files, strdup(dest));
            if (slave_node->local) {
                /* form and execute the cp command */
                asprintf(&cmd, "%s %s %s", scp, exefile, dest);
                system(cmd);
                free(cmd);
            } else {
                /* form and execute the scp commands */
                asprintf(&cmd, "%s -q %s %s:%s", scp, exefile, nodename, dest);
                system(cmd);
                free(cmd);
            }
        SKIP:
            free(exefile);
            free(dest);
        }
        opal_argv_free(files);
        free(scp);
    }
    
    return ORTE_SUCCESS;    
}

int orte_plm_base_append_bootproxy_args(orte_app_context_t *app, char ***argv,
                                        orte_jobid_t jobid, orte_vpid_t vpid,
                                        int num_nodes, orte_vpid_t num_procs,
                                        orte_node_rank_t nrank, orte_local_rank_t lrank,
                                        orte_vpid_t nlocal, int nslots, bool overwrite)
{
    char *param, *path, *tmp, *cmd, *basename, *dest_dir;
    int i;
    
    /* if a prefix is set, pass it to the bootproxy in a special way */
    if (NULL != app->prefix_dir) {
        asprintf(&param, "OMPI_PREFIX=%s", app->prefix_dir);
        opal_argv_append_nosize(argv, param);
        free(param);
    }
    
    /* if there is a working directory specified, add it in a special
     * way so the bootproxy can deal with it
     */
    if (NULL != app->cwd) {
        asprintf(&param, "OMPI_WDIR=%s", app->cwd);
        opal_argv_append_nosize(argv, param);
        free(param);
    }
    
    /* add all OMPI params from the app */
    if (NULL != app->env) {
        for (i=0; NULL != app->env[i]; i++) {
            if (0 == strncmp(app->env[i], "OMPI_", 5)) {
                if (NULL == strchr(app->env[i], ';') &&
                    NULL == strchr(app->env[i], ':')) {
                    opal_argv_append_nosize(argv, app->env[i]);
                } else {
                    tmp = strchr(app->env[i], '=');
                    *tmp = '\0';
                    tmp++;
                    asprintf(&param, "%s=\"%s\"", app->env[i], tmp);
                    opal_argv_append_nosize(argv, param);
                    free(param);
                }
            }
        }
    }
    
    /* add MCA params required for launch */
    
    /***  OVERRIDE WHAT THE APP PROVIDED ONLY IF DIRECTED TO DO SO ***/
    /* tell ESS to select the "slave" component */
    param = mca_base_param_environ_variable("ess",NULL,NULL);
    opal_setenv(param, "slave", overwrite, argv);
    free(param);
    
    /* tell ROUTED to select the "slave" component */
    param = mca_base_param_environ_variable("routed",NULL,NULL);
    opal_setenv(param, "slave", overwrite, argv);
    free(param);
    
    /* tell GRPCOMM to select the "hier" component */
    param = mca_base_param_environ_variable("grpcomm",NULL,NULL);
    opal_setenv(param, "hier", overwrite, argv);
    free(param);

    /* setup yield schedule to be aggressive */
    param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
    opal_setenv(param, "0", overwrite, argv);
    free(param);
    
    
    /***  OVERWRITE ANY PRE-EXISTING VALUES ***/
    /* must pass the number of nodes */
    param = mca_base_param_environ_variable("orte","num","nodes");
    asprintf(&cmd, "%d", num_nodes);
    opal_setenv(param, cmd, true, argv);
    free(param);
    free(cmd);
    
    /* set the daemon uri to point to me */
    param = mca_base_param_environ_variable("orte","local_daemon","uri");
    asprintf(&path, "\"%s\"", orte_rml.get_contact_info());
    opal_setenv(param, path, true, argv);
    free(param);
    free(path);
    
    /* set a value for the HNP uri - it won't be needed, but is
     * required to pass existence tests
     */
    param = mca_base_param_environ_variable("orte","hnp","uri");
    asprintf(&path, "\"%s\"", orte_process_info.my_hnp_uri);
    opal_setenv(param, path, true, argv);
    free(param);
    free(path);
    
    /* set the app_context number */
    param = mca_base_param_environ_variable("orte","app","num");
    opal_setenv(param, "1", true, argv);
    free(param);
    
    /* ensure that any "name" envar is cleared */
    param = mca_base_param_environ_variable("orte","ess","name");
    opal_unsetenv(param, argv);
    free(param);
    
    /* set the jobid */
    orte_util_convert_jobid_to_string(&cmd, jobid);
    param = mca_base_param_environ_variable("orte","ess","jobid");
    opal_setenv(param, cmd, true, argv);
    free(param);
    free(cmd);
    
    /* set the vpid  */
    orte_util_convert_vpid_to_string(&cmd, vpid);
    param = mca_base_param_environ_variable("orte","ess","vpid");
    opal_setenv(param, cmd, true, argv);
    free(param);
    opal_setenv("OMPI_COMM_WORLD_RANK", cmd, true, argv);
    free(cmd);
    
    /* set the number of procs */
    asprintf(&cmd, "%d", (int)num_procs);
    param = mca_base_param_environ_variable("orte","ess","num_procs");
    opal_setenv(param, cmd, true, argv);
    free(param);
    opal_setenv("OMPI_COMM_WORLD_SIZE", cmd, true, argv);
    free(cmd);
    
    asprintf(&cmd, "%lu", (unsigned long) nrank);
    opal_setenv("OMPI_COMM_WORLD_NODE_RANK", cmd, true, argv);
    /* set an mca param for it too */
    param = mca_base_param_environ_variable("orte","ess","node_rank");
    opal_setenv(param, cmd, true, argv);
    free(param);
    free(cmd);

    /* some user-requested public environmental variables */
    asprintf(&cmd, "%d", (int)nslots);
    opal_setenv("OMPI_UNIVERSE_SIZE", cmd, true, argv);
    free(cmd);
    asprintf(&cmd, "%d", (int)lrank);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_RANK", cmd, true, argv);
    free(cmd);
    asprintf(&cmd, "%d", (int)nlocal);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_SIZE", cmd, true, argv);
    free(cmd);
    
    /* if we are going to position the binary or files, did they give us a dest? */
    if (NULL != app->preload_files_dest_dir) {
        /* the target location -must- be an absolute path */
        if (!opal_path_is_absolute(app->preload_files_dest_dir)) {
            orte_show_help("help-plm-base.txt", "abs-path-reqd", true, app->preload_files_dest_dir);
            return ORTE_ERROR;
        }
        dest_dir = app->preload_files_dest_dir;
    } else if (NULL != orte_process_info.tmpdir_base) {
        /* put everything in the tmpdir base */
        dest_dir = orte_process_info.tmpdir_base;
    } else {
        /* put everything in /tmp */
        dest_dir = "/tmp";
    }
    
    if (app->preload_binary) {
        /* construct the target path */
        basename = opal_basename(app->app);
        path = opal_os_path(false, dest_dir, basename, NULL);
        free(basename);
        /* add this to the cmd */
        opal_argv_append_nosize(argv, path);
        free(path);
    } else {
        /* it must already have been put there - if the given
         * path was absolute, just use it
         */
        if (opal_path_is_absolute(app->app)) {
            opal_argv_append_nosize(argv, app->app);
        } else if (NULL != app->cwd) {
            /* prepend the cwd, if provided */
            param = opal_os_path(false, app->cwd, app->app, NULL);
            opal_argv_append_nosize(argv, param);
            free(param);
        } else {
            /* just do your best, i guess */
            opal_argv_append_nosize(argv, app->app);
        }
    }
    
    /* add any provided argv */
    for (i=1; NULL != app->argv[i]; i++) {
        opal_argv_append_nosize(argv, app->argv[i]);
    }
    
    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(*argv, ' ');
        opal_output(0, "%s plm:base:append_bootproxy_args: final argv:\n\t%s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    (NULL == param) ? "NULL" : param);
        if (NULL != param) free(param);
    }
    
    return ORTE_SUCCESS;
}

void orte_plm_base_reset_job(orte_job_t *jdata)
{
    int n, i, j;
    orte_proc_t *proc, *proc_from_node;
    orte_node_t *node_from_map, *node;
    
    /* set the state to restart */
    jdata->state = ORTE_JOB_STATE_RESTART;
    /* cycle through the procs */
    for (n=0; n < jdata->procs->size; n++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, n))) {
            continue;
        }
        if (ORTE_PROC_STATE_TERMINATED < proc->state) {
            /* this proc abnormally terminated */
            proc->state = ORTE_PROC_STATE_RESTART;
            proc->pid = 0;
            /* remove the proc from the node upon which it was mapped
             *
             * NOTE: IT IS IMPORTANT THAT WE LEAVE THE proc->node CONNECTION
             * ALONE SO THAT ANY RESILIENT MAPPING CAN KNOW WHERE THE PROC
             * WAS PREVIOUSLY LOCATED
             */
            node = proc->node;
            for (i=0; i < node->procs->size; i++) {
                if (NULL == (proc_from_node = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                    continue;
                }
                if (proc_from_node->name.jobid == proc->name.jobid &&
                    proc_from_node->name.vpid == proc->name.vpid) {
                    /* got it! */
                    OBJ_RELEASE(proc);  /* keep accounting straight */
                    opal_pointer_array_set_item(node->procs, i, NULL);
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "removing proc %s from node %s at index %d",
                                         ORTE_NAME_PRINT(&proc->name), node->name, i));
                    node->num_procs--;
                    node->slots_inuse--;
                    if (0 == node->num_procs) {
                        /* this node has been emptied - remove it from map */
                        for (j=0; j < jdata->map->nodes->size; j++) {
                            if (NULL == (node_from_map = (orte_node_t*)opal_pointer_array_get_item(jdata->map->nodes, i))) {
                                continue;
                            }
                            if (node_from_map->index == node->index) {
                                /* got it! */
                                OBJ_RELEASE(node);  /* keep accounting straight*/
                                opal_pointer_array_set_item(jdata->map->nodes, i, NULL);
                                break;
                            }
                        }
                    }
                    break;
                }
            }
            /* adjust job accounting */
            jdata->num_terminated--;
        }
    }
    /* clear the info on who aborted */
    jdata->abort = false;
    if (NULL != jdata->aborted_proc) {
        OBJ_RELEASE(jdata->aborted_proc);  /* maintain reference count */
        jdata->aborted_proc = NULL;
    }
    /* since every daemon will be reporting status for every proc, reset these to zero */
    jdata->num_launched = 0;
    jdata->num_reported = 0;
    /* since we are restarting the failed proc, reset the exit status */
    ORTE_RESET_EXIT_STATUS();
}
