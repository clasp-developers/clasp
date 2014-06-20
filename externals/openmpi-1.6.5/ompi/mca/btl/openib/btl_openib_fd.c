/*
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009 Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * Note: this file is a little fast-n-loose with OPAL_HAVE_THREADS --
 * it uses this value in run-time "if" conditionals (vs. compile-time
 * #if conditionals).  We also don't protect including <pthread.h>.
 * That's because this component currently only compiles on Linux and
 * Solaris, and both of these OS's have pthreads.  Using the run-time
 * conditionals gives us bettern compile-time checking, even of code
 * that isn't activated.
 *
 * Note, too, that the functionality in this file does *not* require
 * all the heavyweight OMPI thread infrastructure (e.g., from
 * --enable-mpi-threads or --enable-progress-threads).  All work that
 * is done in a separate progress thread is very carefully segregated
 * from that of the main thread, and communication back to the main
 * thread
 */

#include "ompi_config.h"

#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "opal/util/output.h"

#include "ompi/constants.h"

#include "btl_openib_fd.h"


typedef union {
    ompi_btl_openib_fd_event_callback_fn_t *event;
    ompi_btl_openib_fd_main_callback_fn_t *main;
} callback_u_t;

/*
 * Data for each registered item
 */
typedef struct {
    opal_list_item_t super;
    bool ri_event_used;
    opal_event_t ri_event;
    int ri_fd;
    int ri_flags;
    callback_u_t ri_callback;
    void *ri_context;
} registered_item_t;

static OBJ_CLASS_INSTANCE(registered_item_t, opal_list_item_t, NULL, NULL);

/*
 * Command types
 */
typedef enum {
    /* Read by service thread */
    CMD_TIME_TO_QUIT,
    CMD_ADD_FD,
    CMD_REMOVE_FD,
    ACK_RAN_FUNCTION,

    /* Read by service and main threads */
    CMD_CALL_FUNCTION,
    CMD_MAX
} cmd_type_t;

/*
 * Commands.  Fields ordered to avoid memory holes (and valgrind warnings).
 */
typedef struct {
    callback_u_t pc_fn;
    void *pc_context;
    int pc_fd;
    int pc_flags;
    cmd_type_t pc_cmd;
    char end;
} cmd_t;

/*
 * Queued up list of commands to send to the main thread
 */
typedef struct {
    opal_list_item_t super;
    cmd_t cli_cmd;
} cmd_list_item_t;

static OBJ_CLASS_INSTANCE(cmd_list_item_t, opal_list_item_t, NULL, NULL);

static bool initialized = false;
static int cmd_size = 0;
static fd_set read_fds, write_fds;
static int max_fd;
static opal_list_t registered_items;

/* These items are only used in the threaded version */
/* Owned by the main thread */
static pthread_t thread;
static opal_event_t main_thread_event;
static int pipe_to_service_thread[2] = { -1, -1 };

/* Owned by the service thread */
static int pipe_to_main_thread[2] = { -1, -1 };
static const size_t max_outstanding_to_main_thread = 32;
static size_t waiting_for_ack_from_main_thread = 0;
static opal_list_t pending_to_main_thread;


/*
 * Simple loop over reading from a fd
 */
static int read_fd(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = read(fd, b, len);
        if (rc < 0 && EAGAIN == errno) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}


/*
 * Simple loop over writing to an fd
 */
static int write_fd(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = write(fd, b, len);
        if (rc < 0 && EAGAIN == errno) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Write a command to the main thread, or queue it up if the pipe is full
 */
static int write_to_main_thread(cmd_t *cmd)
{
    /* Note that if we write too much to the main thread pipe and the
       main thread doesn't check it often, we could fill up the pipe
       and cause this thread to block.  Bad!  So we do some simple
       counting here and ensure that we don't fill the pipe.  If we
       are in danger of that, then queue up the commands here in the
       service thread.  The main thread will ACK every CALL_FUNCTION
       command, so we have a built-in mechanism to wake up the service
       thread to drain any queued-up commands. */
    if (opal_list_get_size(&pending_to_main_thread) > 0 ||
        waiting_for_ack_from_main_thread >= max_outstanding_to_main_thread) {
        cmd_list_item_t *cli = OBJ_NEW(cmd_list_item_t);
        if (NULL == cli) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        memcpy(&cli->cli_cmd, cmd, cmd_size);
        opal_list_append(&pending_to_main_thread, &(cli->super));
    } else {
        OPAL_OUTPUT((-1, "fd: writing to main thread"));
        write_fd(pipe_to_main_thread[1], cmd_size, cmd);
        ++waiting_for_ack_from_main_thread;
    }

    return OMPI_SUCCESS;
}

static void service_fd_callback(int fd, short event, void *context)
{
    registered_item_t *ri = (registered_item_t*) context;
    ri->ri_callback.event(fd, event, ri->ri_context);
}


/*
 * Add an fd to the listening set
 */
static int service_pipe_cmd_add_fd(bool use_libevent, cmd_t *cmd)
{
    registered_item_t *ri = OBJ_NEW(registered_item_t);
    if (NULL == ri) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    ri->ri_event_used = false;
    ri->ri_fd = cmd->pc_fd;
    ri->ri_flags = cmd->pc_flags;
    ri->ri_callback.event = cmd->pc_fn.event;
    ri->ri_context = cmd->pc_context;

    if (use_libevent) {
        /* Make an event for this fd */
        ri->ri_event_used = true;
        memset(&ri->ri_event, 0, sizeof(ri->ri_event));
        opal_event_set(&ri->ri_event, ri->ri_fd, 
                       ri->ri_flags | OPAL_EV_PERSIST, service_fd_callback,
                       ri);
        opal_event_add(&ri->ri_event, 0);
    } else {
        /* Add the fd to the relevant fd local sets and update max_fd */
        if (OPAL_EV_READ & ri->ri_flags) {
            FD_SET(ri->ri_fd, &read_fds);
        }
        if (OPAL_EV_WRITE & cmd->pc_flags) {
            FD_SET(ri->ri_fd, &write_fds);
        }
        max_fd = (max_fd > ri->ri_fd) ? max_fd : ri->ri_fd + 1;
    }

    opal_list_append(&registered_items, &ri->super);
    return OMPI_SUCCESS;
}

/*
 * Run a function
 */
static int service_pipe_cmd_call_function(cmd_t *cmd)
{
    cmd_t local_cmd;

    OPAL_OUTPUT((-1, "fd service thread: calling function!"));
    /* Call the function */
    if (NULL != cmd->pc_fn.main) {
        cmd->pc_fn.main(cmd->pc_context);
    }

    /* Now ACK that we ran the function */
    memset(&local_cmd, 0, cmd_size);
    local_cmd.pc_cmd = ACK_RAN_FUNCTION;
    write_fd(pipe_to_main_thread[1], cmd_size, &local_cmd);

    /* Done */
    return OMPI_SUCCESS;
}

/*
 * Remove an fd from the listening set
 */
static int service_pipe_cmd_remove_fd(cmd_t *cmd)
{
    int i;
    opal_list_item_t *item;
    registered_item_t *ri;

    OPAL_OUTPUT((-1, "service thread got unmonitor fd %d", cmd->pc_fd));
    /* Go through the list of registered fd's and find the fd to
       remove */
    for (item = opal_list_get_first(&registered_items);
         NULL != opal_list_get_end(&registered_items);
         item = opal_list_get_next(item)) {
        ri = (registered_item_t*) item;
        if (cmd->pc_fd == ri->ri_fd) {
            /* Found it.  The item knows if it was used as a libevent
               event or an entry in the local fd sets. */
            if (ri->ri_event_used) {
                /* Remove this event from libevent */
                opal_event_del(&ri->ri_event);
            } else {
                /* Remove this item from the fd_sets and recalculate
                   MAX_FD */
                FD_CLR(cmd->pc_fd, &read_fds);
                FD_CLR(cmd->pc_fd, &write_fds);
                for (max_fd = i = pipe_to_service_thread[0]; i < FD_SETSIZE; ++i) {
                    if (FD_ISSET(i, &read_fds) || FD_ISSET(i, &write_fds)) {
                        max_fd = i + 1;
                    }
                }
            }
            
            /* Let the caller know that we have stopped monitoring
               this fd (if they care) */
            if (NULL != cmd->pc_fn.event) {
                cmd->pc_fn.event(cmd->pc_fd, 0, cmd->pc_context);
            }
    
            /* Remove this item from the list of registered items and
               release it */
            opal_list_remove_item(&registered_items, item);
            OBJ_RELEASE(item);
            return OMPI_SUCCESS;
        }
    }

    /* This shouldn't happen */
    return OMPI_ERR_NOT_FOUND;
}


/*
 * Call a function and ACK that we ran it
 */
static int main_pipe_cmd_call_function(cmd_t *cmd)
{
    cmd_t local_cmd;

    OPAL_OUTPUT((-1, "fd main thread: calling function!"));
    /* Call the function */
    if (NULL != cmd->pc_fn.main) {
        cmd->pc_fn.main(cmd->pc_context);
    }

    /* Now ACK that we ran the function */
    memset(&local_cmd, 0, cmd_size);
    local_cmd.pc_cmd = ACK_RAN_FUNCTION;
    write_fd(pipe_to_service_thread[1], cmd_size, &local_cmd);

    /* Done */
    return OMPI_SUCCESS;
}


/*
 * Act on pipe commands
 */
static bool service_pipe_cmd(void)
{
    bool ret = false;
    cmd_t cmd;
    cmd_list_item_t *cli;

    read_fd(pipe_to_service_thread[0], cmd_size, &cmd);
    switch (cmd.pc_cmd) {
    case CMD_ADD_FD:
        OPAL_OUTPUT((-1, "fd service thread: CMD_ADD_FD"));
        if (OMPI_SUCCESS != service_pipe_cmd_add_fd(false, &cmd)) {
            ret = true;
        }
        break;

    case CMD_REMOVE_FD:
        OPAL_OUTPUT((-1, "fd service thread: CMD_REMOVE_FD"));
        if (OMPI_SUCCESS != service_pipe_cmd_remove_fd(&cmd)) {
            ret = true;
        }
        break;

    case CMD_CALL_FUNCTION:
        OPAL_OUTPUT((-1, "fd service thread: CMD_RUN_FUNCTION"));
        if (OMPI_SUCCESS != service_pipe_cmd_call_function(&cmd)) {
            ret = true;
        }
        break;

    case CMD_TIME_TO_QUIT:
        OPAL_OUTPUT((-1, "fd service thread: CMD_TIME_TO_QUIT"));
        ret = true;
        break;

    case ACK_RAN_FUNCTION:
        /* We don't have a guarantee that the main thread will check
           its pipe frequently, so we do some simple counting to
           ensure we just don't have too many outstanding commands to
           the main thread at any given time.  The main thread will
           ACK every CALL_FUNCTION command, so this thread will always
           wake up and continue to drain any queued up functions. */
        cli = (cmd_list_item_t*) opal_list_remove_first(&pending_to_main_thread);
        if (NULL != cli) {
            OPAL_OUTPUT((-1, "sending queued up cmd function to main thread"));
            write_fd(pipe_to_main_thread[1], cmd_size, &(cli->cli_cmd));
            OBJ_RELEASE(cli);
        } else {
            --waiting_for_ack_from_main_thread;
        }
        break;
        
    default:
        OPAL_OUTPUT((-1, "fd service thread: unknown pipe command!"));
        break;
    }

    return ret;
}


/*
 * Main thread logic
 */
static void *service_thread_start(void *context)
{
    int rc, flags;
    fd_set read_fds_copy, write_fds_copy;
    opal_list_item_t *item;
    registered_item_t *ri;

    /* Make an fd set that we can select() on */
    FD_ZERO(&write_fds);
    FD_ZERO(&read_fds);
    FD_SET(pipe_to_service_thread[0], &read_fds);
    max_fd = pipe_to_service_thread[0] + 1;

    OPAL_OUTPUT((-1, "fd service thread running"));

    /* Main loop waiting for commands over the fd's */
    while (1) {
        memcpy(&read_fds_copy, &read_fds, sizeof(read_fds));
        memcpy(&write_fds_copy, &write_fds, sizeof(write_fds));
        OPAL_OUTPUT((-1, "fd service thread blocking on select..."));
        rc = select(max_fd, &read_fds_copy, &write_fds_copy, NULL, NULL);
        if (0 != rc && EAGAIN == errno) {
            continue;
        }

        OPAL_OUTPUT((-1, "fd service thread woke up!"));
        if (rc > 0) {
            if (FD_ISSET(pipe_to_service_thread[0], &read_fds_copy)) {
                OPAL_OUTPUT((-1, "fd service thread: pipe command"));
                if (service_pipe_cmd()) {
                    break;
                }
                OPAL_OUTPUT((-1, "fd service thread: back from pipe command"));
            } 

            /* Go through all the registered events and see who had
               activity */
            if (!opal_list_is_empty(&registered_items)) {
                for (item = opal_list_get_first(&registered_items);
                     item != opal_list_get_end(&registered_items);
                     item = opal_list_get_next(item)) {
                    ri = (registered_item_t*) item;
                    flags = 0;

                    /* See if this fd was ready for reading or writing
                       (fd's will only be in the read_fds or write_fds
                       set depending on what they registered for) */
                    if (FD_ISSET(ri->ri_fd, &read_fds_copy)) {
                        flags |= OPAL_EV_READ;
                    }
                    if (FD_ISSET(ri->ri_fd, &write_fds_copy)) {
                        flags |= OPAL_EV_WRITE;
                    }

                    /* If either was ready, invoke the callback */
                    if (0 != flags) {
                        OPAL_OUTPUT((-1, "fd service thread: invoking callback for registered fd %d", ri->ri_fd));
                        ri->ri_callback.event(ri->ri_fd, flags, 
                                              ri->ri_context);
                        OPAL_OUTPUT((-1, "fd service thread: back from callback for registered fd %d", ri->ri_fd));
                    }
                }
            }
        }
    }

    /* All done */
    OPAL_OUTPUT((-1, "fd service thread: exiting"));
    opal_atomic_wmb();
    return NULL;
}


static void main_thread_event_callback(int fd, short event, void *context)
{
    cmd_t cmd;

    OPAL_OUTPUT((-1, "main thread -- reading command"));
    read_fd(pipe_to_main_thread[0], cmd_size, &cmd);
    switch (cmd.pc_cmd) {
    case CMD_CALL_FUNCTION:
        OPAL_OUTPUT((-1, "fd main thread: calling command"));
        main_pipe_cmd_call_function(&cmd);
        break;

    default:
        OPAL_OUTPUT((-1, "fd main thread: unknown pipe command: %d", 
                    cmd.pc_cmd));
        break;
    }
}

/******************************************************************
 * Main interface calls
 ******************************************************************/

/*
 * Initialize
 * Called by main thread
 */
int ompi_btl_openib_fd_init(void)
{
    if (!initialized) {
        cmd_t bogus;

        OBJ_CONSTRUCT(&registered_items, opal_list_t);

        /* Calculate the real size of the cmd struct */
        cmd_size = (int) (&(bogus.end) - ((char*) &bogus));

        if (OPAL_HAVE_THREADS) {
            OBJ_CONSTRUCT(&pending_to_main_thread, opal_list_t);

            /* Create pipes to communicate between the two threads */
            if (0 != pipe(pipe_to_service_thread)) {
                return OMPI_ERR_IN_ERRNO;
            }
            if (0 != pipe(pipe_to_main_thread)) {
                return OMPI_ERR_IN_ERRNO;
            }

            /* Create a libevent event that is used in the main thread
               to watch its pipe */
            memset(&main_thread_event, 0, sizeof(main_thread_event));
            opal_event_set(&main_thread_event, pipe_to_main_thread[0],
                           OPAL_EV_READ | OPAL_EV_PERSIST, 
                           main_thread_event_callback, NULL);
            opal_event_add(&main_thread_event, 0);
            
            /* Start the service thread */
            if (0 != pthread_create(&thread, NULL, service_thread_start, 
                                    NULL)) {
                int errno_save = errno;
                opal_event_del(&main_thread_event);
                close(pipe_to_service_thread[0]);
                close(pipe_to_service_thread[1]);
                close(pipe_to_main_thread[0]);
                close(pipe_to_main_thread[1]);
                errno = errno_save;
                return OMPI_ERR_IN_ERRNO;
            }
        }

        initialized = true;
    }
    return OMPI_SUCCESS;
}


/*
 * Start monitoring an fd
 * Called by main or service thread; callback will be in service thread
 */
int ompi_btl_openib_fd_monitor(int fd, int flags, 
                               ompi_btl_openib_fd_event_callback_fn_t *callback,
                               void *context)
{
    cmd_t cmd;

    /* Sanity check */
    if (fd < 0 || 0 == flags || NULL == callback) {
        return OMPI_ERR_BAD_PARAM;
    }

    cmd.pc_cmd = CMD_ADD_FD;
    cmd.pc_fd = fd;
    cmd.pc_flags = flags;
    cmd.pc_fn.event = callback;
    cmd.pc_context = context;
    if (OPAL_HAVE_THREADS) {
        /* For the threaded version, write a command down the pipe */
        OPAL_OUTPUT((-1, "main thread sending monitor fd %d", fd));
        write_fd(pipe_to_service_thread[1], cmd_size, &cmd);
    } else {
        /* Otherwise, add it directly */
        service_pipe_cmd_add_fd(true, &cmd);
    }

    return OMPI_SUCCESS;
}


/*
 * Stop monitoring an fd
 * Called by main or service thread; callback will be in service thread
 */
int ompi_btl_openib_fd_unmonitor(int fd, 
                                 ompi_btl_openib_fd_event_callback_fn_t *callback,
                                 void *context)
{
    cmd_t cmd;

    /* Sanity check */
    if (fd < 0) {
        return OMPI_ERR_BAD_PARAM;
    }
    
    cmd.pc_cmd = CMD_REMOVE_FD;
    cmd.pc_fd = fd;
    cmd.pc_flags = 0;
    cmd.pc_fn.event = callback;
    cmd.pc_context = context;
    if (OPAL_HAVE_THREADS) {
        /* For the threaded version, write a command down the pipe */
        OPAL_OUTPUT((-1, "main thread sending unmonitor fd %d", fd));
        write_fd(pipe_to_service_thread[1], cmd_size, &cmd);
    } else {
        /* Otherwise, remove it directly */
        service_pipe_cmd_remove_fd(&cmd);
    }

    return OMPI_SUCCESS;
}

/*
 * Run in the service thread
 * Called by main thread; callback will be in service thread
 */
int ompi_btl_openib_fd_run_in_service(ompi_btl_openib_fd_main_callback_fn_t *callback,
                                      void *context)
{
    cmd_t cmd;

    cmd.pc_cmd = CMD_CALL_FUNCTION;
    cmd.pc_fd = -1;
    cmd.pc_flags = 0;
    cmd.pc_fn.main = callback;
    cmd.pc_context = context;
    if (OPAL_HAVE_THREADS) {
        /* For the threaded version, write a command down the pipe */
        OPAL_OUTPUT((-1, "main thread sending 'run in service'"));
        write_fd(pipe_to_service_thread[1], cmd_size, &cmd);
    } else {
        /* Otherwise, run it directly */
        callback(context);
    }

    return OMPI_SUCCESS;
}

/*
 * Run a function in the main thread
 * Called by service thread
 */
int ompi_btl_openib_fd_run_in_main(ompi_btl_openib_fd_main_callback_fn_t *callback,
                                   void *context)
{
    if (OPAL_HAVE_THREADS) {
        cmd_t cmd;

        OPAL_OUTPUT((-1, "run in main -- sending command"));
        /* For the threaded version, write a command down the pipe */
        cmd.pc_cmd = CMD_CALL_FUNCTION;
        cmd.pc_fd = -1;
        cmd.pc_flags = 0;
        cmd.pc_fn.main = callback;
        cmd.pc_context = context;
        write_to_main_thread(&cmd);
    } else {
        /* Otherwise, call it directly */
        OPAL_OUTPUT((-1, "run in main -- calling now!"));
        callback(context);
    }

    return OMPI_SUCCESS;
}


int
ompi_btl_openib_fd_main_thread_drain(void)
{
    int nfds, ret;
    fd_set rfds;
    struct timeval tv;
  
    while (1) {
        FD_ZERO(&rfds);
        FD_SET(pipe_to_main_thread[0], &rfds);
        nfds = pipe_to_main_thread[0] + 1;

        tv.tv_sec = 0;
        tv.tv_usec = 0;

        ret = select(nfds, &rfds, NULL, NULL, &tv);
        if (ret > 0) {
            main_thread_event_callback(pipe_to_main_thread[0], 0, NULL);
            return 0;
        } else {
            return ret;
        }
    }
}


/*
 * Finalize
 * Called by main thread
 */
int ompi_btl_openib_fd_finalize(void)
{
    if (initialized) {
        if (OPAL_HAVE_THREADS) {
            /* For the threaded version, send a command down the pipe */
            cmd_t cmd;
            OPAL_OUTPUT((-1, "shutting down openib fd"));
            opal_event_del(&main_thread_event);
            memset(&cmd, 0, cmd_size);
            cmd.pc_cmd = CMD_TIME_TO_QUIT;
            write_fd(pipe_to_service_thread[1], cmd_size, &cmd);
            
            pthread_join(thread, NULL);
            opal_atomic_rmb();
            
            opal_event_del(&main_thread_event);
            
            close(pipe_to_service_thread[0]);
            close(pipe_to_service_thread[1]);
            close(pipe_to_main_thread[0]);
            close(pipe_to_main_thread[1]);
            OBJ_DESTRUCT(&pending_to_main_thread);
        }
        OBJ_DESTRUCT(&registered_items);
    }
    initialized = false;

    return OMPI_SUCCESS;
}
