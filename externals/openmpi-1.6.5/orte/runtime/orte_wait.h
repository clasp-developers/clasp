/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for waitpid / async notification of child death with the
 * libevent runtime system.
 */
#ifndef ORTE_WAIT_H
#define ORTE_WAIT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/sys/atomic.h"
#include "opal/event/event.h"
#include "opal/runtime/opal_progress.h"

#include "orte/types.h"
#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS

typedef struct {
    opal_object_t super;
    char *name;
    int channel;
    opal_atomic_lock_t lock;
} orte_trigger_event_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_trigger_event_t);

/** typedef for callback function used in \c ompi_rte_wait_cb */
typedef void (*orte_wait_fn_t)(pid_t wpid, int status, void *data);

/**
 * Disable / re-Enable SIGCHLD handler
 *
 * These functions have to be used after orte_wait_init was called.
 */

ORTE_DECLSPEC void orte_wait_enable(void);
ORTE_DECLSPEC void orte_wait_disable(void);

/**
 * Wait for process terminiation
 *
 * Similar to \c waitpid, \c orte_waitpid utilizes the run-time
 * event library for process terminiation notification.  The \c
 * WUNTRACED option is not supported, but the \c WNOHANG option is
 * supported.
 *
 * \note A \c wpid value of \c -1 is not currently supported and will
 * return an error.
 */
ORTE_DECLSPEC pid_t orte_waitpid(pid_t wpid, int *status, int options);


/**
 * Register a callback for process termination
 *
 * Register a callback for notification when \c wpid causes a SIGCHLD.
 * \c waitpid() will have already been called on the process at this
 * time.  
 *
 * If a thread is already blocked in \c ompi_rte_waitpid for \c wpid,
 * this function will return \c ORTE_ERR_EXISTS.  It is illegal for
 * multiple callbacks to be registered for a single \c wpid
 * (OMPI_EXISTS will be returned in this case).
 *
 * \warning It is not legal for \c wpid to be -1 when registering a
 * callback.
 */
ORTE_DECLSPEC int orte_wait_cb(pid_t wpid, orte_wait_fn_t callback, void *data);

ORTE_DECLSPEC int orte_wait_cb_cancel(pid_t wpid);

ORTE_DECLSPEC int orte_wait_cb_disable(void);

ORTE_DECLSPEC int orte_wait_cb_enable(void);

/**
 * Setup to wait for an event
 *
 * This function is used to setup a trigger event that can be used elsewhere
 * in the code base where we want to wait for some event to
 * happen. For example, orterun uses this function to setup an event
 * that is used to notify orterun of abnormal and normal termination
 * so it can wakeup and exit cleanly.
 *
 * The event will be defined so that firing the provided trigger
 * will cause the event to trigger and callback to the provided
 * function
 */
ORTE_DECLSPEC int orte_wait_event(opal_event_t **event,
                                  orte_trigger_event_t *trig,
                                  char *trigger_name,
                                  void (*cbfunc)(int, short, void*));

/**
 * In a number of places in the code, we need to wait for something
 * to complete - for example, waiting for all launched procs to
 * report into the HNP. In such cases, we want to just call progress
 * so that any messages get processed, but otherwise "hold" the
 * program at this spot until the counter achieves the specified
 * value. We also want to provide a boolean flag, though, so that
 * we break out of the loop should something go wrong.
 */
#define ORTE_PROGRESSED_WAIT(failed, counter, limit)      \
    do {                                                  \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,        \
                            "progressed_wait: %s %d",     \
                             __FILE__, __LINE__));        \
        while (!(failed) && (counter) < (limit)) {        \
            opal_progress();                              \
        }                                                 \
    } while(0);                                           \


/**
 * Trigger a defined event
 *
 * This function will trigger a previously-defined event - as setup
 * by orte_wait_event - by firing the provided trigger
 */
ORTE_DECLSPEC void orte_trigger_event(orte_trigger_event_t *trig);

/**
 * Setup an event to process a message
 *
 * If we are in an OOB recv callback, we frequently cannot process
 * the received message until after we return from the callback to
 * avoid a potential loopback situation - i.e., where processing
 * the message can result in a message being sent somewhere that
 * subsequently causes the recv we are in to get called again.
 * This is typically the problem facing the daemons and HNP.
 *
 * To resolve this problem, we place the message to be processed on
 * a list, and create a zero-time event that calls the function
 * that will process the received message. The event library kindly
 * does not trigger this event until after we return from the recv
 * since the recv itself is considered an "event"! Thus, we will
 * always execute the specified event cb function -after- leaving
 * the recv.
 */
typedef struct {
    opal_object_t super;
    opal_event_t *ev;
    orte_process_name_t sender;
    opal_buffer_t *buffer;
    orte_rml_tag_t tag;
#if OPAL_ENABLE_DEBUG
    char *file;
    int line;
#endif
} orte_message_event_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_message_event_t);

#define ORTE_MESSAGE_EVENT_DELAY(delay, mev)                        \
    do {                                                            \
        struct timeval now;                                         \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,                  \
                            "defining message event delay: %s %d",  \
                            __FILE__, __LINE__));                   \
        now.tv_sec = delay/1000000;                                 \
        now.tv_usec = delay%1000000;                                \
        opal_evtimer_add(mev->ev, &now);                            \
    } while(0);

#if OPAL_ENABLE_DEBUG

#define ORTE_MESSAGE_EVENT(sndr, buf, tg, cbfunc)               \
    do {                                                        \
        orte_message_event_t *mev;                              \
        struct timeval now;                                     \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,              \
                            "defining message event: %s %d",    \
                            __FILE__, __LINE__));               \
        mev = OBJ_NEW(orte_message_event_t);                    \
        mev->sender.jobid = (sndr)->jobid;                      \
        mev->sender.vpid = (sndr)->vpid;                        \
        opal_dss.copy_payload(mev->buffer, (buf));              \
        mev->tag = (tg);                                        \
        mev->file = strdup((buf)->parent.cls_init_file_name);   \
        mev->line = (buf)->parent.cls_init_lineno;              \
        opal_evtimer_set(mev->ev, (cbfunc), mev);               \
        now.tv_sec = 0;                                         \
        now.tv_usec = 0;                                        \
        opal_evtimer_add(mev->ev, &now);                        \
    } while(0);

#else

#define ORTE_MESSAGE_EVENT(sndr, buf, tg, cbfunc)               \
    do {                                                        \
        orte_message_event_t *mev;                              \
        struct timeval now;                                     \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,              \
                            "defining message event: %s %d",    \
                            __FILE__, __LINE__));               \
        mev = OBJ_NEW(orte_message_event_t);                    \
        mev->sender.jobid = (sndr)->jobid;                      \
        mev->sender.vpid = (sndr)->vpid;                        \
        opal_dss.copy_payload(mev->buffer, (buf));              \
        mev->tag = (tg);                                        \
        opal_evtimer_set(mev->ev, (cbfunc), mev);               \
        now.tv_sec = 0;                                         \
        now.tv_usec = 0;                                        \
        opal_evtimer_add(mev->ev, &now);                        \
    } while(0);

#endif
    
/* Sometimes, we just need to get out of the event library so 
 * we can progress - and we need to pass a little info. For those 
 * cases, we define a zero-time event that passes info to a cbfunc 
 */ 
typedef struct { 
    opal_object_t super; 
    opal_event_t *ev; 
    orte_process_name_t proc; 
} orte_notify_event_t; 
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_notify_event_t); 

#define ORTE_NOTIFY_EVENT(cbfunc, data)                         \
    do {                                                        \
        struct timeval now;                                     \
        orte_notify_event_t *tmp;                               \
        tmp = OBJ_NEW(orte_notify_event_t);                     \
        tmp->proc.jobid = (data)->jobid;                        \
        tmp->proc.vpid = (data)->vpid;                          \
        opal_evtimer_set(tmp->ev, (cbfunc), tmp);               \
        now.tv_sec = 0;                                         \
        now.tv_usec = 0;                                        \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,              \
                            "defining notify event at %s:%d",   \
                            __FILE__, __LINE__));               \
        opal_evtimer_add(tmp->ev, &now);                        \
    } while(0);                                                 \

/**
 * In a number of places within the code, we want to setup a timer
 * to detect when some procedure failed to complete. For example,
 * when we launch the daemons, we frequently have no way to directly
 * detect that a daemon failed to launch. Setting a timer allows us
 * to automatically fail out of the launch if we don't hear from a
 * daemon in some specified time window.
 *
 * Computing the amount of time to wait takes a few lines of code, but
 * this macro encapsulates those lines along with the timer event
 * definition just as a convenience. It also centralizes the
 * necessary checks to ensure that the microsecond field is always
 * less than 1M since some systems care about that, and to ensure
 * that the computed wait time doesn't exceed the desired max
 * wait
 */
#define ORTE_DETECT_TIMEOUT(event, n, deltat, maxwait, cbfunc)              \
    do {                                                                    \
        struct timeval now;                                                 \
        opal_event_t *tmp;                                                  \
        int timeout;                                                        \
        tmp = (opal_event_t*)malloc(sizeof(opal_event_t));                  \
        opal_evtimer_set(tmp, (cbfunc), NULL);                              \
        timeout = (deltat) * (n);                                           \
        if ((maxwait) > 0 && timeout > (maxwait)) {                         \
            timeout = (maxwait);                                            \
        }                                                                   \
        now.tv_sec = timeout/1000000;                                       \
        now.tv_usec = timeout%1000000;                                      \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,                          \
                             "defining timeout: %ld sec %ld usec at %s:%d", \
                            (long)now.tv_sec, (long)now.tv_usec,            \
                            __FILE__, __LINE__));                           \
        opal_evtimer_add(tmp, &now);                                        \
        *(event) = tmp;                                                     \
    }while(0);                                                              \


/**
 * There are places in the code where we just want to periodically
 * wakeup to do something, and then go back to sleep again. Setting
 * a timer allows us to do this
 */
#define ORTE_TIMER_EVENT(sec, usec, cbfunc)                                     \
    do {                                                                        \
        struct timeval now;                                                     \
        opal_event_t *tmp;                                                      \
        tmp = (opal_event_t*)malloc(sizeof(opal_event_t));                      \
        opal_evtimer_set(tmp, (cbfunc), tmp);                                   \
        now.tv_sec = (sec);                                                     \
        now.tv_usec = (usec);                                                   \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,                              \
                            "defining timer event: %ld sec %ld usec at %s:%d",  \
                            (long)now.tv_sec, (long)now.tv_usec,                \
                            __FILE__, __LINE__));                               \
        opal_evtimer_add(tmp, &now);                                            \
    }while(0);                                                                  \


/**
 * \internal
 *
 * Initialize the wait system (allocate mutexes, etc.)
 */
ORTE_DECLSPEC int orte_wait_init(void);

/**
 * Kill all processes we are waiting on.
 */
ORTE_DECLSPEC int orte_wait_kill(int sig);

/**
 * \internal
 *
 * Finalize the wait system (deallocate mutexes, etc.)
 */
ORTE_DECLSPEC int orte_wait_finalize(void);

END_C_DECLS

#endif /* #ifndef ORTE_WAIT_H */
