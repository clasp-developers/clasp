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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#include "opal/runtime/opal_progress.h"
#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/constants.h"
#include "opal/mca/timer/base/base.h"
#include "opal/util/output.h"

#define OPAL_PROGRESS_USE_TIMERS (OPAL_TIMER_CYCLE_SUPPORTED || OPAL_TIMER_USEC_SUPPORTED)

/* 
 * default parameters 
 */
static int opal_progress_event_flag = OPAL_EVLOOP_ONELOOP;
volatile int32_t opal_progress_thread_count = 0;
int opal_progress_spin_count = 10000;


/*
 * Local variables
 */
#if OPAL_HAVE_THREAD_SUPPORT
static opal_atomic_lock_t progress_lock;
#endif  /* OPAL_HAVE_THREAD_SUPPORT */

/* callbacks to progress */
static opal_progress_callback_t *callbacks = NULL;
static size_t callbacks_len = 0;
static size_t callbacks_size = 0;

/* do we want to call sched_yield() if nothing happened */
static int call_yield = 1;

#if OPAL_PROGRESS_USE_TIMERS
static opal_timer_t event_progress_last_time = 0;
static opal_timer_t event_progress_delta = 0;
#else
/* current count down until we tick the event library */
static int32_t event_progress_counter = 0;
/* reset value for counter when it hits 0 */
static int32_t event_progress_delta = 0;
#endif
/* users of the event library from MPI cause the tick rate to 
   be every time */
static int32_t num_event_users = 0;

/* How deep are we in opal_progress recursion? */
#if OPAL_HAVE_THREAD_SUPPORT
volatile 
#endif
uint32_t opal_progress_recursion_depth_counter = 0;

#if OPAL_ENABLE_DEBUG
static int debug_output = -1;
#endif

/**
 * Fake callback used for threading purpose when one thread
 * progesses callbacks while another unregister somes. The root
 * of the problem is that we allow modifications of the callback
 * array directly from the callbacks themselves. Now if
 * writing a pointer is atomic, we should not have any more
 * problems.
 */
static int fake_cb(void) { return 0; }

/* init the progress engine - called from orte_init */
int
opal_progress_init(void)
{
#if OPAL_ENABLE_DEBUG
    int param, value;
#endif

    /* reentrant issues */
#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_init(&progress_lock, OPAL_ATOMIC_UNLOCKED);
#endif  /* OPAL_HAVE_THREAD_SUPPORT */

    /* set the event tick rate */
    opal_progress_set_event_poll_rate(10000);

#if OPAL_ENABLE_DEBUG
    param = mca_base_param_find("opal", NULL, "progress_debug");
    mca_base_param_lookup_int(param, &value);
    if (value) {
        debug_output = opal_output_open(NULL);
    }
#endif

    OPAL_OUTPUT((debug_output, "progress: initialized event flag to: %x",
                 opal_progress_event_flag));                 
    OPAL_OUTPUT((debug_output, "progress: initialized yield_when_idle to: %s",
                 call_yield == 0 ? "false" : "true"));
    OPAL_OUTPUT((debug_output, "progress: initialized num users to: %d",
                 num_event_users));
    OPAL_OUTPUT((debug_output, "progress: initialized poll rate to: %ld",
                 (long) event_progress_delta));

    return OPAL_SUCCESS;
}


int
opal_progress_finalize(void)
{
    /* free memory associated with the callbacks */
#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    callbacks_len = 0;
    callbacks_size = 0;
    if (NULL != callbacks) {
        free(callbacks);
        callbacks = NULL;
    }

#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_unlock(&progress_lock);
#endif

    return OPAL_SUCCESS;
}


/*
 * Progress the event library and any functions that have registered to 
 * be called.  We don't propogate errors from the progress functions,
 * so no action is taken if they return failures.  The functions are
 * expected to return the number of events progressed, to determine
 * whether or not we should call sched_yield() during MPI progress.
 * This is only losely tracked, as an error return can cause the number
 * of progressed events to appear lower than it actually is.  We don't
 * care, as the cost of that happening is far outweighed by the cost
 * of the if checks (they were resulting in bad pipe stalling behavior)
 */
void
opal_progress(void)
{
    size_t i;
    int events = 0;

#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_add(&opal_progress_recursion_depth_counter, 1);
#else
    ++opal_progress_recursion_depth_counter;
#endif
    if( opal_progress_event_flag != 0 ) {
#if (OPAL_ENABLE_PROGRESS_THREADS == 0) && OPAL_HAVE_WORKING_EVENTOPS
#if OPAL_PROGRESS_USE_TIMERS
#if OPAL_TIMER_USEC_NATIVE
        opal_timer_t now = opal_timer_base_get_usec();
#else
        opal_timer_t now = opal_timer_base_get_cycles();
#endif  /* OPAL_TIMER_USEC_NATIVE */
    /* trip the event library if we've reached our tick rate and we are
       enabled */
        if (now - event_progress_last_time > event_progress_delta ) {
                event_progress_last_time = (num_event_users > 0) ? 
                    now - event_progress_delta : now;

                events += opal_event_loop(opal_progress_event_flag);
        }

#else /* OPAL_PROGRESS_USE_TIMERS */
    /* trip the event library if we've reached our tick rate and we are
       enabled */
        if (OPAL_THREAD_ADD32(&event_progress_counter, -1) <= 0 ) {
                event_progress_counter = 
                    (num_event_users > 0) ? 0 : event_progress_delta;
                events += opal_event_loop(opal_progress_event_flag);
        }
#endif /* OPAL_PROGRESS_USE_TIMERS */

#endif /* OPAL_ENABLE_PROGRESS_THREADS == 0 && OPAL_HAVE_WORKING_EVENTOPS */
    }

    /* progress all registered callbacks */
    for (i = 0 ; i < callbacks_len ; ++i) {
        events += (callbacks[i])();
    }

#if defined(__WINDOWS__) || defined(HAVE_SCHED_YIELD)
    if (call_yield && events <= 0) {
        /* If there is nothing to do - yield the processor - otherwise
         * we could consume the processor for the entire time slice. If
         * the processor is oversubscribed - this will result in a best-case
         * latency equivalent to the time-slice.
         */
#if defined(__WINDOWS__)
        SwitchToThread();
#else
        sched_yield();
#endif  /* defined(__WINDOWS__) */
    }
#endif  /* defined(__WINDOWS__) || defined(HAVE_SCHED_YIELD) */
#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_add(&opal_progress_recursion_depth_counter, -1);
#else
    --opal_progress_recursion_depth_counter;
#endif
}


int
opal_progress_set_event_flag(int flag)
{
    int tmp = opal_progress_event_flag;
    opal_progress_event_flag = flag;

    OPAL_OUTPUT((debug_output, "progress: set_event_flag setting to %d", flag));

    return tmp;
}


void
opal_progress_event_users_increment(void)
{
    int32_t val;
    val = opal_atomic_add_32(&num_event_users, 1);

    OPAL_OUTPUT((debug_output, "progress: event_users_increment setting count to %d", val));

#if OPAL_PROGRESS_USE_TIMERS
    /* force an update next round (we'll be past the delta) */
    event_progress_last_time -= event_progress_delta;
#else
    /* always reset the tick rate - can't hurt */
    event_progress_counter = 0;
#endif
}


void
opal_progress_event_users_decrement(void)
{
    int32_t val;
    val = opal_atomic_sub_32(&num_event_users, 1);

    OPAL_OUTPUT((debug_output, "progress: event_users_decrement setting count to %d", val));

#if !OPAL_PROGRESS_USE_TIMERS
   /* start now in delaying if it's easy */
   if (val >= 0) {
       event_progress_counter = event_progress_delta;
   }
#endif
}


bool
opal_progress_set_yield_when_idle(bool yieldopt)
{
    bool tmp = (call_yield == 0) ? false : true;
    call_yield = (yieldopt) ? 1 : 0;

    OPAL_OUTPUT((debug_output, "progress: progress_set_yield_when_idle to %s",
                                    call_yield == 0 ? "false" : "true"));

    return tmp;
}


void
opal_progress_set_event_poll_rate(int polltime)
{
    OPAL_OUTPUT((debug_output, "progress: progress_set_event_poll_rate(%d)", polltime));

#if OPAL_PROGRESS_USE_TIMERS
    event_progress_delta = 0;
#  if OPAL_TIMER_USEC_NATIVE
    event_progress_last_time = opal_timer_base_get_usec();
#  else
    event_progress_last_time = opal_timer_base_get_cycles();
#  endif
#else
    event_progress_counter = event_progress_delta = 0;
#endif

    if (polltime == 0) {
#if OPAL_PROGRESS_USE_TIMERS
        /* user specified as never tick - tick once per minute */
        event_progress_delta = 60 * 1000000;
#else
        /* user specified as never tick - don't count often */
        event_progress_delta = INT_MAX;
#endif
    } else {
#if OPAL_PROGRESS_USE_TIMERS
        event_progress_delta = polltime;
#else
        /* subtract one so that we can do post-fix subtraction
           in the inner loop and go faster */
        event_progress_delta = polltime - 1;
#endif
    }

#if OPAL_PROGRESS_USE_TIMERS && !OPAL_TIMER_USEC_NATIVE
    /*  going to use cycles for counter.  Adjust specified usec into cycles */
    event_progress_delta = event_progress_delta * opal_timer_base_get_freq() / 1000000;
#endif
}


int
opal_progress_register(opal_progress_callback_t cb)
{
    int ret = OPAL_SUCCESS;
    size_t index;

#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    /* see if we need to allocate more space */
    if (callbacks_len + 1 > callbacks_size) {
        opal_progress_callback_t *tmp;
        tmp = (opal_progress_callback_t*)realloc(callbacks, sizeof(opal_progress_callback_t) * (callbacks_size + 4));
        if (tmp == NULL) {
            ret = OPAL_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        }
        /* registering fake callbacks to fill callbacks[] */
        for( index = callbacks_len + 1 ;  index < callbacks_size + 4 ; index++) {
            tmp[index] = &fake_cb;
        }

        callbacks = tmp;
        callbacks_size += 4;
    }

    callbacks[callbacks_len++] = cb;

 cleanup:

#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_unlock(&progress_lock);
#endif

    return ret;
}

int
opal_progress_unregister(opal_progress_callback_t cb)
{
    size_t i;
    int ret = OPAL_ERR_NOT_FOUND;

#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_lock(&progress_lock);
#endif

    for (i = 0 ; i < callbacks_len ; ++i) {
        if (cb == callbacks[i]) {
            callbacks[i] = &fake_cb;
            ret = OPAL_SUCCESS;
            break;
        }
    }
    
    /* If we found the function we're unregistering: If callbacks_len
       is 0, we're not goig to do anything interesting anyway, so
       skip.  If callbacks_len is 1, it will soon be 0, so no need to
       do any repacking.  size_t can be unsigned, so 0 - 1 is bad for
       a loop condition :). */
    if (OPAL_SUCCESS == ret) {
        if (callbacks_len > 1 ) {
            /* now tightly pack the array */
            for ( ; i < callbacks_len - 1 ; ++i) {
                callbacks[i] = callbacks[i + 1];
            }
        }
        callbacks[callbacks_len - 1] = &fake_cb;
        callbacks_len--;
    }

#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_unlock(&progress_lock);
#endif

    return ret;
}
