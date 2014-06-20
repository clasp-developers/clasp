/*
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2000-2004 Niels Provos <provos@citi.umich.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "opal_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif


#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#  ifndef HAVE_WINSOCK2_H
#include <sys/_libevent_time.h>
#  endif
#endif
#ifndef HAVE_TIMERADD
#include <sys/_timeradd.h>
#endif
#include <sys/queue.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "opal/event/event.h"
#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/constants.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"

#include "event-internal.h"
#include "evutil.h"
#include "log.h"

#if defined(HAVE_EVENT_PORTS) && HAVE_EVENT_PORTS
extern const struct eventop evportops;
#endif
#if defined(HAVE_SELECT) && HAVE_SELECT
extern const struct eventop selectops;
#endif
#if defined(HAVE_POLL) && HAVE_POLL && HAVE_WORKING_POLL
extern const struct eventop pollops;
#endif
#if defined(HAVE_EPOLL) && HAVE_EPOLL
extern const struct eventop epollops;
#endif
#if defined(HAVE_WORKING_KQUEUE) && HAVE_WORKING_KQUEUE
extern const struct eventop kqops;
#endif
#ifdef HAVE_DEVPOLL
extern const struct eventop devpollops;
#endif
#ifdef WIN32
extern const struct eventop win32ops;
#endif

/* In order of preference */
static const struct eventop *eventops[] = {
#if defined(HAVE_EVENT_PORTS) && HAVE_EVENT_PORTS
	&evportops,
#endif
#if defined(HAVE_WORKING_KQUEUE) && HAVE_WORKING_KQUEUE
	&kqops,
#endif
#if defined(HAVE_EPOLL) && HAVE_EPOLL
	&epollops,
#endif
#if defined(HAVE_DEVPOLL) && HAVE_DEVPOLL
	&devpollops,
#endif
#if defined(HAVE_POLL) && HAVE_POLL && HAVE_WORKING_POLL
	&pollops,
#endif
#if defined(HAVE_SELECT) && HAVE_SELECT
	&selectops,
#endif
#ifdef WIN32
	&win32ops,
#endif
	NULL
};

/* Global state */
struct event_base *current_base = NULL;
extern struct event_base *evsignal_base;
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
static int use_monotonic;
#endif  /* defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC) */

/* Prototypes */
static void	event_queue_insert(struct event_base *, struct event *, int);
static void	event_queue_remove(struct event_base *, struct event *, int);
static int	event_haveevents(struct event_base *);

static void	event_process_active(struct event_base *);

static int	timeout_next(struct event_base *, struct timeval **);
static void	timeout_process(struct event_base *);
#if 0
/* Let's not delete this yet */
static void	timeout_correct(struct event_base *, struct timeval *);
#endif

static void
detect_monotonic(void)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
	struct timespec	ts;

	if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
		use_monotonic = 1;
#endif
}

static int
gettime(struct event_base *base, struct timeval *tp)
{
	if (base->tv_cache.tv_sec) {
		*tp = base->tv_cache;
		return (0);
	}

#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
	if (use_monotonic) {
		struct timespec	ts;

		if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1)
			return (-1);

		tp->tv_sec = ts.tv_sec;
		tp->tv_usec = ts.tv_nsec / 1000;
		return (0);
	}
#endif

	return (evutil_gettimeofday(tp, NULL));
}


OPAL_DECLSPEC opal_mutex_t opal_event_lock;
static int  opal_event_inited = 0;
static bool opal_event_enabled = false;
#if OPAL_ENABLE_PROGRESS_THREADS
static opal_thread_t opal_event_thread;
static opal_event_t opal_event_pipe_event;
static int opal_event_pipe[2];
static int opal_event_pipe_signalled;
#endif

bool opal_event_progress_thread(void)
{
#if OPAL_ENABLE_PROGRESS_THREADS
	return opal_using_threads() ? opal_thread_self_compare(&opal_event_thread) : true;
#else
	return true;
#endif
}
#if OPAL_ENABLE_PROGRESS_THREADS
/* run loop for dispatch thread */
static void* opal_event_run(opal_object_t* arg)
{
	/* Open MPI: Prevent compiler warnings about unused variables */
#if defined(NDEBUG)
	event_loop(0);
#else
	int rc = event_loop(0);
	assert(rc >= 0);
#endif

	opal_mutex_lock(&opal_event_lock);
	event_del(&opal_event_pipe_event);
	close(opal_event_pipe[0]);
	close(opal_event_pipe[1]);
	opal_event_pipe[0] = -1;
	opal_event_pipe[1] = -1;
	opal_mutex_unlock(&opal_event_lock);

	return NULL;
}
#endif /* OPAL_ENABLE_PROGRESS_THREADS */


#if OPAL_ENABLE_PROGRESS_THREADS
static void opal_event_pipe_handler(int sd, short flags, void* user)
{
	unsigned char byte;
	if(read(sd, &byte, 1) < 0) {
		opal_output(0, "opal_event_pipe: read failed with: errno=%d\n", errno);
		opal_event_del(&opal_event_pipe_event);
	}
}
#endif /* OPAL_ENABLE_PROGRESS_THREADS */

static char** opal_event_module_include = NULL;

static int opal_event_allow_system( const char* eventop_name)
{
    char** argv = opal_event_module_include;

    /* if the user specified an event interface - use these exclusively */
    while(argv && *argv) {
        if( 0 == strcmp(*argv, "all") ) return 1;  /* all match */
        if( 0 == strcmp(*argv, eventop_name) ) return 1;
        argv++;
    }
    return 0;
}

int
opal_event_init(void)
{
    struct event_base *base;
    char* event_module_include;

    if(opal_event_inited++ != 0)
        return OPAL_SUCCESS;

#if OPAL_HAVE_WORKING_EVENTOPS

    /* Retrieve the upper level specified event system, if any.
     * Default to select() on OS X and poll() everywhere else because
     * various parts of OMPI / ORTE use libevent with pty's.  pty's
     * *only* work with select on OS X (tested on Tiger and Leopard);
     * we *know* that both select and poll works with pty's everywhere
     * else we care about (other mechansisms such as epoll *may* work
     * with pty's -- we have not tested comprehensively with newer
     * versions of Linux, etc.).  So the safe thing to do is:
     *
     * - On OS X, default to using "select" only
     * - Everywhere else, default to using "poll" only (because poll
     *   is more scalable than select)
     *
     * An upper layer may override this setting if it knows that pty's
     * won't be used with libevent.  For example, we currently have
     * ompi_mpi_init() set to use "all" (to include epoll and friends)
     * so that the TCP BTL can be a bit more scalable -- because we
     * *know* that MPI apps don't use pty's with libevent.  
     * Note that other tools explicitly *do* use pty's with libevent:
     *
     * - orted
     * - orterun (probably only if it launches locally)
     * - ...?
     */
    {
        const struct eventop** _eventop = eventops;
        char available_eventops[1024] = "none";
        char* help_msg = NULL;
        int position = 0;

        while( NULL != (*_eventop) ) {
            if( 0 != position ) {
                position += snprintf( available_eventops + position,
                                      (size_t)(1024 - position),
                                      ", %s", (*_eventop)->name );
            } else {
                position += snprintf( available_eventops + position,
                                      (size_t)(1024 - position),
                                      "%s", (*_eventop)->name );
            }
            available_eventops[position] = '\0';
            _eventop++;  /* go to the next available eventop */
        }
        asprintf( &help_msg, 
                  "Comma-delimited list of libevent subsystems "
                  "to use (%s -- available on your platform)",
                  available_eventops );
        mca_base_param_reg_string_name("opal", "event_include",
                                       help_msg, false, false, 
#ifdef __APPLE__
                                       "select",
#else
#  ifdef __WINDOWS__
                                       "win32",
#  else
                                       "poll",
#  endif
#endif
                                       &event_module_include);
        free(help_msg);  /* release the help message */
    }
    if (NULL == event_module_include) {
        /* Shouldn't happen, but... */
        event_module_include = strdup("select");
    }
    opal_event_module_include = opal_argv_split(event_module_include,',');
    free(event_module_include);

    base = event_base_new();
    OBJ_CONSTRUCT(&opal_event_lock, opal_mutex_t);
    if (base != NULL)
        opal_current_base = base;
    opal_event_enable();
#endif
    return OPAL_SUCCESS;
}


struct event_base *
event_base_new(void)
{
	int i;
	struct event_base *base;

	if ((base = calloc(1, sizeof(struct event_base))) == NULL)
		event_err(1, "%s: calloc", __func__);

	detect_monotonic();
	gettime(base, &base->event_tv);
	
	min_heap_ctor(&base->timeheap);
	TAILQ_INIT(&base->eventqueue);
	base->sig.ev_signal_pair[0] = -1;
	base->sig.ev_signal_pair[1] = -1;
	
	base->evbase = NULL;
	for (i = 0; eventops[i] && !base->evbase; i++) {
		/* Allow only the user selected event mechanisms to be initialized */
		if( !opal_event_allow_system(eventops[i]->name) ) continue;
		base->evsel = eventops[i];

		base->evbase = base->evsel->init(base);
	}

	if (base->evbase == NULL)
		event_errx(1, "%s: no event mechanism available", __func__);

	if (evutil_getenv("EVENT_SHOW_METHOD")) 
		event_msgx("libevent using: %s\n",
			   base->evsel->name);

	/* allocate a single active event queue */
	event_base_priority_init(base, 1);

	return (base);
}

int opal_event_fini(void)
{
    opal_event_disable();
    opal_event_inited--;
    if (NULL != opal_event_module_include) {
        opal_argv_free(opal_event_module_include);
    }
    if( NULL != opal_current_base ) {
        event_base_free(opal_current_base);
        opal_current_base = NULL;
    }

    return OPAL_SUCCESS;
}

int opal_event_enable(void)
{
#if OPAL_ENABLE_PROGRESS_THREADS
	if(opal_using_threads()) {
		int rc;

		opal_mutex_lock(&opal_event_lock);
		if(opal_event_inited > 0 && opal_event_enabled == true) {
			opal_mutex_unlock(&opal_event_lock);
			return OPAL_SUCCESS;
		}

		/* create a pipe to signal the event thread */
		if(pipe(opal_event_pipe) != 0) {
			opal_output(0, "opal_event_init: pipe() failed with errno=%d\n", errno);
			opal_mutex_unlock(&opal_event_lock);
			return OPAL_ERROR;
		}

		opal_event_pipe_signalled = 1;
		event_set(
				&opal_event_pipe_event,
				opal_event_pipe[0],
				EV_READ|EV_PERSIST,
				opal_event_pipe_handler,
				0);
		event_add(&opal_event_pipe_event, 0);
		opal_event_pipe_signalled = 0;

		/* spin up a thread to dispatch events */
		OBJ_CONSTRUCT(&opal_event_thread, opal_thread_t);
		opal_event_enabled = true;
		opal_event_thread.t_run = opal_event_run;
		if((rc = opal_thread_start(&opal_event_thread)) != OPAL_SUCCESS) {
			opal_mutex_unlock(&opal_event_lock);
			return rc;
		}
		opal_mutex_unlock(&opal_event_lock);
	} else {
		opal_event_pipe[0] = -1;
		opal_event_pipe[1] = -1;
		opal_event_enabled = true;
	}
#else
	opal_event_enabled = true;
#endif
	return OPAL_SUCCESS;
}


int opal_event_disable(void)
{
#if OPAL_ENABLE_PROGRESS_THREADS
	if(opal_using_threads()) {
		opal_mutex_lock(&opal_event_lock);
		if(opal_event_inited > 0 && opal_event_enabled == false) {
			opal_mutex_unlock(&opal_event_lock);
			return OPAL_SUCCESS;
		}

		opal_event_enabled = false;
		if(opal_event_pipe_signalled == 0) {
			unsigned char byte = 0;
			if(write(opal_event_pipe[1], &byte, 1) != 1)
				opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
			opal_event_pipe_signalled++;
		}
		opal_mutex_unlock(&opal_event_lock);
		opal_thread_join(&opal_event_thread, NULL);
	} else {
		opal_event_enabled = false;
	}
#else
	opal_event_enabled = false;
#endif
	return OPAL_SUCCESS;
}

int opal_event_restart(void)
{
#if OPAL_HAVE_WORKING_EVENTOPS && !defined(__WINDOWS__)
#if OPAL_ENABLE_PROGRESS_THREADS
	opal_mutex_lock(&opal_event_lock);
	if(opal_event_pipe[0] >= 0) {
		event_del(&opal_event_pipe_event); 
		/* do not close pipes - in case of bproc_vrfork they are not open 
		 * and we may close something else 
		 */
		opal_event_pipe[0] = -1;
		opal_event_pipe[1] = -1;
	}
	opal_event_enabled = false;
	opal_mutex_unlock(&opal_event_lock);
#endif

	opal_event_enable();
	return (OPAL_SUCCESS);
#else /* OPAL_HAVE_WORKING_EVENTOPS */
	return OPAL_ERR_NOT_SUPPORTED;
#endif
}

void
event_base_free(struct event_base *base)
{
	int i, n_deleted=0;
	struct event *ev;

	if (base == NULL && current_base)
		base = current_base;
	if (base == current_base)
		current_base = NULL;

	/* XXX(niels) - check for internal events first */
	assert(base);
	/* Delete all non-internal events. */
	for (ev = TAILQ_FIRST(&base->eventqueue); ev; ) {
		struct event *next = TAILQ_NEXT(ev, ev_next);
		if (!(ev->ev_flags & EVLIST_INTERNAL)) {
			opal_event_del(ev);
			++n_deleted;
		}
		ev = next;
	}
	while ((ev = min_heap_top(&base->timeheap)) != NULL) {
		opal_event_del(ev);
		++n_deleted;
	}

	for (i = 0; i < base->nactivequeues; ++i) {
		for (ev = TAILQ_FIRST(base->activequeues[i]); ev; ) {
			struct event *next = TAILQ_NEXT(ev, ev_active_next);
			if (!(ev->ev_flags & EVLIST_INTERNAL)) {
				event_del(ev);
				++n_deleted;
			}
			ev = next;
		}
	}

	if (n_deleted)
		event_debug(("%s: %d events were still set in base",
			__func__, n_deleted));

	if (base->evsel->dealloc != NULL)
		base->evsel->dealloc(base, base->evbase);

#if 0
        /* Per ticket #1805
           (https://svn.open-mpi.org/trac/ompi/ticket/1805), these
           asserts are temporarily removed until we can have the rest
           of the code base absolutely clean up all pending libevents,
           we're #if 0'ing out these asserts so that we stop dumping
           core. :-\ */
	for (i = 0; i < base->nactivequeues; ++i)
		assert(TAILQ_EMPTY(base->activequeues[i]));

	assert(min_heap_empty(&base->timeheap));
#endif
	min_heap_dtor(&base->timeheap);

	for (i = 0; i < base->nactivequeues; ++i)
		free(base->activequeues[i]);
	free(base->activequeues);

	assert(TAILQ_EMPTY(&base->eventqueue));

	free(base);
}

/* reinitialized the event base after a fork */
int
event_reinit(struct event_base *base)
{
	const struct eventop *evsel = base->evsel;
	void *evbase = base->evbase;
	int res = 0;
	struct event *ev;

	/* check if this event mechanism requires reinit */
	if (!evsel->need_reinit)
		return (0);

	/* prevent internal delete */
	if (base->sig.ev_signal_added) {
		/* we cannot call event_del here because the base has
		 * not been reinitialized yet. */
		event_queue_remove(base, &base->sig.ev_signal,
		    EVLIST_INSERTED);
		if (base->sig.ev_signal.ev_flags & EVLIST_ACTIVE)
			event_queue_remove(base, &base->sig.ev_signal,
			    EVLIST_ACTIVE);
		base->sig.ev_signal_added = 0;
	}
	
	if (base->evsel->dealloc != NULL)
		base->evsel->dealloc(base, base->evbase);
	evbase = base->evbase = evsel->init(base);
	if (base->evbase == NULL)
		event_errx(1, "%s: could not reinitialize event mechanism",
		    __func__);

	TAILQ_FOREACH(ev, &base->eventqueue, ev_next) {
		if (evsel->add(evbase, ev) == -1)
			res = -1;
	}

	return (res);
}

int
event_priority_init(int npriorities)
{
  return event_base_priority_init(current_base, npriorities);
}

int
event_base_priority_init(struct event_base *base, int npriorities)
{
	int i;

	if (base->event_count_active)
		return (-1);

	if (base->nactivequeues && npriorities != base->nactivequeues) {
		for (i = 0; i < base->nactivequeues; ++i) {
			free(base->activequeues[i]);
		}
		free(base->activequeues);
	}

	/* Allocate our priority queues */
	base->nactivequeues = npriorities;
	base->activequeues = (struct event_list **)
	    calloc(base->nactivequeues, sizeof(struct event_list *));
	if (base->activequeues == NULL)
		event_err(1, "%s: calloc", __func__);

	for (i = 0; i < base->nactivequeues; ++i) {
		base->activequeues[i] = malloc(sizeof(struct event_list));
		if (base->activequeues[i] == NULL)
			event_err(1, "%s: malloc", __func__);
		TAILQ_INIT(base->activequeues[i]);
	}

	return (0);
}

int
event_haveevents(struct event_base *base)
{
	return (base->event_count > 0);
}

/*
 * Active events are stored in priority queues.  Lower priorities are always
 * process before higher priorities.  Low priority events can starve high
 * priority ones.
 */

static void
event_process_active(struct event_base *base)
{
	struct event *ev;
	struct event_list *activeq = NULL;
	int i;
	short ncalls;

	for (i = 0; i < base->nactivequeues; ++i) {
		if (TAILQ_FIRST(base->activequeues[i]) != NULL) {
			activeq = base->activequeues[i];
			break;
		}
	}

	assert(activeq != NULL);

	for (ev = TAILQ_FIRST(activeq); ev; ev = TAILQ_FIRST(activeq)) {
		if (ev->ev_events & EV_PERSIST)
			event_queue_remove(base, ev, EVLIST_ACTIVE);
		else
			event_del(ev);
		
		/* Allows deletes to work */
		ncalls = ev->ev_ncalls;
		ev->ev_pncalls = &ncalls;
		while (ncalls) {
			ncalls--;
			ev->ev_ncalls = ncalls;
                        OPAL_THREAD_UNLOCK(&opal_event_lock);
			(*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
			OPAL_THREAD_LOCK(&opal_event_lock);

			if (base->event_break)
				return;
		}
	}
}

/*
 * Wait continously for events.  We exit only if no events are left.
 */

int
event_dispatch(void)
{
	return (event_loop(0));
}

int
event_base_dispatch(struct event_base *event_base)
{
  return (event_base_loop(event_base, 0));
}

const char *
event_base_get_method(struct event_base *base)
{
	assert(base);
	return (base->evsel->name);
}

static void
event_loopexit_cb(int fd, short what, void *arg)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	struct event_base *base = arg;
	base->event_gotterm = 1;
#endif /* OPAL_HAVE_WORKING_EVENTOPS */
}

/* not thread safe */
int
event_loopexit(const struct timeval *tv)
{
	return (event_once(-1, EV_TIMEOUT, event_loopexit_cb,
		    current_base, tv));
}

int
event_base_loopexit(struct event_base *event_base, const struct timeval *tv)
{
	return (event_base_once(event_base, -1, EV_TIMEOUT, event_loopexit_cb,
		    event_base, tv));
}

/* not thread safe */
int
event_loopbreak(void)
{
	return (event_base_loopbreak(current_base));
}

int
event_base_loopbreak(struct event_base *event_base)
{
	if (event_base == NULL)
		return (-1);

	event_base->event_break = 1;
	return (0);
}



/* not thread safe */

int
event_loop(int flags)
{
	return event_base_loop(current_base, flags);
}

int
event_base_loop(struct event_base *base, int flags)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	const struct eventop *evsel = base->evsel;
	void *evbase = base->evbase;
	struct timeval tv;
	struct timeval *tv_p;
	int res, done;
#endif  /* OPAL_HAVE_WORKING_EVENTOPS */

        if (opal_event_inited == false)
		return(0);

#if OPAL_HAVE_WORKING_EVENTOPS
	/* clear time cache */
	base->tv_cache.tv_sec = 0;

	res = OPAL_THREAD_TRYLOCK(&opal_event_lock);
        if (0 != res) return 0;
        
	if (base->sig.ev_signal_added)
		evsignal_base = base;
	done = 0;
	while (!done && opal_event_enabled) {
		/* Terminate the loop if we have been asked to */
		if (base->event_gotterm) {
			base->event_gotterm = 0;
			break;
		}

		if (base->event_break) {
			base->event_break = 0;
			break;
		}
#if 0
                /* OMPI: George wants to comment this out for now */
		OPAL_THREAD_UNLOCK(&opal_event_lock);
                timeout_correct(base, &tv);
#endif

		tv_p = &tv;
		if (!base->event_count_active && !(flags & EVLOOP_NONBLOCK)) {
			timeout_next(base, &tv_p);
		} else {
			/* 
			 * if we have active events, we just poll new events
			 * without waiting.
			 */
			evutil_timerclear(&tv);
		}
		
		/* If we have no events, we just exit */
		if (!event_haveevents(base)) {
                    OPAL_THREAD_UNLOCK(&opal_event_lock);
                    event_debug(("%s: no events registered.", __func__));
                    return (1);
		}

		/* update last old time */
		gettime(base, &base->event_tv);

		/* clear time cache */
		base->tv_cache.tv_sec = 0;

#if OPAL_ENABLE_PROGRESS_THREADS
		opal_event_pipe_signalled = 0;
#endif

		res = evsel->dispatch(base, evbase, tv_p);

#if OPAL_ENABLE_PROGRESS_THREADS
		opal_event_pipe_signalled = 1;
#endif

		if (res == -1)
                    {
                        opal_output(0, "%s: ompi_evesel->dispatch() failed.", __func__);
			OPAL_THREAD_UNLOCK(&opal_event_lock);
                        return (-1);
                    }
		gettime(base, &base->tv_cache);

		timeout_process(base);

		if (base->event_count_active) {
			event_process_active(base);
			if (!base->event_count_active && (flags & (EVLOOP_ONCE|EVLOOP_ONELOOP)))
				done = 1;
		} else if (flags & (EVLOOP_NONBLOCK|EVLOOP_ONELOOP))
			done = 1;
	}

	/* clear time cache */
	base->tv_cache.tv_sec = 0;

	event_debug(("%s: asked to terminate loop.", __func__));
        
	OPAL_THREAD_UNLOCK(&opal_event_lock);
	return (base->event_count_active);
#else
        return 0;
#endif
}

/* Sets up an event for processing once */

struct event_once {
	struct event ev;

	void (*cb)(int, short, void *);
	void *arg;
};

/* One-time callback, it deletes itself */

static void
event_once_cb(int fd, short events, void *arg)
{
	struct event_once *eonce = arg;

	(*eonce->cb)(fd, events, eonce->arg);
	free(eonce);
}

/* not threadsafe, event scheduled once. */
int
event_once(int fd, short events,
    void (*callback)(int, short, void *), void *arg, const struct timeval *tv)
{
	return event_base_once(current_base, fd, events, callback, arg, tv);
}

/* Schedules an event once */
int
event_base_once(struct event_base *base, int fd, short events,
    void (*callback)(int, short, void *), void *arg, const struct timeval *tv)
{
	struct event_once *eonce;
	struct timeval etv;
	int res;

	/* We cannot support signals that just fire once */
	if (events & EV_SIGNAL)
		return (-1);

	if ((eonce = (struct event_once *)calloc(1, sizeof(struct event_once))) == NULL)
		return (-1);

	eonce->cb = callback;
	eonce->arg = arg;

	if (events == EV_TIMEOUT) {
		if (tv == NULL) {
			evutil_timerclear(&etv);
			tv = &etv;
		}

		opal_evtimer_set(&eonce->ev, event_once_cb, eonce);
	} else if (events & (EV_READ|EV_WRITE)) {
		events &= EV_READ|EV_WRITE;

		event_set(&eonce->ev, fd, events, event_once_cb, eonce);
	} else {
		/* Bad event combination */
		free(eonce);
		return (-1);
	}

	res = event_base_set(base, &eonce->ev);
	if (res == 0)
		res = event_add(&eonce->ev, tv);
	if (res != 0) {
		free(eonce);
		return (res);
	}

	return (0);
}

void
event_set(struct event *ev, int fd, short events,
	  void (*callback)(int, short, void *), void *arg)
{
	/* Take the current base - caller needs to set the real base later */
	ev->ev_base = current_base;

	ev->ev_callback = callback;
	ev->ev_arg = arg;
	ev->ev_fd = fd;
	ev->ev_events = events;
	ev->ev_res = 0;
	ev->ev_flags = EVLIST_INIT;
	ev->ev_ncalls = 0;
	ev->ev_pncalls = NULL;

	min_heap_elem_init(ev);

	/* by default, we put new events into the middle priority */
	if(current_base)
		ev->ev_pri = current_base->nactivequeues/2;
}

int
event_base_set(struct event_base *base, struct event *ev)
{
	/* Only innocent events may be assigned to a different base */
	if (ev->ev_flags != EVLIST_INIT)
		return (-1);

	ev->ev_base = base;
	ev->ev_pri = base->nactivequeues/2;

	return (0);
}

/*
 * Set's the priority of an event - if an event is already scheduled
 * changing the priority is going to fail.
 */

int
event_priority_set(struct event *ev, int pri)
{
	if (ev->ev_flags & EVLIST_ACTIVE)
		return (-1);
	if (pri < 0 || pri >= ev->ev_base->nactivequeues)
		return (-1);

	ev->ev_pri = pri;

	return (0);
}

/*
 * Checks if a specific event is pending or scheduled.
 */

int
event_pending(struct event *ev, short event, struct timeval *tv)
{
	struct timeval	now, res;
	int flags = 0;

	if (ev->ev_flags & EVLIST_INSERTED)
		flags |= (ev->ev_events & (EV_READ|EV_WRITE|EV_SIGNAL));
	if (ev->ev_flags & EVLIST_ACTIVE)
		flags |= ev->ev_res;
	if (ev->ev_flags & EVLIST_TIMEOUT)
		flags |= EV_TIMEOUT;

	event &= (EV_TIMEOUT|EV_READ|EV_WRITE|EV_SIGNAL);

	/* See if there is a timeout that we should report */
	if (tv != NULL && (flags & event & EV_TIMEOUT)) {
		gettime(ev->ev_base, &now);
		evutil_timersub(&ev->ev_timeout, &now, &res);
		/* correctly remap to real time */
		evutil_gettimeofday(&now, NULL);
		evutil_timeradd(&now, &res, tv);
	}

	return (flags & event);
}

int
event_add(struct event *ev, const struct timeval *tv)
{
	struct event_base *base = ev->ev_base;
	const struct eventop *evsel = base->evsel;
	void *evbase = base->evbase;
	int res = 0;

	event_debug((
		 "event_add: event: %p, %s%s%scall %p",
		 ev,
		 ev->ev_events & EV_READ ? "EV_READ " : " ",
		 ev->ev_events & EV_WRITE ? "EV_WRITE " : " ",
		 tv ? "EV_TIMEOUT " : " ",
		 ev->ev_callback));

	assert(!(ev->ev_flags & ~EVLIST_ALL));

	/*
	 * prepare for timeout insertion further below, if we get a
	 * failure on any step, we should not change any state.
	 */
	if (tv != NULL && !(ev->ev_flags & EVLIST_TIMEOUT)) {
		if (min_heap_reserve(&base->timeheap,
			1 + min_heap_size(&base->timeheap)) == -1)
			return (-1);  /* ENOMEM == errno */
	}

	if ((ev->ev_events & (EV_READ|EV_WRITE|EV_SIGNAL)) &&
	    !(ev->ev_flags & (EVLIST_INSERTED|EVLIST_ACTIVE))) {
		res = evsel->add(evbase, ev);
		if (res != -1)
			event_queue_insert(base, ev, EVLIST_INSERTED);
	}

	/* 
	 * we should change the timout state only if the previous event
	 * addition succeeded.
	 */
	if (res != -1 && tv != NULL) {
		struct timeval now;

		/* 
		 * we already reserved memory above for the case where we
		 * are not replacing an exisiting timeout.
		 */
		if (ev->ev_flags & EVLIST_TIMEOUT)
			event_queue_remove(base, ev, EVLIST_TIMEOUT);

		/* Check if it is active due to a timeout.  Rescheduling
		 * this timeout before the callback can be executed
		 * removes it from the active list. */
		if ((ev->ev_flags & EVLIST_ACTIVE) &&
		    (ev->ev_res & EV_TIMEOUT)) {
			/* See if we are just active executing this
			 * event in a loop
			 */
			if (ev->ev_ncalls && ev->ev_pncalls) {
				/* Abort loop */
				*ev->ev_pncalls = 0;
			}
			
			event_queue_remove(base, ev, EVLIST_ACTIVE);
		}

		gettime(base, &now);
		evutil_timeradd(&now, tv, &ev->ev_timeout);

		event_debug((
			 "event_add: timeout in %ld seconds, call %p",
			 tv->tv_sec, ev->ev_callback));

		event_queue_insert(base, ev, EVLIST_TIMEOUT);
	}

#if OPAL_ENABLE_PROGRESS_THREADS
	if(opal_using_threads() && opal_event_pipe_signalled == 0) {
            unsigned char byte = 0;
            if(write(opal_event_pipe[1], &byte, 1) != 1)
                opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
            opal_event_pipe_signalled++;
	}
#endif
	return (res);
}

int
event_del(struct event *ev)
{
        int rc = 0;
	struct event_base *base;
	const struct eventop *evsel;
	void *evbase;

	event_debug(("event_del: %p, callback %p",
		 ev, ev->ev_callback));

	/* An event without a base has not been added */
	if (ev->ev_base == NULL)
		return (-1);

	base = ev->ev_base;
	evsel = base->evsel;
	evbase = base->evbase;

	assert(!(ev->ev_flags & ~EVLIST_ALL));

	/* See if we are just active executing this event in a loop */
	if (ev->ev_ncalls && ev->ev_pncalls) {
		/* Abort loop */
		*ev->ev_pncalls = 0;
	}

	if (ev->ev_flags & EVLIST_TIMEOUT)
		event_queue_remove(base, ev, EVLIST_TIMEOUT);

	if (ev->ev_flags & EVLIST_ACTIVE)
		event_queue_remove(base, ev, EVLIST_ACTIVE);

	if (ev->ev_flags & EVLIST_INSERTED) {
		event_queue_remove(base, ev, EVLIST_INSERTED);
		rc = (evsel->del(evbase, ev));
	}
#if OPAL_ENABLE_PROGRESS_THREADS
        if(opal_using_threads() && opal_event_pipe_signalled == 0) {
            unsigned char byte = 0;
            if(write(opal_event_pipe[1], &byte, 1) != 1)
                opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
            opal_event_pipe_signalled++;
        }
#endif

    return (rc);
}

void
event_active(struct event *ev, int res, short ncalls)
{
	/* We get different kinds of events, add them together */
	if (ev->ev_flags & EVLIST_ACTIVE) {
		ev->ev_res |= res;
		return;
	}

	ev->ev_res = res;
	ev->ev_ncalls = ncalls;
	ev->ev_pncalls = NULL;
	event_queue_insert(ev->ev_base, ev, EVLIST_ACTIVE);
}

static int
timeout_next(struct event_base *base, struct timeval **tv_p)
{
	struct timeval now = OPAL_TIMEOUT_DEFAULT;
	struct event *ev;
	struct timeval *tv = *tv_p;

	if ((ev = min_heap_top(&base->timeheap)) == NULL) {
		/* if no time-based events are active wait for I/O */
		*tv = now;
		return (0);
	}

	if (gettime(base, &now) == -1)
		return (-1);

	if (evutil_timercmp(&ev->ev_timeout, &now, <=)) {
		evutil_timerclear(tv);
		return (0);
	}

	evutil_timersub(&ev->ev_timeout, &now, tv);

	assert(tv->tv_sec >= 0);
	assert(tv->tv_usec >= 0);

	event_debug(("timeout_next: in %ld seconds", tv->tv_sec));
	return (0);
}

#if 0
/* Let's not delete this yet, but it doesn't look necessary right now */
/*
 * Determines if the time is running backwards by comparing the current
 * time against the last time we checked.  Not needed when using clock
 * monotonic.
 */

static void
timeout_correct(struct event_base *base, struct timeval *tv)
{
	struct event **pev;
	unsigned int size;
	struct timeval off;

	if (use_monotonic)
		return;

	/* Check if time is running backwards */
	gettime(base, tv);
	if (evutil_timercmp(tv, &base->event_tv, >=)) {
		base->event_tv = *tv;
		return;
	}

	event_debug(("%s: time is running backwards, corrected",
		    __func__));
	evutil_timersub(&base->event_tv, tv, &off);

	/*
	 * We can modify the key element of the node without destroying
	 * the key, beause we apply it to all in the right order.
	 */
	pev = base->timeheap.p;
	size = base->timeheap.n;
	for (; size-- > 0; ++pev) {
		struct timeval *ev_tv = &(**pev).ev_timeout;
		evutil_timersub(ev_tv, &off, ev_tv);
	}
	/* Now remember what the new time turned out to be. */
	base->event_tv = *tv;
}
#endif

void
timeout_process(struct event_base *base)
{
	struct timeval now;
	struct event *ev;

	if (min_heap_empty(&base->timeheap))
		return;

	gettime(base, &now);

	while ((ev = min_heap_top(&base->timeheap))) {
		if (evutil_timercmp(&ev->ev_timeout, &now, >))
			break;

		/* delete this event from the I/O queues */
		event_del(ev);

		event_debug(("timeout_process: call %p",
			 ev->ev_callback));
		event_active(ev, EV_TIMEOUT, 1);
	}
}

void
event_queue_remove(struct event_base *base, struct event *ev, int queue)
{
	if (!(ev->ev_flags & queue))
		event_errx(1, "%s: %p(fd %d) not on queue %x", __func__,
			   (void*)ev, ev->ev_fd, queue);

	if (~ev->ev_flags & EVLIST_INTERNAL)
		base->event_count--;

	ev->ev_flags &= ~queue;
	switch (queue) {
	case EVLIST_INSERTED:
		TAILQ_REMOVE(&base->eventqueue, ev, ev_next);
		break;
	case EVLIST_ACTIVE:
		base->event_count_active--;
		TAILQ_REMOVE(base->activequeues[ev->ev_pri],
		    ev, ev_active_next);
		break;
	case EVLIST_TIMEOUT:
		min_heap_erase(&base->timeheap, ev);
		break;
	default:
		event_errx(1, "%s: unknown queue %x", __func__, queue);
	}
}

void
event_queue_insert(struct event_base *base, struct event *ev, int queue)
{
	if (ev->ev_flags & queue) {
		/* Double insertion is possible for active events */
		if (queue & EVLIST_ACTIVE)
			return;

		event_errx(1, "%s: %p(fd %d) already on queue %x", __func__,
			   (void*)ev, ev->ev_fd, queue);
	}

	if (~ev->ev_flags & EVLIST_INTERNAL)
		base->event_count++;

	ev->ev_flags |= queue;
	switch (queue) {
	case EVLIST_INSERTED:
		TAILQ_INSERT_TAIL(&base->eventqueue, ev, ev_next);
		break;
	case EVLIST_ACTIVE:
		base->event_count_active++;
		TAILQ_INSERT_TAIL(base->activequeues[ev->ev_pri],
		    ev,ev_active_next);
		break;
	case EVLIST_TIMEOUT: {
		min_heap_push(&base->timeheap, ev);
		break;
	}
	default:
		event_errx(1, "%s: unknown queue %x", __func__, queue);
	}
}

/* Functions for debugging */

const char *
event_get_version(void)
{
	return ("OpenMPI");
}

/* 
 * No thread-safe interface needed - the information should be the same
 * for all threads.
 */

const char *
event_get_method(void)
{
	return (current_base->evsel->name);
}
