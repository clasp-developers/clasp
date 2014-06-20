/*
 * Compile with:
 * cc -I/usr/local/include -o time-test time-test.c -L/usr/local/lib -levent
 */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#ifndef WIN32
#ifdef HAVE_SYS_QUEUE_H
#include <sys/queue.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#else
#include <time.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <opal/event/event.h>

int lasttime;

void
timeout_cb(int fd, short event, void *arg)
{
	struct timeval tv;
	struct opal_event *timeout = arg;
	int newtime = time(NULL);

	printf("%s: called at %d: %d\n", __func__, newtime,
	    newtime - lasttime);
	lasttime = newtime;

	timerclear(&tv);
	tv.tv_sec = 2;
	opal_event_add(timeout, &tv);
}

int
main (int argc, char **argv)
{
	struct opal_event timeout;
	struct timeval tv;
 
	/* Initalize the event library */
	opal_event_init();

	/* Initalize one event */
	opal_evtimer_set(&timeout, timeout_cb, &timeout);

	timerclear(&tv);
	tv.tv_sec = 2;
	opal_event_add(&timeout, &tv);

	lasttime = time(NULL);
	
	opal_event_dispatch();

	return (0);
}

