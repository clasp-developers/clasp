/*
 * Allegro CL dependent C helper routines for CLX
 */

/*
 * This code requires select and interval timers.
 * This means you probably need BSD, or a version
 * of Unix with select and interval timers added.
 */

#include <sys/types.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <stdio.h>

#define ERROR -1
#define INTERRUPT -2
#define TIMEOUT 0
#define SUCCESS 1

#ifdef FD_SETSIZE
#define NUMBER_OF_FDS FD_SETSIZE	/* Highest possible file descriptor */
#else
#define NUMBER_OF_FDS 32
#endif

/* Length of array needed to hold all file descriptor bits */
#define CHECKLEN ((NUMBER_OF_FDS+8*sizeof(int)-1) / (8 * sizeof(int)))

extern int errno;

/*
 * This function waits for input to become available on 'fd'.  If timeout is
 * 0, wait forever.  Otherwise wait 'timeout' seconds.  If input becomes
 * available before the timer expires, return SUCCESS.  If the timer expires
 * return TIMEOUT.  If an error occurs, return ERROR.  If an interrupt occurs
 * while waiting, return INTERRUPT.
 */
int fd_wait_for_input(fd, timeout)
    register int fd;
    register int timeout;
{
    struct timeval timer;
    register int i;
    int checkfds[CHECKLEN];

    if (fd < 0 || fd >= NUMBER_OF_FDS) {
	fprintf(stderr, "Bad file descriptor argument: %d to fd_wait_for_input\n", fd);
	fflush(stderr);
    }

    for (i = 0; i < CHECKLEN; i++)
      checkfds[i] = 0;
    checkfds[fd / (8 * sizeof(int))] |= 1 << (fd % (8 * sizeof(int)));

    if (timeout) {
	timer.tv_sec = timeout;
	timer.tv_usec = 0;
	i = select(32, checkfds, (int *)0, (int *)0, &timer);
    } else
      i = select(32, checkfds, (int *)0, (int *)0, (struct timeval *)0);

    if (i < 0)
      /* error condition */
      if (errno == EINTR)
	return (INTERRUPT);
      else
	return (ERROR);
    else if (i == 0)
      return (TIMEOUT);
    else
      return (SUCCESS);
}
