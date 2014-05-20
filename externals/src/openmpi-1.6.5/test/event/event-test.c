/*
 * Compile with:
 * cc -I/usr/local/include -o event-test event-test.c -L/usr/local/lib -levent
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
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#else
#include <windows.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include <opal/event/event.h>

static void
fifo_read(int fd, short event, void *arg)
{
	char buf[255];
	int len;
	struct opal_event *ev = arg;
#ifdef WIN32
	DWORD dwBytesRead;
#endif

	/* Reschedule this event */
	opal_event_add(ev, NULL);

	fprintf(stderr, "fifo_read called with fd: %d, event: %d, arg: %p\n",
		fd, event, arg);
#ifdef WIN32
	len = ReadFile((HANDLE)fd, buf, sizeof(buf) - 1, &dwBytesRead, NULL);

	/* Check for end of file. */ 
	if(len && dwBytesRead == 0) {
		fprintf(stderr, "End Of File");
		opal_event_del(ev);
		return;
	}

	buf[dwBytesRead + 1] = '\0';
#else
	len = read(fd, buf, sizeof(buf) - 1);

	if (len == -1) {
		perror("read");
		return;
	} else if (len == 0) {
		fprintf(stderr, "Connection closed\n");
		return;
	}

	buf[len] = '\0';
#endif
	fprintf(stdout, "Read: %s\n", buf);
}

int
main (int argc, char **argv)
{
	struct opal_event evfifo;
#ifdef WIN32
	HANDLE socket;
	// Open a file. 
	socket = CreateFile("test.txt",     // open File 
			GENERIC_READ,                 // open for reading 
			0,                            // do not share 
			NULL,                         // no security 
			OPEN_EXISTING,                // existing file only 
			FILE_ATTRIBUTE_NORMAL,        // normal file 
			NULL);                        // no attr. template 

	if(socket == INVALID_HANDLE_VALUE)
		return 1;

#else
	struct stat st;
	char *fifo = "event.fifo";
	int socket;
 
	if (lstat (fifo, &st) == 0) {
		if ((st.st_mode & S_IFMT) == S_IFREG) {
			errno = EEXIST;
			perror("lstat");
			exit (1);
		}
	}

	unlink (fifo);
	if (mkfifo (fifo, 0600) == -1) {
		perror("mkfifo");
		exit (1);
	}

	/* Linux pipes are broken, we need O_RDWR instead of O_RDONLY */
#ifdef __linux
	socket = open (fifo, O_RDWR | O_NONBLOCK, 0);
#else
	socket = open (fifo, O_RDONLY | O_NONBLOCK, 0);
#endif

	if (socket == -1) {
		perror("open");
		exit (1);
	}

	fprintf(stderr, "Write data to %s\n", fifo);
#endif
	/* Initalize the event library */
	opal_event_init();

	/* Initalize one event */
#ifdef WIN32
	opal_event_set(&evfifo, (int)socket, OPAL_EV_READ, fifo_read, &evfifo);
#else
	opal_event_set(&evfifo, socket, OPAL_EV_READ, fifo_read, &evfifo);
#endif

	/* Add it to the active events, without a timeout */
	opal_event_add(&evfifo, NULL);
	
	opal_event_dispatch();
#ifdef WIN32
	CloseHandle(socket);
#endif
	return (0);
}

