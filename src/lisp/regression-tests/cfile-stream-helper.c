/* Linux fixtures for cfile-stream.lisp; used serially in the test process. */
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

static pthread_t reader, writer;
static struct sigaction saved_action;
static sigset_t saved_mask;
static volatile sig_atomic_t signals_seen;
static int sockets[2], prefix_length, writer_error;

static void interrupted(int signo) {
  (void)signo;
  ++signals_seen;
}

static void *supply_input(void *unused) {
  (void)unused;
  const char bytes[] = "abcdef";
  if (prefix_length && send(sockets[1], bytes, prefix_length, MSG_NOSIGNAL) != prefix_length)
    writer_error = 1;
  /* Give the reader repeated opportunities to block in fread. Always send
     the remainder and close, even if the implementation fails to retry. */
  for (int i = 0; i < 10; ++i) {
    struct timespec delay = {0, 20000000};
    while (nanosleep(&delay, &delay) < 0 && errno == EINTR) {}
    if (pthread_kill(reader, SIGWINCH)) writer_error = 1;
  }
  if (send(sockets[1], bytes + prefix_length, 6 - prefix_length, MSG_NOSIGNAL)
      != 6 - prefix_length)
    writer_error = 1;
  close(sockets[1]);
  return NULL;
}

/* Called and finished on the Lisp reader thread. Temporarily omit SA_RESTART
   so a signal interrupts fread; restore the disposition and thread mask. */
int clasp_test_interrupted_input(int prefix) {
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets)) return -1;
  struct sigaction action = {0};
  action.sa_handler = interrupted;
  sigemptyset(&action.sa_mask);
  if (sigaction(SIGWINCH, &action, &saved_action)) goto fail;
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGWINCH);
  if (pthread_sigmask(SIG_UNBLOCK, &mask, &saved_mask)) goto restore_action;
  reader = pthread_self();
  prefix_length = prefix;
  signals_seen = 0;
  writer_error = 0;
  if (pthread_create(&writer, NULL, supply_input, NULL)) {
    pthread_sigmask(SIG_SETMASK, &saved_mask, NULL);
    goto restore_action;
  }
  return sockets[0]; /* Caller owns this descriptor. */
restore_action:
  sigaction(SIGWINCH, &saved_action, NULL);
fail:
  close(sockets[0]);
  close(sockets[1]);
  return -1;
}

int clasp_test_finish_interrupted_input(void) {
  int failed = pthread_join(writer, NULL);
  int count = signals_seen;
  sigaction(SIGWINCH, &saved_action, NULL);
  pthread_sigmask(SIG_SETMASK, &saved_mask, NULL);
  return failed || writer_error ? -1 : count;
}

int clasp_test_output_pair(void) {
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets)) return -1;
  return sockets[0];
}

int clasp_test_close_peer(void) { return close(sockets[1]); }

int clasp_test_peer_byte(void) {
  unsigned char byte;
  ssize_t count = recv(sockets[1], &byte, 1, MSG_DONTWAIT);
  if (count == 1) return byte;
  if (count == 0) return -1;
  return errno == EAGAIN || errno == EWOULDBLOCK ? -2 : -3;
}
