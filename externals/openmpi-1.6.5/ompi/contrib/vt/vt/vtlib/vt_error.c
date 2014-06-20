/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "vt_error.h"
#include "vt_env.h"
#include "vt_iowrap.h"
#include "vt_trc.h"

#define VT_MSG_PFIX  "VampirTrace"
#define VT_MSG_SIZE  1024

/* exit without performing exit handler (vt_close) on fatal error */
#define DO_EXIT_FAILURE vt_failure = 1; exit(EXIT_FAILURE)

static int vt_pid = -1;

void vt_error_pid(const int pid)
{
  vt_pid = pid;
}

static void vt_print_msg(const char* prefix, const char* fmt, va_list az)
{
  char buffer[VT_MSG_SIZE];

  if (vt_pid != -1) snprintf(buffer, sizeof(buffer)-1,
			     "[%d]", vt_pid); else buffer[0]=0;
  snprintf(buffer + strlen(buffer), sizeof(buffer)-1, "%s: ", VT_MSG_PFIX);

  if (prefix) snprintf(buffer + strlen(buffer), sizeof(buffer)-1,
		       "%s: ", prefix);

  vsnprintf(buffer + strlen(buffer), sizeof(buffer)-1, fmt, az);

  vt_iowrap_externals_init();
  libc_fprintf(stderr, "%s\n", buffer);
  fflush(NULL);
}

void vt_libassert_fail(const char* f, int l, const char* expr)
{
  vt_iowrap_externals_init();
  libc_fprintf(stderr, "%s: FATAL: %s:%d: Assertion `%s' failed\n\nPlease report this incident to " PACKAGE_BUGREPORT "\n",
	       VT_MSG_PFIX, f, l, expr);
  DO_EXIT_FAILURE;
}

void vt_error_impl(const char* f, int l)
{
  char buffer[VT_MSG_SIZE];
  snprintf(buffer, sizeof(buffer)-1, "%s: FATAL: %s:%d", VT_MSG_PFIX, f, l);

  perror(buffer);
  fflush(stderr);
  
  DO_EXIT_FAILURE;
}

void vt_error_msg(const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vt_print_msg("FATAL", fmt, ap);
  va_end(ap);
  
  DO_EXIT_FAILURE;
}

void vt_warning(const char* fmt, ...)
{
  va_list ap;

  if (vt_env_verbose() >= 1) {
    va_start(ap, fmt);
    vt_print_msg("WARNING", fmt, ap);
    va_end(ap);
  }
}

void vt_cntl_msg(int level, const char* fmt, ...)
{
  va_list ap;

  if (vt_env_verbose() >= level) {
    va_start(ap, fmt);
    vt_print_msg(NULL, fmt, ap);
    va_end(ap);
  }
}

