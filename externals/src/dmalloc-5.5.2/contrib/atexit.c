/*
 *
 * Copyright (c) 1994, Textil Computer Design GmbH, Dresden
 *
 * Author: J"org Wunsch
 *
 * Dumb atexit() implementation. It is far from being elegant. It is
 * only here to provide a workaround for systems where the existing
 * atexit() implementation is known to cause problems due to doing
 * own mallocs. The problem arose originally on a Data General
 * machine running DG/UX 5.4R*, along with gcc compiling C++ code.
 * In order to have global and static variables called their const-
 * ructors, a chunk of code has been placed by the compiler that
 * ran before invoking main(). This code registered the desctructors
 * with atexit() at this very early stage, but the existing atexit()
 * bypassed the normal memory allocation scheme, and hence caused
 * grievous troubles in combination with the dmalloc library.
 *
 * Known problem for DG/UX: the crt0.o (at least in a COFF environ-
 * ment) passes the return value from main() to _real_exit() instead
 * of exit(). Hence programs which return from main instead of calling
 * exit() do not work as expected. I do not see any good workaround
 * for this so far (since crt0.o always happens to reference the
 * _real_exit() from the library, even if we would provide our very
 * own symbol for it).
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose and without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of TCD GmbH not be used in advertising or publicity pertaining to
 * distribution of the document or software without specific, written prior
 * permission.
 *
 * Textil Computer Design GmbH makes no representations about the
 * suitability of the software described herein for any purpose.  It
 * is provided "as is" without express or implied warranty.
 */


/* ANSI C requires at least 32 atexit functions to be allowed */
#define MAXATEXIT 32

static void (*__atexit[MAXATEXIT])(void);

static int __atexit_registered = 0;

int
atexit(void (*fn)(void)) {
  if(__atexit_registered == MAXATEXIT) return -1;
  
  __atexit[__atexit_registered++] = fn;
  return 0;
}


void
exit(int status) {
  static int already_in_atexit = 0;
  extern void _exit(int);

  if(already_in_atexit++)
    /* some silly atexit()-registered stuff called exit(); give up */
    _exit(status);
  
  while(__atexit_registered)
    (*__atexit[--__atexit_registered])();

  _exit(status);
}
