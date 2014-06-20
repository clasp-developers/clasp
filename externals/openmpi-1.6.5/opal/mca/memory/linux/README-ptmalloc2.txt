ptmalloc2 - a multi-thread malloc implementation
================================================

Wolfram Gloger (wg@malloc.de)

Nov 2004


Introduction
============

This package is a modified version of Doug Lea's malloc-2.7.1pre
implementation (available seperately from ftp://g.oswego.edu/pub/misc)
that I adapted for multiple threads, while trying to avoid lock
contention as much as possible.  Many thanks should go to Doug Lea
(dl@cs.oswego.edu) for the great original malloc implementation.

As part of the GNU C library, the source files are available under the
GNU Library General Public License (see the comments in the files).
But as part of this stand-alone package, the code is also available
under the (probably less restrictive) conditions described in the file
'COPYRIGHT'.  In any case, there is no warranty whatsoever for this
package.

The current distribution should be available from:

http://www.malloc.de/malloc/ptmalloc2.tar.gz


Compilation
===========

It should be possible to build ptmalloc2 on any UN*X-like system that
implements the sbrk(), mmap(), munmap() and mprotect() calls.  If
mmap() is not available, it is only possible to produce a
non-threadsafe implementation.  Since there are now several source
files, a library (libmalloc.a) is generated.  See the Makefile for
examples of the compile-time options.

Note that support for non-ANSI compilers is no longer a significant
goal.

Several example targets are provided in the Makefile:

 o Posix threads (pthreads), compile with "make posix"

 o Posix threads with explicit initialization, compile with
   "make posix-explicit" (known to be required on HPUX)

 o Posix threads without "tsd data hack" (see below), compile with
   "make posix-with-tsd"

 o Solaris threads, compile with "make solaris"

 o SGI sproc() threads, compile with "make sproc"

 o no threads, compile with "make nothreads"

For Linux:

 o make "linux-pthread" (almost the same as "make posix")

Note that some compilers need special flags for multi-threaded code,
e.g. with Solaris cc with Posix threads, one should use:

% make posix SYS_FLAGS='-mt'

Some additional targets, ending in `-libc', are also provided in the
Makefile, to compare performance of the test programs to the case when
linking with the standard malloc implementation in libc.

A potential problem remains: If any of the system-specific functions
for getting/setting thread-specific data or for locking a mutex call
one of the malloc-related functions internally, the implementation
cannot work at all due to infinite recursion.  One example seems to be
Solaris 2.4.  I would like to hear if this problem occurs on other
systems, and whether similar workarounds could be applied.

For Posix threads, too, an optional hack like that has been integrated
(activated when defining USE_TSD_DATA_HACK) which depends on
`pthread_t' being convertible to an integral type (which is of course
not generally guaranteed).  USE_TSD_DATA_HACK is now the default
because I haven't yet found a non-glibc pthreads system where this
hack is _not_ needed.

*NEW* and _important_: In (currently) one place in the ptmalloc2
source, a write memory barrier is needed, named
atomic_write_barrier().  This macro needs to be defined at the end of
malloc-machine.h.  For gcc, a fallback in the form of a full memory
barrier is already defined, but you may need to add another definition
if you don't use gcc.

Usage
=====

Just link libmalloc.a into your application.

Some wicked systems (e.g. HPUX apparently) won't let malloc call _any_
thread-related functions before main().  On these systems,
USE_STARTER=2 must be defined during compilation (see "make
posix-explicit" above) and the global initialization function
ptmalloc_init() must be called explitly, preferably at the start of
main().

Otherwise, when using ptmalloc2, no special precautions are necessary.

Link order is important
=======================

On some systems, when overriding malloc and linking against shared
libraries, the link order becomes very important.  E.g., when linking
C++ programs on Solaris, don't rely on libC being included by default,
but instead put `-lthread' behind `-lC' on the command line:

  CC ... libmalloc.a -lC -lthread

This is because there are global constructors in libC that need
malloc/ptmalloc, which in turn needs to have the thread library to be
already initialized.

Debugging hooks
===============

All calls to malloc(), realloc(), free() and memalign() are routed
through the global function pointers __malloc_hook, __realloc_hook,
__free_hook and __memalign_hook if they are not NULL (see the malloc.h
header file for declarations of these pointers).  Therefore the malloc
implementation can be changed at runtime, if care is taken not to call
free() or realloc() on pointers obtained with a different
implementation than the one currently in effect.  (The easiest way to
guarantee this is to set up the hooks before any malloc call, e.g.
with a function pointed to by the global variable
__malloc_initialize_hook).

A useful application of the hooks is built-in into ptmalloc2: The
implementation is usually very unforgiving with respect to misuse,
such as free()ing a pointer twice or free()ing a pointer not obtained
with malloc() (these will typically crash the application
immediately).  To debug in such situations, you can set the
environment variable `MALLOC_CHECK_' (note the trailing underscore).
Performance will suffer somewhat, but you will get more controlled
behaviour in the case of misuse.  If MALLOC_CHECK_=0, wrong free()s
will be silently ignored, if MALLOC_CHECK_=1, diagnostics will be
printed on stderr, and if MALLOC_CHECK_=2, abort() will be called on
any error.

You can now also tune other malloc parameters (normally adjused via
mallopt() calls from the application) with environment variables:

    MALLOC_TRIM_THRESHOLD_    for deciding to shrink the heap (in bytes)

    MALLOC_TOP_PAD_           how much extra memory to allocate on
                              each system call (in bytes)

    MALLOC_MMAP_THRESHOLD_    min. size for chunks allocated via
                              mmap() (in bytes)

    MALLOC_MMAP_MAX_          max. number of mmapped regions to use

Tests
=====

Two testing applications, t-test1 and t-test2, are included in this
source distribution.  Both perform pseudo-random sequences of
allocations/frees, and can be given numeric arguments (all arguments
are optional):

% t-test[12] <n-total> <n-parallel> <n-allocs> <size-max> <bins>

    n-total = total number of threads executed (default 10)
    n-parallel = number of threads running in parallel (2)
    n-allocs = number of malloc()'s / free()'s per thread (10000)
    size-max = max. size requested with malloc() in bytes (10000)
    bins = number of bins to maintain

The first test `t-test1' maintains a completely seperate pool of
allocated bins for each thread, and should therefore show full
parallelism.  On the other hand, `t-test2' creates only a single pool
of bins, and each thread randomly allocates/frees any bin.  Some lock
contention is to be expected in this case, as the threads frequently
cross each others arena.

Performance results from t-test1 should be quite repeatable, while the
behaviour of t-test2 depends on scheduling variations.

Conclusion
==========

I'm always interested in performance data and feedback, just send mail
to ptmalloc@malloc.de.

Good luck!
