30 March 2009

This file documents Open MPI's usage of ptmalloc2.  This is perhaps
our 7,208,499th iteration of ptmalloc2 support, so let's document it
here so that some future developer might spend *slightly* less time
understanding what the heck is going on.

See glibc documentation about malloc hooks before continuing.  This is
pretty much required reading before reading the rest of this file /
having a hope of understanding what's going on here:

 http://www.gnu.org/software/libc/manual/html_mono/libc.html#Hooks-for-Malloc

The overall goal is that we're using the Linux glibc hooks to wholly
replace the underlying allocator.  We *used* to use horrid linker
tricks to interpose OMPI's ptmalloc2 symbols with the glibc ones --
meaning that user apps would call our symbols and not the glibc ones.
But that scheme is fraught with problems, not the least of which is
that *all* MPI applications will be forced to use our overridden
allocator (not just the ones that need it, such as the ones running on
OpenFabrics-based networks).  Instead, what we do here is, frankly,
quite similar to what is done in MX: we use the 4 glibc hooks to
assert our own malloc, realloc, free, and memalign functions.  This
allows the decision as to whether to use this internal ptmalloc2
allocate to be a run-time decision.  This is quite important; using
this internal allocator has both benefits (allowing using
mpi_leave_pinned=1 behavior) and drawbacks (breaking some debuggers,
being unnecessary for non-OpenFabrics-based networks, etc.).

Here's how it works...

This component *must* be linked statically as part of libopen-pal; it
*cannot* be a DSO.  Specifically, this library must be present during
pre-main() initialization phases so that its __malloc_initialize_hook
can be found and executed.  Loading it as a DSO during MPI_INIT is far
too late.  In configure.m4, we define the M4 macro
MCA_memory_ptmalloc2_COMPILE_MODE to always compile this component in
static mode.  Yay flexible build system.

This component provides an munmap() function that will intercept calls
to munmap() and do the Right Thing.  That is fairly straightforward to
do.  Intercepting the malloc/free/etc. allocator is much more
complicated.

All the ptmalloc2 public symbols in this component have been name
shifted via the rename.h file.  Hence, what used to be "malloc" is now
opal_memory_ptmalloc2_malloc.  Since all the public symbols are
name-shifted, we can safely link this component in all MPI
applications.  Specifically: just because this ptmalloc2 allocator is
present in all OMPI executables and user-level applications, it won't
necessarily be used -- it's a separate/run-time decision as to whether
it will be used.

We set the __malloc_initialize_hook variable to point to
opal_memory_ptmalloc2_malloc_init_hook (in hooks.c).  This function is
called by the underlying glibc allocator before any allocations occur
and before the memory allocation subsystem is setup.  As such, this
function is *extremely* restricted in what it can do.  It cannot call
any form of malloc, for example (which seems fairly obvious, but it's
worth mentioning :-) ).  This function is one of the determining
steps as to whether we'll use the internal ptmalloc2 allocator or
not.  Several checks are performed:

- Was either the MCA params mpi_leave_pinned or
  mpi_leave_pinned_pipeline set?
- Is a driver found to be active indicating that an OS-bypass network
  is in effect (OpenFabrics, MX, Open-MX, ...etc.)
- Was an environment variable set indicating that we want to disable
  this component?

If the $OMPI_MCA_memory_ptmalloc2_disable or the $FAKEROOTKEY env
variables are set, we don't enable the memory hooks.

We then use the following matrix to determine whether to enable the
memory hooks or not (explanation of the matrix is below):

       lp / lpp   yes   no   runtime   not found       
       yes        yes   yes  yes       yes
       no         yes   no   no        no
       runtime    yes   no   runtime   runtime
       not found  yes   no   runtime   runtime

lp = leave_pinned (the rows), lpp = leave_pinned_pipeline (the columns)
yes = found that variable to be set to "yes" (i.e., 1)
no = found that variable to be set to "no" (i.e., 0)
runtime = found that variable to be set to "determine at runtime" (i.e., -1)
not found = that variable was not set at all

Hence, if we end up on a "yes" block in the matrix, we enable the
hooks.  If we end up in a "no" block in the matrix, we disable the
hooks.  If we end up in a "runtime" block in the matrix, then we
enable the hooks *if* we can find indications that an OS bypass
network is present and available for use (e.g., OpenFabrics, MX,
Open-MX, ...etc.).

To be clear: sometime during process startup, this function will
definitely be called.  It will either set the 4 hook functions to
point to our name-shifted ptmalloc2 functions, or it won't.  If the 4
hook functions are set, then the underlying glibc allocator will
always call our 4 functions in all the relevant places instead of
calling its own functions.  Specifically: the process is calling the
underlying glibc allocator, but that underlying glibc allocator will
make function pointer callbacks to our name-shifted ptmalloc2
functions to actually do the work.

Note that because we know our ptmalloc will not be providing all 5
hook variables (because we want to use the underlying glibc hook
variables), they are #if 0'ed out in our malloc.c.  This has the
direct consequence that the *_hook_ini() in hooks.c are never used.
So to avoid compiler/linker warnings, I #if 0'ed those out as well.

All the public functions in malloc.c that call hook functions were
modified to #if 0 the hook function invocations.  After all, that's
something that we want the *underlying* glibc allocator to do -- but
we are putting these functions as the hooks, so we don't want to
invoke ourselves in an infinite loop!

The next thing that happens in the startup sequence is that the
ptmalloc2 memory component's "open" function is called during
MPI_INIT.  But we need to test to see if the glibc memory hooks have
been overridden before MPI_INIT was invoked.  If so, we need to signal
that our allocator support may not be complete.

Patrick Geoffray/MX suggests a simple test: malloc() 4MB and then free
it.  Watch to see if our name-shifted ptmalloc2 free() function was
invoked.  If it was, then all of our hooks are probably in place and
we can proceed.  If not, then set flags indicating that this memory
allocator only supports MUNMAP (not FREE/CHUNK).

We actually perform this test for malloc, realloc, and memalign.  If
they all pass, then we say that the memory allocator supports
everything.  If any of them fail, then we say that the memory
allocator does not support FREE/CHUNK.

NOTE: we *used* to simply set the FREE/CHUNK support flags during our
ptmalloc2's internal ptmalloc_init() function.  This is not a good
idea becaus even after our ptmalloc_init() function has been invoked,
someone may come in an override our memory hooks.  Doing tests during
the ptmalloc2 memory component's open function seems to be the safest
way to test whether we *actually* support FREE/CHUNK (this is what MX
does, too).

As stated above, we always intercept munmap() -- this is acceptable in
all environments.  But we test that, too, just to be sure that the
munmap intercept is working.  If we verify that it is working
properly, then we set that we have MUNMAP support.

Much later in the init sequence during MPI_INIT, components indicate
whether they want to use mpi_leave_pinned[_pipeline] support or not.
For example, the openib BTL queries the opal_mem_hooks_support_level()
function to see if FREE and MUNMAP are supported.  If they are, then
the openib BTL sets mpi_leave_pinned = 1.

Finally, the mpool base does a final check.  If
mpi_leave_pinned[_pipeline] is set to 1 and/or use_mem_hooks is set,
if FREE/MUNMAP are not set in the supported flags, then a warning is
printed.  Otherwise, life continues (assumedly using
mpi_leave_pinned[_pipeline] support).

Simple, right?

