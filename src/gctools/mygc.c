// Don't do static analysis on the garbage collector code
#ifndef RUNNING_GC_BUILDER
#include <config.h>
#include <clasp/gctools/configure_memory.h>

#ifdef USE_BOEHM
// Configure Boehm GC
#include "clasp/gctools/boehm_config.h"
#include "extra/gc.c"
#endif

#endif // #ifndef RUNNING_GC_BUILDER
