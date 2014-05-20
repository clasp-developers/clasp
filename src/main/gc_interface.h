#ifndef GC_INTERFACE_H
#define GC_INTERFACE_H


namespace gctools {

#ifndef RUNNING_GC_BUILDER // when running the static analyzer - don't include the following
#ifdef USE_MPS
#define GC_KIND_SELECTORS
#include GARBAGE_COLLECTION_INCLUDE
#undef GC_KIND_SELECTORS
#endif
#endif // ifndef RUNNING_GC_BUILDER


};
#endif
