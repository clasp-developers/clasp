#ifndef GC_INTERFACE_H
#define GC_INTERFACE_H
#include "core/foundation.h"

//
// All class forward declarations
//
namespace core {
    class T_O;
    class WrappedPointer_O;
    class Functoid;
    class Creator;
    class Iterator_O;
};
namespace clbind {
    class ConstructorCreator;
};

#ifndef RUNNING_GC_BUILDER // when running the static analyzer - don't include the following
#ifdef USE_MPS
#define DECLARE_FORWARDS
#include GARBAGE_COLLECTION_INCLUDE
#undef DECLARE_FORWARDS
#endif
#endif // ifndef RUNNING_GC_BUILDER




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
