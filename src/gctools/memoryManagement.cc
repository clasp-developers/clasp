//

#include "core/foundation.h"
#include "core/object.h"
#include "core/numbers.h"
#include "memoryManagement.h"
//#include "main/allHeaders.cc"



namespace gctools {



    HeapRoot* 	rooted_HeapRoots = NULL;
    StackRoot* 	rooted_StackRoots = NULL;



};

#ifdef USE_REFCOUNT
#include "intrusiveRefCountGarbageCollection.cc"
#endif

#ifdef USE_BOEHM
#include "boehmGarbageCollection.cc"
#endif

#if defined(USE_MPS)
#include "mpsGarbageCollection.cc"
#endif
