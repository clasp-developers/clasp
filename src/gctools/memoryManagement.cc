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

#ifdef USE_MPS
#include "mpsGarbageCollection.cc"
#else
#include "intrusiveRefCountGarbageCollection.cc"
#endif
