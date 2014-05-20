#include "foundation.h"
//#include "object.h"
//#include "smart_pointers.h"


namespace mem
{


    void initialize_smart_pointers()
    {
#if 0  // Need -std=c++11 to use the alignof operator and c++11 is broken (May13 2013)
	unsigned long align_gcobject = alignof(GCObject);
	if ( align_gcobject < 4 )
	{
	    printf("Cannot initialize smart-pointers - GCObject alignment is %lu and it must be at least 4\n", align_gcobject );
	    exit(1);
	}
#endif
    }

};
