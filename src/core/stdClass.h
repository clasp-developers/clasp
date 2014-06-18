#ifndef	core_StdClass_H //[
#define core_StdClass_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "metaClass.h"
#include "environment.h"

namespace core {

// Set up this class differently

    SMART(StdClass);
    class StdClass_O : public Class_O
    {
        LISP_META_CLASS(StandardClass);
        LISP_BASE1(Class_O);
        LISP_CLASS(core,CorePkg,StdClass_O,"stdClass");
    public:

	StdClass_O( const StdClass_O& ss ); //!< Copy constructor

	explicit StdClass_O();
	virtual ~StdClass_O();
    };
};

template<> struct gctools::GCInfo<core::StdClass_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = false;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::StdClass_O);
#endif //]
