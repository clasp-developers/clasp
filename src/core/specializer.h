#ifndef	core_Specializer_H //[
#define core_Specializer_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
//#include "model.h"
#include "metaobject.h"
#include "environment.h"

namespace core {

// Set up this class differently

    SMART(Specializer);
    class Specializer_O : public Metaobject_O
    {
	LISP_META_CLASS(StandardClass);
	LISP_BASE1(Metaobject_O);
	LISP_CLASS(core,CorePkg,Specializer_O,"specializer");
    public:

	explicit Specializer_O();
	virtual ~Specializer_O();
    };


};
TRANSLATE(core::Specializer_O);
#endif //]
