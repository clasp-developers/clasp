#ifndef	core_Metaobject_H //[
#define core_Metaobject_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
//#include "model.h"
#include "executables.fwd.h"
#include "lisp.h"

#include "standardObject.h"
#include "environment.h"

namespace core {

// Set up this class differently

SMART(Metaobject);
class Metaobject_O : public StandardObject_O
{
    LISP_META_CLASS(StandardClass);
    LISP_BASE1(StandardObject_O);
    LISP_CLASS(core,CorePkg,Metaobject_O,"metaobject");
public:

	explicit Metaobject_O();
	virtual ~Metaobject_O();
};


};
TRANSLATE(core::Metaobject_O);
#endif //]
