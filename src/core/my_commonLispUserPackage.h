#ifndef _commonLispUser_H
#define _commonLispUser_H

#include "foundation.h"
#include "object.h"
#include "core/commonLispUserPackage.fwd.h"

namespace cluser
{

    extern core::Package_sp globalCommonLispUserPkg;

    void initialize_commonLispUserPackage();


};


#endif
