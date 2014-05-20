

#include "foundation.h"
#include "package.h"
#include "lisp.h"

#include "wxWidgetsExpose.h"

namespace core 
{

//
// Initialize classes with WxWidgets
//
void initializePackagesAndClasses(Lisp_sp lisp)
{

    lisp->makePackage(WxPackage);


#define Use_CorePkg
#define Use_MbbPackage
#define Use_WxPackage
#include "core_initClasses_inc.h"

    lisp->installGlobalInitializationCallback(initializeWxWidgetsConstants);
};





};
