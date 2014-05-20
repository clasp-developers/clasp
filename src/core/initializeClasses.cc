

#include "foundation.h"
#include "lisp.h"



namespace core 
{

//
// Initialize classes without WxWidgets
//
void initializePackagesAndClasses(Lisp_sp lisp)
{
    DEPRECIATED();
#define Use_CorePkg
#define Use_MbbPackage
// #define Use_WxPackage
#include "core_initClasses_inc.h"

};

};
