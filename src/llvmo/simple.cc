#define	DEBUG_LEVEL_FULL

#include <string>
#include "core/foundation.h"
#include "core/bundle.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/archiveNode.h"
#include "core/candoOpenMp.h"
#include "core/cons.h"



int main(int argc, char* argv[] )
{	// Do not touch debug log until after MPI init
    core::Fixnum_sp fn = core::Fixnum_O::create(1234);
    printf("in main: %s\n", fn->__repr__().c_str());
}
