#ifndef CLASP_CXX_INTEROP_H
#define CLASP_CXX_INTEROP_H

#include <clasp/core/foundation.h>
#include <clasp/clbind/clbind.h>
#include <clasp/core/lambdaListHandler.h>


// ------------------------------------------------------------
//
// Set up macros for interop
//
typedef void (*fnStartUp)();
extern "C" void cc_register_startup_function(fnStartUp fn);

struct clasp_register_startup {
  clasp_register_startup(fnStartUp startup_function) {
    cc_register_startup_function(startup_function);
  }
};

#define CLASP_REGISTER_STARTUP(fn) static clasp_register_startup dummy(fn);;

#endif
