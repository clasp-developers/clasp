#ifndef CLASP_CXX_INTEROP_H
#define CLASP_CXX_INTEROP_H

#include <clasp/core/foundation.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/compiler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/clbind/clbind.h>
#include <clasp/core/lambdaListHandler.h>


// ------------------------------------------------------------
//
// Set up macros for interop
//
typedef void (*voidStartUp)(void);

struct clasp_register_startup {
  clasp_register_startup(voidStartUp startup_function) {
    core::StartUp su(core::StartUp::void_function,0,(void*)startup_function);
    core::register_startup_function(su);
  }
};

#define CLASP_REGISTER_STARTUP(fn) static clasp_register_startup dummy(fn);;

#endif
