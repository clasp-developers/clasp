#ifndef CLASP_CXX_INTEROP_H
#define CLASP_CXX_INTEROP_H


#include <clasp/core/core.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/compiler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/clbind/clbind.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/package.h>
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

#define CLASP_REGISTER_NAMED_STARTUP(named_fn,fn) \
  extern "C" void named_fn() { \
    fn(); \
  }



//
// Make more compatible with pybind11
//
namespace clbind {

  typedef core::T_sp object;
  
  typedef core::HashTable_sp dict;
  typedef core::SimpleVector_sp list;
  typedef scope_ module;
  

  core::DoubleFloat_sp float_(double val);
  core::Str8Ns_sp str(const std::string& str);
  core::T_sp bool_(int val);
  core::T_sp int_(int val);
  
template <class To_SP, typename From_SP>
inline bool isinstance(From_SP const& rhs) {
  return gctools::IsA<To_SP>(rhs);
}



};

size_t len(core::T_sp object);

namespace expose = clbind;


namespace clbind {

  template <class TT>
  core::T_sp cast(const TT& obj);

class cast_error {
};
};


#endif
