/*
    File: llvmoExpose_gen.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/package.h>
#include <clasp/core/stringList.h>
#include <clasp/core/environment.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/str.h>
#include <clasp/llvmo/llvmoExpose.h>
//#include "llvmoExpose.generated.h"
#include <clasp/core/wrappers.h>
#include <clasp/core/external_wrappers.h>

namespace llvmo {
LLVMContext_sp LLVMContext_O::get_global_context() {
  GC_RESERVE_TRY(LLVMContext_O, context) {
    GC_RESERVE_GET(LLVMContext_O, context);
    context->_ptr = &(llvm::getGlobalContext());
    llvm::LLVMContext &lc = llvm::getGlobalContext();
    ASSERT(context->_ptr == &lc);
  }
  return context;
};
}

namespace llvmo {
EXPOSE_CLASS(llvmo, LLVMContext_O);

void LLVMContext_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<LLVMContext_O>();
  af_def(LlvmoPkg, "get-global-context", &LLVMContext_O::get_global_context);
};

void LLVMContext_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
}

namespace llvmo {
EXPOSE_CLASS(llvmo, Pass_O);

void Pass_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<Pass_O>();
};

void Pass_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
}

namespace llvmo {
EXPOSE_CLASS(llvmo, FunctionPass_O);

void FunctionPass_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<FunctionPass_O>();
};

void FunctionPass_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
}

namespace llvmo {
EXPOSE_CLASS(llvmo, ModulePass_O);

void ModulePass_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<ModulePass_O>();
};

void ModulePass_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
}

namespace llvmo {
EXPOSE_CLASS(llvmo, ImmutablePass_O);

void ImmutablePass_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<ImmutablePass_O>();
};

void ImmutablePass_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
}

namespace llvmo {
EXPOSE_CLASS(llvmo, PassManagerBase_O);

void PassManagerBase_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<PassManagerBase_O>();
};

void PassManagerBase_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
Value_sp Value_O::create(llvm::Value *ptr) {
  return core::RP_Create_wrapped<Value_O, llvm::Value *>(ptr);
};
}

namespace llvmo {
EXPOSE_CLASS(llvmo, Value_O);

void Value_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<Value_O>()
      .def("dump", &llvm::Value::dump)

      .def("getName", &llvm::Value::getName)

      .def("setName", &llvm::Value::setName)

      .def("getType", &llvm::Value::getType)

      ;
};

void Value_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
namespace llvmo {
}

namespace llvmo {
EXPOSE_CLASS(llvmo, User_O);

void User_O::exposeCando(core::Lisp_sp lisp) {
  core::externalClass_<User_O>();
};

void User_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo
