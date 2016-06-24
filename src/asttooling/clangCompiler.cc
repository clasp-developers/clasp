/*
    File: clangCompiler.cc
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

//#include <clang/driver/Driver.h>
//#include <clang/Frontend/TextDiagnosticPrinter.h>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/translators.h>
#include <clasp/core/str.h>
#include <clasp/core/arguments.h>
#include <clasp/clbind/clbind.h>
#include <clasp/llvmo/translators.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/translators.h>
#include <clasp/asttooling/symbolTable.h>
#include <clasp/core/wrappers.h>
#include <clasp/asttooling/Diagnostics.h>
#include <clasp/asttooling/Registry.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/asttooling/clangCompilePkg.h>
#include <clasp/core/translators.h>

#ifdef USE_MPS
#define NAMESPACE_clbind_clang
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind_clang
#endif


namespace clang_compile {
using namespace clbind;

class Foo {
public:
  std::string _message;
  void setMessage(const std::string& m) { this->_message = m; };
  std::string message() { return this->_message; };
  Foo() : _message("Hi there") {};
  virtual ~Foo() {printf("%s:%d - destructing Foo\n", __FILE__, __LINE__ ); }
};
};
#if 0
namespace clang_compile {
class DerivableFoo : public clbind::Derivable<Foo> {
public:
  typedef Foo Base;
  static gctools::smart_ptr<DerivableFoo> CreateFoo() { GC_ALLOCATE(DerivableFoo,f); return f;};
  
};
};
DERIVABLE_TRANSLATE(clang_compile::Foo);
#endif

namespace clang_compile {
void initialize_clang_compile() {
  printf("%s:%d  initialize_clang_compile\n", __FILE__, __LINE__ );
  // overloaded functions that had trouble resolving
  package(ClangCompilePkg, {"CLANG-COMPILER"}, {"CL", "CORE"})
    [
    /* -- */                                                               
     class_<clang::DiagnosticOptions>("DiagnosticOptions") //, no_default_constructor)
     ,
     class_<clang::DiagnosticIDs>("DiagnosticIds")
//     ,
//     derivable_class_<DerivableFoo>("Foo",no_default_constructor)
//     ,def("CreateFoo",&DerivableFoo::CreateFoo,policies<adopt<result>>())
#if 0
     ,
     class_<clang::driver::Driver>("Driver")
     ,
     class_<clang::TextDiagnosticPrinter>("TextDiagnosticPrinter")
#endif
  ];
}
};
