/*
    File: clbindLlvmExpose.cc
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
#include <clasp/core/foundation.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/IR/DataLayout.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/clbind/clbind.h>

#ifdef USE_MPS
// What the hell is this?   Which name means something???
#define NAMESPACE_clbind_llvm
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind_clang
#endif

#if 0
typedef clbind::Wrapper<llvm::APInt, std::unique_ptr<llvm::APInt>> APInt_wrapper;
typedef clbind::Wrapper<llvm::APSInt, std::unique_ptr<llvm::APSInt>> APSInt_wrapper;
#endif

namespace llvmo {

using namespace clbind;

std::string do_apint_tostring(const llvm::APInt& ai, unsigned Radix, bool Signed) {
  llvm::SmallString<256> istr;
  ai.toString(istr, Radix, Signed);
  return istr.str().str();
}

void initialize_clbind_llvm_expose() {
  clbind::package_ pkg("LLVM");
  clbind::scope_& m = pkg.scope();
  class_<llvm::APInt>(m, "APInt");
#if 0
  // Option 1 FAIL
  m.def("APInt_toString", +[](const llvm::APInt& ai, unsigned Radix, bool Signed)
    /*__attribute__((noinline))*/
        __attribute__((shared))
  {
        llvm::SmallString<256> istr;
        ai.toString( istr, Radix, Signed );
        return istr.str().str();
  } );
#elif 2
  // Option 2 WORKS
  m.def("APInt_toString", do_apint_tostring);
#elif 0
  // Option 3 FAIL
  auto do_thing = +[](const llvm::APInt& ai, unsigned Radix, bool Signed) __attribute__((noinline)) {
    llvm::SmallString<256> istr;
    ai.toString(istr, Radix, Signed);
    return istr.str().str();
  };
  m.def("APInt_toString", do_thing);
#elif 0
  // Option 4 FAIL
  struct ToString {
    static std::string doit(const llvm::APInt& ai, unsigned Radix, bool Signed) __attribute__((noinline)) {
      llvm::SmallString<256> istr;
      ai.toString(istr, Radix, Signed);
      return istr.str().str();
    };
  };
  m.def("APInt_toString", ToString::doit);
#elif 1
  // Option 5
  externLambda = +[](const llvm::APInt& ai, unsigned Radix, bool Signed) __attribute__((noinline)) {
    llvm::SmallString<256> istr;
    ai.toString(istr, Radix, Signed);
    return istr.str().str();
  };
  m.def("APInt_toString", externLambda);
#endif

  class_<llvm::APSInt, llvm::APInt>(m, "APSInt");
}
} // namespace llvmo
