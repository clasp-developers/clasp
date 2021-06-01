/*
    File: stacks.cc
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
#include <execinfo.h>
#include <clasp/core/foundation.h>
#include <clasp/core/stacks.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/primitives.h>
#include <clasp/core/debugger.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

int global_debug_ihs = 0;

namespace core {


#ifdef DEBUG_IHS
// Maintain a shadow stack of function pointers when DEBUG_IHS is turned on
// This will slow the system down, but will provide function names of functions
// on the stack when an IHS problem is 
std::vector<void*>     global_debug_ihs_shadow_stack;
#endif

// TODO Get rid of this definition of global_debug_ihs
int global_debug_ihs = 0;

void InvocationHistoryFrame::validate() const {
#if 0
  T_sp res((gctools::Tagged)(((core::T_O**)(this->_args->reg_save_area))[LCC_CLOSURE_REGISTER]));
  if (res) {
    if (gc::IsA<Function_sp>(res)) {
      return;
    }
    printf("%s:%d  There is a problem in the creation of an InvocationHistoryFrame - bad function: %p\n:", __FILE__, __LINE__, res.raw_());
    printf("        Closure -> %s\n", _rep_(res).c_str());
    abort();
  }
  printf("%s:%d  There is a problem in the creation of an InvocationHistoryFrame - bad function: %p\n:", __FILE__, __LINE__, res.raw_());
  abort();
#endif
}

T_sp InvocationHistoryFrame::function() const
  {
    Function_sp res((gctools::Tagged)(((core::T_O**)(this->_args->reg_save_area))[LCC_CLOSURE_REGISTER]));
    if ( !res ) {
      return _Nil<T_O>();
    }
    return res;
  }

void* InvocationHistoryFrame::register_save_area() const
  {
    return (this->_args->reg_save_area);
  }

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE SimpleVector_sp InvocationHistoryFrame::arguments() const {
  size_t numberOfArguments = this->_remaining_nargs;
  va_list cargs;
  va_copy(cargs,this->_args);
  T_O* objRaw;
  if (numberOfArguments > CALL_ARGUMENTS_LIMIT) {
    va_end(cargs);
    return SimpleVector_O::make(0);
  }
  SimpleVector_sp vargs = SimpleVector_O::make(numberOfArguments);
  for (size_t i(0); i < numberOfArguments; ++i) {
    //objRaw = this->valist_sp().indexed_arg(i);
    objRaw = va_arg(cargs,core::T_O*);
    (*vargs)[i] = T_sp((gc::Tagged)objRaw);
  }
  va_end(cargs);
  return vargs;
}

string InvocationHistoryFrame::argumentsAsString(int maxWidth) const {
  SimpleVector_sp vargs = this->arguments();
  T_sp sout = clasp_make_string_output_stream();
  int nargs = cl__length(vargs);
  for (int i(0); i < nargs; ++i) {
    T_sp obj = vargs->rowMajorAref(i);
    if (gc::IsA<Instance_sp>(obj)) {
      clasp_write_string("#<", sout);
      write_ugly_object(cl__class_of(obj), sout);
      clasp_write_string("> ", sout);
    } else {
      write_ugly_object(obj, sout);
      clasp_write_char(' ', sout);
    }
  }
  String_sp strres = gc::As<String_sp>(cl__get_output_stream_string(sout));
  if (cl__length(strres) > maxWidth) {
    return strres->get_std_string().substr(0, maxWidth) + "...";
  }
  return strres->get_std_string();
}

string InvocationHistoryFrame::asStringLowLevel(Closure_sp closure,int index) const {
  if (!closure) {
    return "InvocationHistoryFrame::asStringLowLevel NULL closure";
  };
  T_sp funcNameObj = closure->functionName();
  string funcName = _rep_(funcNameObj);
  uint lineNumber = closure->lineNumber();
  uint column = closure->column();
  FileScope_sp sfi = gc::As<FileScope_sp>(core__file_scope(closure->sourcePathname()));
  string sourceFileName = sfi->fileName();
  stringstream ss;
  string closureType = "/?";
  if (closure) {
    if (closure->interpretedP()) {
      closureType = "/i";
    } else if (closure->compiledP()) {
      closureType = "/c";
    } else if (closure->builtinP()) {
      closureType = "/b";
    }
  } else
    closureType = "toplevel";
  string sargs = this->argumentsAsString(256);
  ss << (BF("#%3d%2s %20s %5d (%s %s)") % index % closureType % sourceFileName % lineNumber % funcName % sargs).str();
  //	ss << std::endl;
  //	ss << (BF("     activationFrame->%p") % this->activationFrame().get()).str();
  return ss.str();
}

string InvocationHistoryFrame::asString(int index) const {
  string name;
  return this->asStringLowLevel(gc::As<Closure_sp>(this->function()),index);
}

void InvocationHistoryFrame::dump(int index) const {
  string dump = this->asString(index);
  printf("%s\n", dump.c_str());
}

};
