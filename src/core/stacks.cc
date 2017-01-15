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
#include <clasp/core/primitives.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

namespace core {

/*! Return the index of the stack entry with the matching key.
      If return -1 then the key wasn't found */
int ExceptionStack::findKey(FrameKind kind, T_sp key) {
  for (int i(this->_Stack.size() - 1); i >= 0; --i) {
    if (this->_Stack[i]._FrameKind == kind && this->_Stack[i]._Key == key)
      return i;
  }
  return -1;
}

Vector_sp ExceptionStack::backtrace() {
  if (this->_Stack.size() == 0) {
    return _Nil<Vector_O>();
  }
  printf("%s:%d ExceptionStack::backtrace stack size = %lu\n", __FILE__, __LINE__, this->_Stack.size());
  Vector_sp result = core__make_vector(_Nil<T_O>(), this->_Stack.size(), false, make_fixnum((int)(this->_Stack.size())));
  for (int i(0), iEnd(this->_Stack.size()); i < iEnd; ++i) {
    Symbol_sp kind;
    SYMBOL_EXPORT_SC_(KeywordPkg, catchFrame);
    SYMBOL_EXPORT_SC_(KeywordPkg, blockFrame);
    SYMBOL_EXPORT_SC_(KeywordPkg, tagbodyFrame);
    SYMBOL_EXPORT_SC_(KeywordPkg, landingPadFrame);
    switch (this->_Stack[i]._FrameKind) {
    case NullFrame:
      kind = _Nil<Symbol_O>();
      break;
    case CatchFrame:
      kind = kw::_sym_catchFrame;
      break;
    case BlockFrame:
      kind = kw::_sym_blockFrame;
      break;
    case TagbodyFrame:
      kind = kw::_sym_tagbodyFrame;
      break;
    case LandingPadFrame:
      kind = kw::_sym_landingPadFrame;
      break;
    };
    result->rowMajorAset(i, Cons_O::create(kind, this->_Stack[i]._Key));
  }
  return result;
}


Function_sp InvocationHistoryFrame::function() const
  {
    VaList_sp args = this->valist_sp();
    Function_sp res = LCC_VA_LIST_CLOSURE(args);
    if ( !res ) {
      printf("%s:%d Frame was found with no closure\n", __FILE__, __LINE__ );
      abort();
    }
    return res;
  }

VectorObjects_sp InvocationHistoryFrame::arguments() const {
#if 0
  VaList_sp orig_args = this->valist_sp();
  VaList_S copy_args_s(*orig_args);
  VaList_sp copy_args(&copy_args_s);
  LCC_RESET_VA_LIST_TO_START(copy_args_s);
  size_t numberOfArguments = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(copy_args);
  VectorObjects_sp vargs = VectorObjects_O::create(_Nil<T_O>(), numberOfArguments, cl::_sym_T_O->symbolValue());
  T_O* objRaw;
#if 0
  for (size_t i(0); i < numberOfArguments; ++i) {
    //objRaw = this->valist_sp().indexed_arg(i);
    LCC_VA_LIST_INDEXED_ARG(objRaw,copy_args,i);
    vargs->rowMajorAset(i, T_sp((gc::Tagged)objRaw));
  }
#endif
  return vargs;
#else
  VaList_sp orig_args = this->valist_sp();
  T_O** register_area = LCC_VA_LIST_REGISTER_SAVE_AREA(orig_args);
  T_O** overflow_area = LCC_ORIGINAL_VA_LIST_OVERFLOW_ARG_AREA(orig_args);
  size_t numberOfArguments = orig_args->total_nargs(); //    LCC_VA_LIST_NUMBER_OF_ARGUMENTS(orig_args);
  VectorObjects_sp vargs = VectorObjects_O::make(numberOfArguments,_Nil<T_O>());
  T_O* objRaw;
  for (size_t i(0); i < numberOfArguments; ++i) {
    //objRaw = this->valist_sp().indexed_arg(i);
    objRaw = orig_args->absolute_indexed_arg(i);
    vargs->rowMajorAset(i, T_sp((gc::Tagged)objRaw));
  }
  return vargs;
#endif
}

string InvocationHistoryFrame::argumentsAsString(int maxWidth) const {
  VectorObjects_sp vargs = this->arguments();
  T_sp sout = clasp_make_string_output_stream();
  int nargs = cl__length(vargs);
  for (int i(0); i < nargs; ++i) {
    T_sp obj = vargs->rowMajorAref(i);
    if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
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
    return strres->get().substr(0, maxWidth) + "...";
  }
  return strres->get();
}

string InvocationHistoryFrame::asStringLowLevel(Closure_sp closure,int index) const {
  if (!closure) {
    return "InvocationHistoryFrame::asStringLowLevel NULL closure";
  };
  T_sp funcNameObj = closure->_name;
  string funcName = _rep_(funcNameObj);
  uint lineNumber = closure->lineNumber();
  uint column = closure->column();
  int sourceFileInfoHandle = closure->sourceFileInfoHandle();
  SourceFileInfo_sp sfi = core__source_file_info(make_fixnum(sourceFileInfoHandle));
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
  return this->asStringLowLevel(this->function(),index);
}

void InvocationHistoryFrame::dump(int index) const {
  string dump = this->asString(index);
  printf("%s\n", dump.c_str());
}


size_t backtrace_size() {
  InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStack;
  size_t count = 0;
  while (frame) {
    frame = frame->_Previous;
    ++count;
  }
  return count;
}

string backtrace_as_string() {
  stringstream ss;
  ss.str("");
  ss << std::endl;
  InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStack;
  ss << "--------STACK TRACE--------" << std::endl;
  int ihsCur = core__ihs_current_frame();
  InvocationHistoryFrame* cur = frame;
  int i = 0;
  while (cur) {
    if (i == ihsCur) {
      ss << "-->";
    } else {
      ss << "   ";
    }
    ss << "frame";
    ss << cur->asString(i) << std::endl;
    cur = cur->_Previous;
    ++i;
  }
  return ss.str();
}


SYMBOL_EXPORT_SC_(CorePkg, STARwatchDynamicBindingStackSTAR);
void DynamicBindingStack::push(Symbol_sp var) {
#if 0 // debugging
  if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
    List_sp assoc = cl__assoc(var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
    if ( assoc.notnilp() ) {
      T_sp funcDesig = oCdr(assoc);
      if ( funcDesig.notnilp() ) {
        eval::funcall(funcDesig,var,_lisp->_true());
      } else {
        printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->formattedName(true).c_str());
      }
    }
  }
#endif
#if 0 // debugging
  if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
	    //	    printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->formattedName(true).c_str());
    printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->symbolNameAsString().c_str());
	    //printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu]\n", __FILE__, __LINE__, this->_Bindings.size() );
  };
#endif
  DynamicBinding bind(var, var->symbolValueUnsafe());
  this->_Bindings.push_back(bind);
}

void DynamicBindingStack::pop() {
  DynamicBinding &bind = this->_Bindings.back();
#if 0 // debugging
  if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValue().notnilp() ) {
    List_sp assoc = cl__assoc(bind._Var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
    if ( assoc.notnilp() ) {
      T_sp funcDesig = oCdr(assoc);
      if ( funcDesig.notnilp() ) {
        eval::funcall(funcDesig,bind._Var,_Nil<T_O>());
      } else {
        printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s  overwriting value = %s\n", __FILE__, __LINE__, this->_Bindings.size()-1, _rep_(bind._Var).c_str(), _rep_(bind._Var->symbolValue()).c_str() );
      }
    }
  }
#endif
#if 0 // debugging
  if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
	    //	    printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size()-1, bind._Var->formattedName(true).c_str());
    printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), bind._Var->symbolNameAsString().c_str());
	    //printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu]\n", __FILE__, __LINE__, this->_Bindings.size() );
  }
#endif
  bind._Var->setf_symbolValue(bind._Val);
  this->_Bindings.pop_back();
}

#ifdef OLD_MPS
GC_RESULT DynamicBindingStack::scanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {
  GC_SCANNER_BEGIN() {
    for (vector<DynamicBinding>::iterator it = this->_Bindings.begin(); it < this->_Bindings.end(); ++it) {
      SMART_PTR_FIX(it->_Var);
      SMART_PTR_FIX(it->_Val);
    }
  }
  GC_SCANNER_END();
  return GC_RES_OK;
}
#endif

#ifdef OLD_MPS
GC_RESULT InvocationHistoryStack::scanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {
  InvocationHistoryStack &ihs = my_thread->invocationHistoryStack(); // in multithreaded code there is one for every thread
  InvocationHistoryFrame *cur = ihs.top();
  GC_SCANNER_BEGIN() {
    while (cur) {
      switch (cur->_IHSFrameKind) {
      case TopLevel: {
        TopLevelIHF *tcur = dynamic_cast<TopLevelIHF *>(cur);
        tcur->scanGCRoots(GC_SCAN_ARGS_PASS);
        break;
      }
      case Debugger: {
        DebuggerIHF *dcur = dynamic_cast<DebuggerIHF *>(cur);
        dcur->scanGCRoots(GC_SCAN_ARGS_PASS);
        break;
      }
      case CxxFunction: {
        CxxFunctionIHF *ccur = dynamic_cast<CxxFunctionIHF *>(cur);
        ccur->scanGCRoots(GC_SCAN_ARGS_PASS);
        break;
      }
      case LispInterpretedFunction: {
        LispInterpretedFunctionIHF *lcur = dynamic_cast<LispInterpretedFunctionIHF *>(cur);
        lcur->scanGCRoots(GC_SCAN_ARGS_PASS);
        break;
      }
      case LispCompiledFunction: {
        LispCompiledFunctionIHF *mcur = dynamic_cast<LispCompiledFunctionIHF *>(cur);
        mcur->scanGCRoots(GC_SCAN_ARGS_PASS);
        break;
      }
      case MacroExpansionFunction: {
        MacroExpansionIHF *mcur = dynamic_cast<MacroExpansionIHF *>(cur);
        mcur->scanGCRoots(GC_SCAN_ARGS_PASS);
        break;
      }
      };
      cur = cur->_Previous;
    }
  }
  GC_SCANNER_END();
  return GC_RES_OK;
}
#endif
}
