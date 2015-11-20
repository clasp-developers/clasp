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
#include <clasp/core/str.h>
#include <clasp/core/instance.h>
#include <clasp/core/primitives.h>
#include <clasp/core/vectorObjects.h>
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
  Vector_sp result = core_make_vector(_Nil<T_O>(), this->_Stack.size(), false, make_fixnum((int)(this->_Stack.size())));
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
    result->setf_elt(i, Cons_O::create(kind, this->_Stack[i]._Key));
  }
  return result;
}

InvocationHistoryFrame::InvocationHistoryFrame(gctools::tagged_pointer<Closure> c, core::T_O *valist_sptr, T_sp env)
    : closure(c), environment(env), _NumberOfArguments(0), _RegisterArguments(NULL), _StackArguments(NULL) {
  if (valist_sptr != NULL) {
    VaList_sp arguments(reinterpret_cast<core::VaList_S *>(gc::untag_valist(valist_sptr)));
    this->_NumberOfArguments = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(arguments);
    this->_RegisterArguments = LCC_VA_LIST_REGISTER_SAVE_AREA(arguments);
    this->_StackArguments = LCC_VA_LIST_OVERFLOW_ARG_AREA(arguments);
  }
  this->_Stack = &_lisp->invocationHistoryStack();
  this->_Previous = this->_Stack->top();
  if (this->_Previous == NULL) {
    this->_Index = 0;
  } else {
    this->_Index = this->_Previous->_Index + 1;
  }
  this->_Stack->push(this);
  this->_Bds = _lisp->bindings().size();
}

InvocationHistoryFrame::~InvocationHistoryFrame() {
  this->_Stack->pop();
}

VectorObjects_sp InvocationHistoryFrame::arguments() const {
  if (this->_NumberOfArguments == 0) {
    VectorObjects_sp vnone = VectorObjects_O::create(_Nil<T_O>(), 0, cl::_sym_T_O->symbolValue());
    return vnone;
  }
  size_t numberOfArguments = this->_NumberOfArguments;
  VectorObjects_sp vargs = VectorObjects_O::create(_Nil<T_O>(), numberOfArguments, cl::_sym_T_O->symbolValue());
  for (size_t i(0); i < numberOfArguments; ++i) {
    T_sp obj;
    if (i < LCC_ARGS_IN_REGISTERS) {
      obj = core::T_sp((gc::Tagged)(this->_RegisterArguments[LCC_ABI_ARGS_IN_REGISTERS - LCC_ARGS_IN_REGISTERS + i]));
    } else {
      obj = core::T_sp((gc::Tagged)(this->_StackArguments[i - LCC_ARGS_IN_REGISTERS]));
    }
    vargs->setf_elt(i, obj);
  }
  return vargs;
}

string InvocationHistoryFrame::argumentsAsString(int maxWidth) const {
  VectorObjects_sp vargs = this->arguments();
  T_sp sout = clasp_make_string_output_stream();
  int nargs = cl_length(vargs);
  for (int i(0); i < nargs; ++i) {
    T_sp obj = vargs->elt(i);
    if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
      clasp_write_string("#<", sout);
      write_ugly_object(af_classOf(obj), sout);
      clasp_write_string("> ", sout);
    } else {
      write_ugly_object(obj, sout);
      clasp_write_char(' ', sout);
    }
  }
  Str_sp strres = gc::As<Str_sp>(cl_get_output_stream_string(sout));
  if (cl_length(strres) > maxWidth) {
    return strres->get().substr(0, maxWidth) + "...";
  }
  return strres->get();
}

string InvocationHistoryFrame::asStringLowLevel(gctools::tagged_pointer<Closure> closure) const {
  if (!closure) {
    return "InvocationHistoryFrame::asStringLowLevel NULL closure";
  };
  T_sp funcNameObj = closure->name;
  string funcName = _rep_(funcNameObj);
  uint lineNumber = closure->lineNumber();
  uint column = closure->column();
  int sourceFileInfoHandle = closure->sourceFileInfoHandle();
  SourceFileInfo_sp sfi = core_sourceFileInfo(make_fixnum(sourceFileInfoHandle));
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
  ss << (BF("#%3d%2s@%p %20s %5d/%-3d (%s %s)") % this->_Index % closureType % (void *)closure.raw_() % sourceFileName % lineNumber % column % funcName % sargs).str();
  //	ss << std::endl;
  //	ss << (BF("     activationFrame->%p") % this->activationFrame().get()).str();
  return ss.str();
}

string InvocationHistoryFrame::asString() const {
  string name;
  return this->asStringLowLevel(this->closure);
}

void InvocationHistoryFrame::dump() const {
  string dump = this->asString();
  printf("%s\n", dump.c_str());
}

vector<InvocationHistoryFrame *> InvocationHistoryStack::asVectorFrames() {
  vector<InvocationHistoryFrame *> frames;
  frames.resize(this->_Top->index() + 1);
  for (InvocationHistoryFrame *cur = _lisp->invocationHistoryStack().top();
       cur != NULL; cur = cur->previous()) {
    frames[cur->index()] = cur;
  }
  return frames;
}

string InvocationHistoryStack::asString() const {
  stringstream ss;
  ss.str("");
  ss << std::endl;
  vector<InvocationHistoryFrame *> frames = _lisp->invocationHistoryStack().asVectorFrames();
  ss << "--------STACK TRACE--------" << std::endl;
  int ihsCur = af_ihsCurrentFrame();
  for (int i = 0; i < frames.size(); ++i) {
    InvocationHistoryFrame *cur = frames[i];
    if (i == ihsCur) {
      ss << "-->";
    } else {
      ss << "   ";
    }
    ss << "frame";
    ss << cur->asString() << std::endl;
  }
  return ss.str();
}

SYMBOL_EXPORT_SC_(CorePkg, STARwatchDynamicBindingStackSTAR);
void DynamicBindingStack::push(Symbol_sp var) {
#if 0 // debugging
  if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
    List_sp assoc = cl_assoc(var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
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
    List_sp assoc = cl_assoc(bind._Var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
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
  InvocationHistoryStack &ihs = _lisp->invocationHistoryStack(); // in multithreaded code there is one for every thread
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

namespace core {

void initialize_stacks() {
  //	CoreDefun(dynamicBindingStackDump);
}
};
