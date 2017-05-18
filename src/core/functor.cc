/*
    File: functor.cc
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
//#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
//#include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/instance.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/lambdaListHandler.h>
//#i n c l u d e "environmentDependent.h"
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>



namespace core {

// Keep track of how many interpreted closure calls there are
uint64_t global_interpreted_closure_calls = 0;

CL_DEFUN Integer_sp core__interpreted_closure_calls() {
  return Integer_O::create((Fixnum)global_interpreted_closure_calls);
}

#ifdef DEBUG_FUNCTION_CALL_COUNTER

CL_DEFUN size_t core__function_call_counter(Function_sp f)
{
  return f->_TimesCalled;
}
#endif

string Function_O::__repr__() const {
  T_sp name = this->name();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString();
#ifdef USE_BOEHM
  ss << "@" << (void*)this << " ";
#endif
  ss << " " << _rep_(name);
  ss << " :ftype " << _rep_(this->getKind());
  ss << " lambda-list: " << _rep_(this->lambda_list());
  if ( this->functionAddress() != NULL ) {
    ss << " :fptr " << reinterpret_cast<void*>(this->functionAddress());
  }
  ss << ">";
  return ss.str();
}

string Closure_O::nameAsString() const {
  if (this->_name.nilp()) {
    return "Function-name(NIL)";
  } else if (Symbol_sp sname = this->_name.asOrNull<Symbol_O>()) {
    stringstream ss;
    ss << "Function-name(";
    ss << sname->symbolNameAsString();
    ss << ")";
    return ss.str();
  } else if (Cons_sp cname = this->_name.asOrNull<Cons_O>()) {
    stringstream ss;
    ss << "Function-name(setf ";
    ss << gc::As<Symbol_sp>(oCadr(cname))->symbolNameAsString();
    ss << ")";
    return ss.str();
  } else if (cl__stringp(this->_name)) {
    String_sp strname = gc::As_unsafe<String_sp>(this->_name);
    stringstream ss;
    ss << "Function-name(string-";
    ss << strname->get();
    ss << ")";
    return ss.str();
  }
  THROW_HARD_ERROR(BF("Cannot get name as string of Functoid"));
}

};


namespace core {

List_sp FunctionClosure_O::source_info() const {
  return Cons_O::createList(clasp_make_fixnum(this->_sourceFileInfoHandle),
                            clasp_make_fixnum(this->_filePos),
                            clasp_make_fixnum(this->_lineno),
                            clasp_make_fixnum(this->_column));
};

void FunctionClosure_O::set_source_info(List_sp source_info)
{
  T_sp sourceFileInfo = oCar(source_info);
  T_sp filePos = oCadr(source_info);
  T_sp lineno = oCaddr(source_info);
  T_sp column = oCadddr(source_info);
  if (sourceFileInfo.fixnump()) {
    this->_sourceFileInfoHandle = sourceFileInfo.unsafe_fixnum();
  } else if (Symbol_sp sym = sourceFileInfo.asOrNull<Symbol_O>()) {
    // do nothing - leave the sourceFileInfoHandle alone
    if ( sym == _sym_current_source_file ) {
      // Do nothing, leave the sourceFileInfo alone
    } else {
      printf("%s:%d Illegal source file name designator: %s\n", __FILE__, __LINE__, _rep_(sym).c_str());
    }
  } else if (cl__stringp(sourceFileInfo)) {
    String_sp str = gc::As_unsafe<String_sp>(sourceFileInfo);
    this->_sourceFileInfoHandle = _lisp->getOrRegisterSourceFileInfo(str->get_std_string());
    printf("%s:%d  Function is having its source-info file set after the fact to: %s  filePos: %s\n", __FILE__,__LINE__,str->get().c_str(), _rep_(filePos).c_str());
  }
  this->_filePos = filePos.fixnump() ? filePos.unsafe_fixnum() : 0;
  this->_lineno = lineno.fixnump() ? lineno.unsafe_fixnum() : 0;
  this->_column = column.fixnump() ? column.unsafe_fixnum() : 0;
}


bool FunctionClosure_O::macroP() const {
  return this->kind == kw::_sym_macro;
}
int FunctionClosure_O::sourceFileInfoHandle() const {
  return this->_sourceFileInfoHandle;
};

size_t FunctionClosure_O::filePos() const {
  return this->_filePos;
}

int FunctionClosure_O::lineNumber() const {
  return this->_lineno;
}

int FunctionClosure_O::column() const {
  return this->_column;
}

T_sp FunctionClosure_O::setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) {
  SourceFileInfo_mv sfi = core__source_file_info(sourceFile);
  this->_sourceFileInfoHandle = gc::As<Fixnum_sp>(sfi.valueGet_(1)).unsafe_fixnum();
  this->_filePos = filePos;
  this->_lineno = lineno;
  this->_column = column;
  SourcePosInfo_sp spi = SourcePosInfo_O::create(this->_sourceFileInfoHandle, filePos, lineno, column);
  return spi;
}

T_sp FunctionClosure_O::sourcePosInfo() const {
  SourcePosInfo_sp spi = SourcePosInfo_O::create(this->_sourceFileInfoHandle, this->_filePos, this->_lineno, this->_column);
  return spi;
}

CL_DEFUN size_t core__closure_with_slots_size(size_t number_of_slots)
{
  size_t result = gctools::sizeof_container_with_header<ClosureWithSlots_O>(number_of_slots);
  return result;
}

CL_DEFUN size_t core__closure_length(Closure_sp tclosure)
{
  if ( ClosureWithSlots_sp closure = tclosure.asOrNull<ClosureWithSlots_O>() ) {
    return closure->_Slots._Length;
  } else if ( ClosureWithFrame_sp closure = tclosure.asOrNull<ClosureWithFrame_O>() ) {
    T_sp env = closure->closedEnvironment();
    if ( ValueEnvironment_sp ve = env.asOrNull<ValueEnvironment_O>() ) {
      env = ve->getActivationFrame();
    }
    if ( ValueFrame_sp tvf = env.asOrNull<ValueFrame_O>() ) {
      return tvf->length();
    }
  }
  return 0;
}

CL_DEFUN T_sp core__closure_ref(Closure_sp tclosure, size_t index)
{
  if ( ClosureWithSlots_sp closure = tclosure.asOrNull<ClosureWithSlots_O>() ) {
    if ( index >= closure->_Slots._Length ) {
      SIMPLE_ERROR(BF("Out of bounds closure reference - there are only %d slots") % closure->_Slots._Length );
    }
    return closure->_Slots[index];
  } else if ( ClosureWithFrame_sp closure = tclosure.asOrNull<ClosureWithFrame_O>() ) {
    T_sp env = closure->closedEnvironment();
    if ( ValueEnvironment_sp ve = env.asOrNull<ValueEnvironment_O>() ) {
      env = ve->getActivationFrame();
    }
    if ( ValueFrame_sp tvf = env.asOrNull<ValueFrame_O>() ) {
      if ( index >= tvf->length() ) {
        SIMPLE_ERROR(BF("Out of bounds closure reference - there are only %d slots") % tvf->length() );
      }
      return (*tvf)[index];
    }
  }
  SIMPLE_ERROR(BF("Out of bounds closure reference - there are no slots"));
}

CL_DEFUN void core__closure_slots_dump(Closure_sp closure) {
  size_t nslots = core__closure_length(closure);
  printf("Closure has %zu slots\n", nslots);
  for ( int i=0; i<nslots; ++i ) {
    printf("    Slot[%d] --> %s\n", i, _rep_(core__closure_ref(closure,i)).c_str());
  }
  SourcePosInfo_sp spi = closure->sourcePosInfo();
  T_mv tsfi = core__source_file_info(spi);
  if ( SourceFileInfo_sp sfi = tsfi.asOrNull<SourceFileInfo_O>() ) {
    printf("Closure source: %s:%d\n", sfi->namestring().c_str(), spi->lineno() );
  }
}

T_sp BuiltinClosure_O::lambda_list() const {
  return this->_lambdaListHandler->lambdaList();
}

void BuiltinClosure_O::setf_lambda_list(List_sp lambda_list) {
  // Do nothing
}

#if 0
LCC_RETURN BuiltinClosure_O::LISP_CALLING_CONVENTION() {
  INCREMENT_FUNCTION_CALL_COUNTER(this);
  IMPLEMENT_MEF(BF("Handle call to BuiltinClosure"));
};
#endif


InterpretedClosure_O::InterpretedClosure_O(T_sp fn, Symbol_sp k, LambdaListHandler_sp llh, List_sp dec, T_sp doc, T_sp e, List_sp c, SOURCE_INFO)
  : Base(interpretedClosureEntryPoint,fn, k, e, SOURCE_INFO_PASS), _lambdaListHandler(llh), _declares(dec), _docstring(doc), _code(c) {
}

T_sp InterpretedClosure_O::lambda_list() const {
  return this->lambdaListHandler()->lambdaList();
}

void InterpretedClosure_O::setf_lambda_list(List_sp lambda_list) {
  // Do nothing - setting the lambdaListHandler is all that's needed
}

LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS) {
  InterpretedClosure_O* closure = gctools::untag_general<InterpretedClosure_O*>((InterpretedClosure_O*)lcc_closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  INITIALIZE_VA_LIST();
  ++global_interpreted_closure_calls;
  ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler(closure->_lambdaListHandler, closure->_closedEnvironment);
//  printf("%s:%d ValueEnvironment_O:createForLambdaListHandler llh: %s\n", __FILE__, __LINE__, _rep_(this->_lambdaListHandler).c_str());
//  newValueEnvironment->dump();
  ValueEnvironmentDynamicScopeManager scope(newValueEnvironment);
#ifdef USE_EXPENSIVE_BACKTRACE
  INVOCATION_HISTORY_FRAME(); // InvocationHistoryFrame _frame(&lcc_arglist_s._Args);
#endif
  lambdaListHandler_createBindings(closure->asSmartPtr(), closure->_lambdaListHandler, scope, LCC_PASS_ARGS_LLH);
//  printf("%s:%d     after lambdaListHandler_createbindings\n", __FILE__, __LINE__);
//  newValueEnvironment->dump();
  ValueFrame_sp newActivationFrame = gc::As<ValueFrame_sp>(newValueEnvironment->getActivationFrame());
  //        InvocationHistoryFrame _frame(this,newActivationFrame);
  return eval::sp_progn(closure->_code, newValueEnvironment).as_return_type();
};


};

namespace core {


LCC_RETURN compiledDispatchFunctionDummyEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS)
{
  SIMPLE_ERROR(BF("Dont ever call this"));
}


#ifdef USE_COMPILED_CLOSURE
core::T_sp CompiledClosure_O::lambda_list() const {
  return this->_lambdaList;
}

void CompiledClosure_O::setf_lambda_list(core::List_sp lambda_list) {
  this->_lambdaList = lambda_list;
}
#endif


#if 0
LCC_RETURN InstanceClosure_O::LISP_CALLING_CONVENTION() {
  INCREMENT_FUNCTION_CALL_COUNTER(this);
// Copy the arguments passed in registers into the multiple_values array and those
// will be processed by the generic function
#ifdef _DEBUG_BUILD
  VaList_S saved_args(*reinterpret_cast<VaList_S *>(untag_valist(lcc_arglist)));
#endif
  VaList_sp gfargs((gc::Tagged)lcc_arglist);
  return (this->entryPoint)(this->instance, gfargs);
}
#endif

LCC_RETURN MacroClosure_O::entry_point(LCC_ARGS_ELLIPSIS) {
  MacroClosure_O* closure = gctools::untag_general<MacroClosure_O*>((MacroClosure_O*)lcc_closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
    List_sp form = gc::As<Cons_sp>(LCC_ARG0());
    T_sp env = gc::As<T_sp>(LCC_ARG1());
//    InvocationHistoryFrame _frame(lcc_vargs); // The environment could be a Non-Clasp Environment (Cleavir)
    return ((closure->mptr)(form, env)).as_return_type();
  };


};
