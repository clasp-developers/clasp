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
#include <clasp/core/mpPackage.h>
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
  printf("%s:%d ExceptionStack::backtrace stack size = %zu\n", __FILE__, __LINE__, this->_Stack.size());
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

SimpleVector_sp InvocationHistoryFrame::arguments() const {
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
  SimpleVector_sp vargs = SimpleVector_O::make(numberOfArguments);
  T_O* objRaw;
  for (size_t i(0); i < numberOfArguments; ++i) {
    //objRaw = this->valist_sp().indexed_arg(i);
    objRaw = orig_args->absolute_indexed_arg(i);
    (*vargs)[i] = T_sp((gc::Tagged)objRaw);
  }
  return vargs;
#endif
}

string InvocationHistoryFrame::argumentsAsString(int maxWidth) const {
  SimpleVector_sp vargs = this->arguments();
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


size_t DynamicBindingStack::new_binding_index()
{
#ifdef CLASP_THREADS
  RAIILock<mp::GlobalMutex> mutex(mp::global_BindingIndexPoolMutex);
  if ( mp::global_BindingIndexPool.size() != 0 ) {
    size_t index = mp::global_BindingIndexPool.back();
    mp::global_BindingIndexPool.pop_back();
    return index;
  }
  return mp::global_LastBindingIndex.fetch_add(1);
#else
  return 0;
#endif
};

void DynamicBindingStack::release_binding_index(size_t index)
{
#ifdef CLASP_THREADS
  RAIILock<mp::GlobalMutex> mutex(mp::global_BindingIndexPoolMutex);
  mp::global_BindingIndexPool.push_back(index);
#endif
};

//#define DEBUG_DYNAMIC_BINDING_STACK 1

T_sp* DynamicBindingStack::reference_raw_(Symbol_O* var) const{
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS ) {
#ifdef DEBUG_DYNAMIC_BINDING_STACK
    printf("%s:%d     reference_raw %s    _GlobalValue   returning-> %p\n", __FILE__, __LINE__, _rep_(var->_Name).c_str(), var->_GlobalValue.raw_());
#endif
    return &var->_GlobalValue;
  }
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
  if (gctools::tagged_no_thread_local_bindingp(this->_ThreadLocalBindings[index].raw_())) {
#ifdef DEBUG_DYNAMIC_BINDING_STACK
    printf("%s:%d     reference_raw %s    _GlobalValue  but index = %zu   returning -> %p\n", __FILE__, __LINE__, _rep_(var->_Name).c_str(), index , var->_GlobalValue.raw_());
    fflush(stdout);
#endif
    return &var->_GlobalValue;
  }
#ifdef DEBUG_DYNAMIC_BINDING_STACK
  printf("%s:%d     reference_raw %s    _ThreadLocalBindings[%zu]  -> %p\n", __FILE__, __LINE__, _rep_(var->_Name).c_str(), index, this->_ThreadLocalBindings[index].raw_());
#endif
  return &this->_ThreadLocalBindings[index];
#else
  return &var->_GlobalValue;
#endif
}

SYMBOL_EXPORT_SC_(CorePkg,STARwatchDynamicBindingStackSTAR);
void DynamicBindingStack::push_with_value_coming(Symbol_sp var) {
  T_sp* current_value_ptr = this->reference(var);
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS )
    var->_Binding = this->new_binding_index();
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
#ifdef DEBUG_DYNAMIC_BINDING_STACK // debugging
  printf("%s:%d  caught push[%zu] of %s  pushing value: %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->symbolNameAsString().c_str(), _rep_(this->_ThreadLocalBindings[index]).c_str());
#endif
  this->_Bindings.emplace_back(var,this->_ThreadLocalBindings[index]);
  this->_ThreadLocalBindings[index] = *current_value_ptr;
#else
  this->_Bindings.emplace_back(var,var->symbolValueUnsafe());
#endif
}


void DynamicBindingStack::push(Symbol_sp var, T_sp value) {
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS )
    var->_Binding = this->new_binding_index();
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
#ifdef DEBUG_DYNAMIC_BINDING_STACK // debugging
  printf("%s:%d  caught push[%zu] of %s  pushing value: %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->symbolNameAsString().c_str(), _rep_(this->_ThreadLocalBindings[index]).c_str());
#endif
  this->_Bindings.emplace_back(var,this->_ThreadLocalBindings[index]);
  this->_ThreadLocalBindings[index] = value;
#else
  this->_Bindings.emplace_back(var,var->symbolValueUnsafe());
  this->_GlobalValue = value;
#endif
}



void DynamicBindingStack::pop() {
  DynamicBinding &bind = this->_Bindings.back();
#ifdef DEBUG_DYNAMIC_BINDING_STACK // debugging
#if 1
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
  printf("%s:%d  DynamicBindingStack::pop %s -> %s\n", __FILE__, __LINE__, _rep_(bind._Var).c_str(), _rep_(bind._Val).c_str());
#endif
#ifdef CLASP_THREADS
  ASSERT(this->_ThreadLocalBindings.size()>bind._Var->_Binding); 
  this->_ThreadLocalBindings[bind._Var->_Binding] = bind._Val;
  this->_Bindings.pop_back();
#else
  bind._Var->setf_symbolValue(bind._Val);
  this->_Bindings.pop_back();
#endif
}


}
