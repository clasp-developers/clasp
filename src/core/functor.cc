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
std::atomic<uint64_t> global_interpreted_closure_calls;


FunctionDescription* makeFunctionDescription(T_sp functionName,
                                             T_sp lambda_list,
                                             T_sp docstring,
                                             T_sp sourcePathname,
                                             int lineno,
                                             int column,
                                             int filePos,
                                             T_sp declares) {
  // There is space for 5 roots needed - make sure that matches the number pushed below
  if (my_thread->_GCRoots->remainingCapacity()<FunctionDescription::Roots) {
    my_thread->_GCRoots = new gctools::GCRootsInModule();
  }
  FunctionDescription* fdesc = new FunctionDescription();
  gctools::GCRootsInModule* roots = my_thread->_GCRoots;
  fdesc->gcrootsInModule = roots;
  // There are 6 roots needed 
  fdesc->sourcePathnameIndex =  roots->push_back(sourcePathname.tagged_());
  fdesc->functionNameIndex = roots->push_back(functionName.tagged_());
  fdesc->lambdaListIndex = roots->push_back(lambda_list.tagged_());
  fdesc->docstringIndex = roots->push_back(docstring.tagged_());
  fdesc->declareIndex = roots->push_back(declares.tagged_());
  fdesc->lineno = lineno;
  fdesc->column = column;
  fdesc->filepos = filePos;
  return fdesc;
}

void validateFunctionDescription(const char* filename, size_t lineno, Function_sp func) {
  T_sp functionName = func->functionName();
  if (functionName.unboundp()) {
    printf("FunctionDescription defined at %s:%zu  is missing functionName\n", filename, lineno);
    abort();
  }
  T_sp sourcePathname = func->sourcePathname();
  if (sourcePathname.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing sourcePathname\n", _rep_(functionName).c_str(), filename, lineno);
  }
  T_sp lambdaList = func->lambdaList();
  if (lambdaList.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing lambdaList\n", _rep_(functionName).c_str(), filename, lineno);
  }
  T_sp docstring = func->docstring();
  if (docstring.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing docstring\n", _rep_(functionName).c_str(), filename, lineno);
  }
  T_sp declares = func->declares();
  if (declares.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing declares\n", _rep_(functionName).c_str(), filename, lineno);
  }
}
};

extern "C" void dumpFunctionDescription(void* vfdesc)
{
  core::FunctionDescription* fdesc = (core::FunctionDescription*)vfdesc;
  printf("FunctionDescription @%p\n", fdesc);
  printf("sourcePathname[%d] = %s\n", fdesc->sourcePathnameIndex, _rep_(core::T_sp((gctools::Tagged)fdesc->gcrootsInModule->get(fdesc->sourcePathnameIndex))).c_str()); 
  printf("functionName[%d] = %s\n", fdesc->functionNameIndex, _rep_(core::T_sp((gctools::Tagged)fdesc->gcrootsInModule->get(fdesc->functionNameIndex))).c_str()); 
  printf("lambdaList[%d] = %s\n", fdesc->lambdaListIndex, _rep_(core::T_sp((gctools::Tagged)fdesc->gcrootsInModule->get(fdesc->lambdaListIndex))).c_str()); 
  printf("docstring[%d] = %s\n", fdesc->docstringIndex, _rep_(core::T_sp((gctools::Tagged)fdesc->gcrootsInModule->get(fdesc->docstringIndex))).c_str()); 
  printf("declare[%d] = %s\n", fdesc->declareIndex, _rep_(core::T_sp((gctools::Tagged)fdesc->gcrootsInModule->get(fdesc->declareIndex))).c_str());
  printf("lineno = %d\n", fdesc->lineno);
  printf("column = %d\n", fdesc->column);
  printf("filepos = %d\n", fdesc->filepos);
};

namespace core {

CL_DEFUN void core__dumpFunctionDescription(Function_sp func)
{
  FunctionDescription* fdesc = func->fdesc();
  dumpFunctionDescription(fdesc);
}

CL_LISPIFY_NAME("core:functionSourcePos");
CL_DEFMETHOD T_mv Function_O::functionSourcePos() const {
  T_sp sfi = this->sourcePathname();
  return Values(sfi, make_fixnum(this->filePos()), make_fixnum(this->lineno()));
}

CL_DEFUN T_sp core__function_docstring(Function_sp func) {
  return func->docstring();
}

CL_LISPIFY_NAME("core:function-docstring");
CL_DEFUN_SETF T_sp setf_function_docstring(T_sp doc, Function_sp func) {
  func->setf_docstring(doc);
  return doc;
}

ClosureWithSlots_sp ClosureWithSlots_O::make_interpreted_closure(T_sp name, T_sp type, T_sp lambda_list, LambdaListHandler_sp lambda_list_handler, T_sp declares, T_sp docstring, T_sp form, T_sp environment, SOURCE_INFO) {
  SourceFileInfo_sp sfi = core__source_file_info(core::make_fixnum(sourceFileInfoHandle));
  FunctionDescription* interpretedFunctionDescription = makeFunctionDescription(name,lambda_list,docstring,sfi,lineno,column,filePos);
  ClosureWithSlots_sp closure =
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(INTERPRETED_CLOSURE_SLOTS,
                                                              &interpretedClosureEntryPoint,
                                                              interpretedFunctionDescription,
                                                              ClosureWithSlots_O::interpretedClosure);
  (*closure)[INTERPRETED_CLOSURE_FORM_SLOT] = form;
  (*closure)[INTERPRETED_CLOSURE_ENVIRONMENT_SLOT] = environment;
  if (lambda_list_handler.nilp()) {
    printf("%s:%d  A NIL lambda-list-handler was passed for %s lambdalist: %s\n", __FILE__, __LINE__, _rep_(name).c_str(), _rep_(lambda_list).c_str());
    abort();
  }
  (*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT] = lambda_list_handler;
  closure->setf_lambdaList(lambda_list_handler->lambdaList());
  closure->setf_declares(declares);
  closure->setf_docstring(docstring);
  validateFunctionDescription(__FILE__,__LINE__,closure);
  return closure;
}
ClosureWithSlots_sp ClosureWithSlots_O::make_bclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, T_sp environment) {
  core::FunctionDescription* fdesc = makeFunctionDescription(name,lambda_list);
  ClosureWithSlots_sp closure = 
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(BCLASP_CLOSURE_SLOTS,
                                                              ptr,
                                                              (core::FunctionDescription*)fdesc,
                                                              ClosureWithSlots_O::bclaspClosure);
  (*closure)[BCLASP_CLOSURE_ENVIRONMENT_SLOT] = environment;
  closure->setf_sourcePathname(_Nil<T_O>());
  closure->setf_lambdaList(lambda_list);
  closure->setf_declares(_Nil<T_O>());
  closure->setf_docstring(_Nil<T_O>());
  validateFunctionDescription(__FILE__,__LINE__,closure);
  return closure;
}

ClosureWithSlots_sp ClosureWithSlots_O::make_cclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, SOURCE_INFO) {
  core::FunctionDescription* fdesc = makeFunctionDescription(name,lambda_list);
  ClosureWithSlots_sp closure = 
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(0,
                                                              ptr,
                                                              (core::FunctionDescription*)fdesc,
                                                              ClosureWithSlots_O::cclaspClosure);
  closure->setf_lambdaList(lambda_list);
  closure->setf_declares(_Nil<T_O>());
  closure->setf_docstring(_Nil<T_O>());
  validateFunctionDescription(__FILE__,__LINE__,closure);
  return closure;
}

CL_DEFUN Integer_sp core__interpreted_closure_calls() {
  return Integer_O::create((Fixnum)global_interpreted_closure_calls);
}

#ifdef DEBUG_FUNCTION_CALL_COUNTER

CL_DEFUN size_t core__function_call_counter(Function_sp f)
{
  return f->_TimesCalled;
}
#endif

T_sp Function_O::setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) {
  SourceFileInfo_mv sfi = core__source_file_info(sourceFile);
  this->setf_sourcePathname(sfi->pathname());
  this->setf_filePos(filePos);
  this->setf_lineno(lineno);
  this->setf_column(column);
  SourcePosInfo_sp spi = SourcePosInfo_O::create(sfi, filePos, lineno, column);
  return spi;
}

CL_DEFMETHOD Pointer_sp Function_O::function_pointer() const {
  return Pointer_O::create((void*)this->entry.load());
};

string Function_O::__repr__() const {
  T_sp name = this->functionName();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
#ifdef USE_BOEHM
  ss << "@" << (void*)this << " ";
#endif
  ss << " " << _rep_(name);
  ss << " lambda-list: " << _rep_(this->lambdaList());
  if ( this->entry != NULL ) {
    ss << " :fptr " << reinterpret_cast<void*>(this->entry.load());
  }
  
  ss << ">";
  return ss.str();
}

CL_DEFMETHOD Pointer_sp Function_O::function_description_address() const {
  return Pointer_O::create(this->fdesc());
}

CL_DEFUN void core__set_function_description_address(Function_sp func, Pointer_sp address) {
  func->set_fdesc((FunctionDescription*)(address->ptr()));
}

};


namespace core {
char* global_dump_functions = NULL;
void Closure_O::describeFunction() const {
  if (global_dump_functions) {
    printf("%s:%d  Closure_O %s entry@%p fdesc@%p\n", __FILE__, __LINE__, _rep_(this->functionName()).c_str(), (void*)this->entry.load(),(void*)this->fdesc());
  }
}

CL_DEFUN size_t core__closure_with_slots_size(size_t number_of_slots)
{
  size_t result = gctools::sizeof_container_with_header<ClosureWithSlots_O>(number_of_slots);
  return result;
}

CL_DEFUN size_t core__closure_length(Closure_sp tclosure)
{
  ASSERT(gc::IsA<ClosureWithSlots_sp>(tclosure));
  ClosureWithSlots_sp closure = gc::As_unsafe<ClosureWithSlots_sp>(tclosure);
  if (closure->closureType == ClosureWithSlots_O::cclaspClosure) {
    return closure->_Slots._Length;
  }
  return gc::As<ValueFrame_sp>(tclosure->closedEnvironment())->length();
}

T_sp ClosureWithSlots_O::code() const {
  if (this->interpretedP()) {
    return (*this)[INTERPRETED_CLOSURE_FORM_SLOT];
  }
  SIMPLE_ERROR(BF("Tried to get code for a non interpreted closure"));
}


string ClosureWithSlots_O::__repr__() const {
  T_sp name = this->functionName();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
#ifdef USE_BOEHM
  ss << "@" << (void*)this << " ";
#endif
  ss << " " << _rep_(name);
  ss << " :type ";
  switch (this->closureType) {
  case interpretedClosure:
      ss << "interpreted ";
      break;
  case bclaspClosure:
      ss << "bclasp ";
      break;
  case cclaspClosure:
      ss << "cclasp ";
      break;
  }
  ss << " lambda-list: " << _rep_(this->lambdaList());
  if ( this->entry != NULL ) {
    ss << " :fptr " << reinterpret_cast<void*>(this->entry.load());
  }
  
  ss << ">";
  return ss.str();
}



CL_DEFUN T_sp core__closure_ref(Closure_sp tclosure, size_t index)
{
  if ( ClosureWithSlots_sp closure = tclosure.asOrNull<ClosureWithSlots_O>() ) {
    switch (closure->closureType) {
    case ClosureWithSlots_O::interpretedClosure:
    case ClosureWithSlots_O::bclaspClosure:
      {
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
        SIMPLE_ERROR(BF("Out of bounds closure reference - there are no slots"));
      }
        break;
    case ClosureWithSlots_O::cclaspClosure:
        if ( index >= closure->_Slots._Length ) {
          SIMPLE_ERROR(BF("Out of bounds closure reference - there are only %d slots") % closure->_Slots._Length );
        }
        return closure->_Slots[index];
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

LCC_RETURN unboundFunctionEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS) {
  ClosureWithSlots_O* closure = gctools::untag_general<ClosureWithSlots_O*>((ClosureWithSlots_O*)lcc_closure);
  Symbol_sp symbol = gc::As<Symbol_sp>((*closure)[0]);
  ERROR_UNDEFINED_FUNCTION(symbol);
}

LCC_RETURN unboundSetfFunctionEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS) {
  ClosureWithSlots_O* closure = gctools::untag_general<ClosureWithSlots_O*>((ClosureWithSlots_O*)lcc_closure);
  Symbol_sp symbol = gc::As<Symbol_sp>((*closure)[0]);
  List_sp name = Cons_O::createList(cl::_sym_setf,symbol);
  ERROR_UNDEFINED_FUNCTION(name);
}

//  printf("%s:%d    closure name -> %s\n", __FILE__, __LINE__, _rep_(closure->functionName()).c_str());

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS) {
  ClosureWithSlots_O* closure = gctools::untag_general<ClosureWithSlots_O*>((ClosureWithSlots_O*)lcc_closure);
//  printf("%s:%d    closure name -> %s\n", __FILE__, __LINE__, _rep_(closure->functionName()).c_str());
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  INITIALIZE_VA_LIST();
  ++global_interpreted_closure_calls;
  ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler((*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT], (*closure)[INTERPRETED_CLOSURE_ENVIRONMENT_SLOT]);
//  printf("%s:%d ValueEnvironment_O:createForLambdaListHandler llh: %s\n", __FILE__, __LINE__, _rep_(this->_lambdaListHandler).c_str());
//  newValueEnvironment->dump();
  ValueEnvironmentDynamicScopeManager scope(newValueEnvironment);
  ALWAYS_INVOCATION_HISTORY_FRAME(); // InvocationHistoryFrame _frame(&lcc_arglist_s._Args);
  lambdaListHandler_createBindings(closure->asSmartPtr(), (*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT], scope, LCC_PASS_ARGS_LLH);
//  printf("%s:%d     after lambdaListHandler_createbindings\n", __FILE__, __LINE__);
//  newValueEnvironment->dump();
  ValueFrame_sp newActivationFrame = gc::As<ValueFrame_sp>(newValueEnvironment->getActivationFrame());
  //        InvocationHistoryFrame _frame(this,newActivationFrame);
  return eval::sp_progn((*closure)[INTERPRETED_CLOSURE_FORM_SLOT], newValueEnvironment).as_return_type();
};


};

