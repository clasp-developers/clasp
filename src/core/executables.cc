/*
    File: executables.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/executables.h>
#include <clasp/core/lisp.h>
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
//#include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/lambdaListHandler.h>
//#i n c l u d e "environmentDependent.h"
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(function);
CL_DECLARE();
CL_DOCSTRING("functionLambdaList");
CL_DEFUN T_mv core__function_lambda_list(T_sp obj) {
  if (obj.nilp()) {
    return Values(_Nil<T_O>(), _Nil<T_O>());
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    if (!sym->fboundp()) {
      return Values(_Nil<T_O>(), _Nil<T_O>());
    }
    Function_sp fn = sym->symbolFunction();
    return Values(fn->lambdaList(), _lisp->_true());
  } else if (Function_sp func = obj.asOrNull<Function_O>()) {
    return Values(func->lambdaList(), _lisp->_true());
  }
  return Values(_Nil<T_O>(), _Nil<T_O>());
}

CL_LAMBDA(function);
CL_DECLARE();
CL_DOCSTRING("functionSourcePosInfo");
CL_DEFUN gc::Nilable<SourcePosInfo_sp> core__function_source_pos_info(T_sp functionDesignator) {
  Function_sp func = coerce::functionDesignator(functionDesignator);
  gctools::tagged_pointer<Closure> closure = func->closure;
  gc::Nilable<SourcePosInfo_sp> sourcePosInfo = closure->sourcePosInfo();
  return sourcePosInfo;
}

CL_LAMBDA(fn kind);
CL_DECLARE();
CL_DOCSTRING("set the kind of a function object (:function|:macro)");
CL_DEFUN void core__set_kind(Function_sp fn, Symbol_sp kind) {
  fn->closure->setKind(kind);
};

bool FunctionClosure::macroP() const {
  return this->kind == kw::_sym_macro;
}
int FunctionClosure::sourceFileInfoHandle() const {
  return this->_sourceFileInfoHandle;
};

size_t FunctionClosure::filePos() const {
  return this->_filePos;
}

int FunctionClosure::lineNumber() const {
  return this->_lineno;
}

int FunctionClosure::column() const {
  return this->_column;
}

T_sp FunctionClosure::setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) {
  SourceFileInfo_mv sfi = core__source_file_info(sourceFile);
  this->_sourceFileInfoHandle = gc::As<Fixnum_sp>(sfi.valueGet(1)).unsafe_fixnum();
  this->_filePos = filePos;
  this->_lineno = lineno;
  this->_column = column;
  SourcePosInfo_sp spi = SourcePosInfo_O::create(this->_sourceFileInfoHandle, filePos, lineno, column);
  return spi;
}

T_sp FunctionClosure::sourcePosInfo() const {
  SourcePosInfo_sp spi = SourcePosInfo_O::create(this->_sourceFileInfoHandle, this->_filePos, this->_lineno, this->_column);
  return spi;
}

T_sp BuiltinClosure::lambdaList() const {
  return this->_lambdaListHandler->lambdaList();
}

void BuiltinClosure::setf_lambda_list(T_sp lambda_list) {
  // Do nothing
}

LCC_RETURN BuiltinClosure::LISP_CALLING_CONVENTION() {
  IMPLEMENT_MEF(BF("Handle call to BuiltinClosure"));
};

InterpretedClosure::InterpretedClosure(T_sp fn, Symbol_sp k, LambdaListHandler_sp llh, List_sp dec, T_sp doc, T_sp e, List_sp c, SOURCE_INFO)
  : FunctionClosure(fn, k, e, SOURCE_INFO_PASS), _lambdaListHandler(llh), _declares(dec), _docstring(doc), _code(c) {
}

T_sp InterpretedClosure::lambdaList() const {
  return this->lambdaListHandler()->lambdaList();
}

void InterpretedClosure::setf_lambda_list(T_sp lambda_list) {
  // Do nothing - setting the lambdaListHandler is all that's needed
}

LCC_RETURN InterpretedClosure::LISP_CALLING_CONVENTION() {
  ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler(this->_lambdaListHandler, this->closedEnvironment);
  ValueEnvironmentDynamicScopeManager scope(newValueEnvironment);
  InvocationHistoryFrame _frame(gctools::tagged_pointer<Closure>(this), lcc_arglist);
  lambdaListHandler_createBindings(gctools::tagged_pointer<Closure>(this), this->_lambdaListHandler, scope, LCC_PASS_ARGS);
  ValueFrame_sp newActivationFrame = gc::As<ValueFrame_sp>(newValueEnvironment->getActivationFrame());
  VectorObjects_sp debuggingInfo = _lambdaListHandler->namesOfLexicalVariablesForDebugging();
  newActivationFrame->attachDebuggingInfo(debuggingInfo);
  //        InvocationHistoryFrame _frame(this,newActivationFrame);
  _frame.setActivationFrame(newActivationFrame);
#if 0
  if (_sym_STARdebugInterpretedClosureSTAR->symbolValue().notnilp()) {
    printf("%s:%d Entering InterpretedClosure   source file = %s  lineno=%d\n", __FILE__, __LINE__, _frame.sourcePathName().c_str(), _frame.lineno());
  }
#endif
  return eval::sp_progn(this->_code, newValueEnvironment).as_return_type();
};

T_mv Function_O::lambdaList() {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return Values(this->closure->lambdaList(), _lisp->_true());
}

CL_DEFMETHOD void Function_O::setf_lambda_list(T_sp ll) {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  this->closure->setf_lambda_list(ll);
}

CL_LISPIFY_NAME("core:cleavir_ast");
CL_DEFMETHOD T_sp Function_O::cleavir_ast() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->cleavir_ast();
}

CL_LISPIFY_NAME("core:setf_cleavir_ast");
CL_DEFMETHOD void Function_O::setf_cleavir_ast(T_sp ast) {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  this->closure->setf_cleavir_ast(ast);
}

CL_LISPIFY_NAME("core:functionLambdaListHandler");
CL_DEFMETHOD T_sp Function_O::functionLambdaListHandler() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->lambdaListHandler();
};
CL_LISPIFY_NAME("core:macrop");
CL_DEFMETHOD bool Function_O::macroP() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->macroP();
}

CL_LISPIFY_NAME("core:setFunctionKind");
CL_DEFMETHOD void Function_O::setKind(Symbol_sp k) {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  this->closure->setKind(k);
}
CL_LISPIFY_NAME("core:functionKind");
CL_DEFMETHOD Symbol_sp Function_O::functionKind() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->getKind();
};

CL_LISPIFY_NAME("core:function_docstring");
CL_DEFMETHOD T_sp Function_O::docstring() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->docstring();
};

CL_LISPIFY_NAME("core:function_declares");
CL_DEFMETHOD List_sp Function_O::declares() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->declares();
};
CL_LISPIFY_NAME("core:closedEnvironment");
CL_DEFMETHOD T_sp Function_O::closedEnvironment() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->closedEnvironment;
};

CL_LISPIFY_NAME("core:functionName");
CL_DEFMETHOD T_sp Function_O::functionName() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  return this->closure->name;
}

CL_LISPIFY_NAME("core:functionSourcePos");
CL_DEFMETHOD T_mv Function_O::functionSourcePos() const {
  ASSERTF(this->closure, BF("The Function closure is NULL"));
  T_sp spi = this->closure->sourcePosInfo();
  T_sp sfi = core__source_file_info(spi);
  if (sfi.nilp() || spi.nilp()) {
    return Values(sfi, make_fixnum(0), make_fixnum(0));
  }
  return Values(sfi, make_fixnum(gc::As<SourcePosInfo_sp>(spi)->filepos()), make_fixnum(gc::As<SourcePosInfo_sp>(spi)->lineno()));
}



SYMBOL_EXPORT_SC_(KeywordPkg, calledFunction);
SYMBOL_EXPORT_SC_(KeywordPkg, givenNumberOfArguments);
SYMBOL_EXPORT_SC_(KeywordPkg, requiredNumberOfArguments);
SYMBOL_EXPORT_SC_(KeywordPkg, unrecognizedKeyword);

void handleArgumentHandlingExceptions(gctools::tagged_pointer<Closure> closure) {
  Function_sp func = Function_O::make(closure);
  try {
    throw;
  } catch (TooManyArgumentsError &error) {
    lisp_error(core::_sym_tooManyArgumentsError, lisp_createList(kw::_sym_calledFunction, func, kw::_sym_givenNumberOfArguments, make_fixnum(error.givenNumberOfArguments), kw::_sym_requiredNumberOfArguments, make_fixnum(error.requiredNumberOfArguments)));
  } catch (TooFewArgumentsError &error) {
    lisp_error(core::_sym_tooFewArgumentsError, lisp_createList(kw::_sym_calledFunction, func, kw::_sym_givenNumberOfArguments, make_fixnum(error.givenNumberOfArguments), kw::_sym_requiredNumberOfArguments, make_fixnum(error.requiredNumberOfArguments)));
  } catch (UnrecognizedKeywordArgumentError &error) {
    lisp_error(core::_sym_unrecognizedKeywordArgumentError, lisp_createList(kw::_sym_calledFunction, func, kw::_sym_unrecognizedKeyword, error.argument));
  }
}

CL_LAMBDA(fn);
CL_DECLARE();
CL_DOCSTRING("functionLambdaExpression");
CL_DEFUN T_mv cl__function_lambda_expression(Function_sp fn) {
  T_sp code;
  code = core__function_lambda_list(fn);
  bool closedp = true; // fn->closedEnvironment().notnilp();
  T_sp name = fn->closure->name;
  return Values(code, _lisp->_boolean(closedp), name);
};

CL_LAMBDA(fn);
CL_DECLARE();
CL_DOCSTRING("functionSourceCode");
CL_DEFUN T_sp core__function_source_code(Function_sp fn) {
  gctools::tagged_pointer<Closure> closure = fn->closure;
  if (auto ic = closure.as<InterpretedClosure>()) {
    return ic->code();
  }
  return _Nil<T_O>();
}




string Function_O::__repr__() const {
  if (!(this->closure)) {
    return "Function_O::__repr__ NULL closure";
  }
  T_sp name = this->closure->name;
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString();
  ss << "/" << this->closure->describe();
  ss << " " << _rep_(name);
  ss << " lambda-list: " << _rep_(this->closure->lambdaList());
#if 0
  auto closure = this->closure;
  void* fptr = closure->functionAddress();
  if ( fptr!=NULL ) {
    ss << " :address " << fptr;
  }
#endif
  ss << ">";
  return ss.str();
}

#if defined(XML_ARCHIVE)
void Function_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  node->archiveWeakPointer("weakName", this->_WeakName);
}
#endif // defined(XML_ARCHIVE)





};
