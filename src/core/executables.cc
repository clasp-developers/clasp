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
    NamedFunction_sp fn = sym->symbolFunction();
    return Values(fn->lambdaList(), _lisp->_true());
  } else if (NamedFunction_sp func = obj.asOrNull<NamedFunction_O>()) {
    return Values(func->lambdaList(), _lisp->_true());
  }
  return Values(_Nil<T_O>(), _Nil<T_O>());
}

CL_LAMBDA(function);
CL_DECLARE();
CL_DOCSTRING("functionSourcePosInfo");
CL_DEFUN gc::Nilable<SourcePosInfo_sp> core__function_source_pos_info(T_sp functionDesignator) {
  Closure_sp closure = coerce::closureDesignator(functionDesignator);
  gc::Nilable<SourcePosInfo_sp> sourcePosInfo = closure->sourcePosInfo();
  return sourcePosInfo;
}

CL_LAMBDA(fn kind);
CL_DECLARE();
CL_DOCSTRING("set the kind of a function object (:function|:macro)");
CL_DEFUN void core__set_kind(NamedFunction_sp fn, Symbol_sp kind) {
  fn->setKind(kind);
};



CL_DEFMETHOD void NamedFunction_O::setf_lambda_list(T_sp ll) {
  SUBIMP();
}

CL_LISPIFY_NAME("core:cleavir_ast");
CL_DEFMETHOD T_sp NamedFunction_O::cleavir_ast() const {
  return this->cleavir_ast();
}

CL_LISPIFY_NAME("core:setf_cleavir_ast");
CL_DEFMETHOD void NamedFunction_O::setf_cleavir_ast(T_sp ast) {
  this->setf_cleavir_ast(ast);
}

CL_LISPIFY_NAME("core:functionLambdaListHandler");
CL_DEFMETHOD T_sp NamedFunction_O::functionLambdaListHandler() const {
  return this->lambdaListHandler();
};
CL_LISPIFY_NAME("core:macrop");
CL_DEFMETHOD bool NamedFunction_O::macroP() const {
  return this->macroP();
}

CL_LISPIFY_NAME("core:setFunctionKind");
CL_DEFMETHOD void NamedFunction_O::setKind(Symbol_sp k) {
  this->setKind(k);
}
CL_LISPIFY_NAME("core:functionKind");
CL_DEFMETHOD Symbol_sp NamedFunction_O::functionKind() const {
  return this->getKind();
};

CL_LISPIFY_NAME("core:function_docstring");
CL_DEFMETHOD T_sp NamedFunction_O::docstring() const {
  return this->docstring();
};

CL_LISPIFY_NAME("core:function_declares");
CL_DEFMETHOD List_sp NamedFunction_O::declares() const {
  return this->declares();
};


CL_LISPIFY_NAME("core:functionSourcePos");
CL_DEFMETHOD T_mv NamedFunction_O::functionSourcePos() const {
  T_sp spi = this->sourcePosInfo();
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

void handleArgumentHandlingExceptions(Closure_sp closure) {
  NamedFunction_sp func = closure;
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
CL_DEFUN T_mv cl__function_lambda_expression(NamedFunction_sp fn) {
  T_sp code;
  code = core__function_lambda_list(fn);
  bool closedp = true; // fn->closedEnvironment().notnilp();
  T_sp name = fn->_name;
  return Values(code, _lisp->_boolean(closedp), name);
};

CL_LAMBDA(fn);
CL_DECLARE();
CL_DOCSTRING("functionSourceCode");
CL_DEFUN T_sp core__function_source_code(NamedFunction_sp fn) {
  if (auto ic = fn.as<InterpretedClosure_O>()) {
    return ic->code();
  }
  return _Nil<T_O>();
}




string NamedFunction_O::__repr__() const {
  T_sp name = this->_name;
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString();
  ss << "/" << this->describe();
  ss << " " << _rep_(name);
  ss << " lambda-list: " << _rep_(this->lambdaList());
  ss << ">";
  return ss.str();
}

#if defined(XML_ARCHIVE)
void NamedFunction_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  node->archiveWeakPointer("weakName", this->_WeakName);
}
#endif // defined(XML_ARCHIVE)





};
