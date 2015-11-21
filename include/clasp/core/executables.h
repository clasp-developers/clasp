/*
    File: executables.h
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
#ifndef _core_executables_H //(
#define _core_executables_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
//#i n c l u d e "macro.h"
#include <clasp/core/cons.h>
#include <clasp/core/symbol.h>
#include <clasp/core/str.fwd.h>
#include <clasp/core/activationFrame.fwd.h>
#include <clasp/core/package.fwd.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/lambdaListHandler.fwd.h>
#include <clasp/core/lispDefinitions.h>

namespace core {

#if 0
class FunctionClosure : public Closure {
public:
  T_sp _SourcePosInfo;
  Symbol_sp kind;

public:
  DISABLE_NEW();
  FunctionClosure(T_sp name, T_sp spo, Symbol_sp k, T_sp env)
      : Closure(name, env), _SourcePosInfo(spo), kind(k){};
  FunctionClosure(T_sp name)
      : Closure(name, _Nil<T_O>()), _SourcePosInfo(_Nil<T_O>()), kind(kw::_sym_function){};

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  virtual const char *describe() const { return "SingleDispatchGenericFunctoid"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {SIMPLE_ERROR(BF("Subclass must implement"));};
  void setKind(Symbol_sp k) { this->kind = k; };
  Symbol_sp getKind() const { return this->kind; };
  bool macroP() const;
  T_sp sourcePosInfo() const { return this->_SourcePosInfo; };
  T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
  virtual int sourceFileInfoHandle() const;
  virtual size_t filePos() const;
  virtual int lineNumber() const;
  virtual int column() const;
};
#endif
extern void handleArgumentHandlingExceptions(gctools::tagged_pointer<Closure>);
};

namespace core {
SMART(LambdaListHandler);
SMART(Function);
class Function_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, Function_O, "Function");

#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif  // defined(XML_ARCHIVE)
public: // was protected:
  gctools::tagged_pointer<Closure> closure;

public:
  Function_O() : Base(), closure(){};
  virtual ~Function_O(){};

public:
  static Function_sp make(gctools::tagged_pointer<Closure> c) {
    GC_ALLOCATE(Function_O, f);
    f->closure = c;
    return f;
  }

public:
  string __repr__() const;
  string description() const { return "Function::description"; };

  virtual bool macroP() const;
  virtual void setKind(Symbol_sp k);
  virtual Symbol_sp functionKind() const;
  T_sp functionLambdaListHandler() const;
  /*! Return (values lambda-list foundp) */
  T_mv lambdaList();
  T_sp closedEnvironment() const;
  List_sp functionDeclares() const;
  T_sp functionName() const;
  T_mv functionSourcePos() const;
  T_sp cleavir_ast() const;
  virtual void setf_cleavir_ast(T_sp ast);
  List_sp declares() const;
  T_sp docstring() const;
};
};
template <>
struct gctools::GCInfo<core::Function_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
TRANSLATE(core::Function_O);

namespace core {
class InterpretedClosure : public FunctionClosure {
public:
  LambdaListHandler_sp _lambdaListHandler;
  List_sp _declares;
  T_sp _docstring;
  List_sp _code;

public:
  DISABLE_NEW();
  InterpretedClosure(T_sp fn, Symbol_sp k, LambdaListHandler_sp llh, List_sp dec, T_sp doc, T_sp e, List_sp c, SOURCE_INFO);
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "InterpretedClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  bool interpretedP() const { return true; };
  T_sp docstring() const { return this->_docstring; };
  List_sp declares() const { return this->_declares; };
  List_sp code() const { return this->_code; };
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
  T_sp lambdaList() const;
};

#if 0
class BuiltinClosure : public FunctionClosure {
public:
  LambdaListHandler_sp _lambdaListHandler;

public:
  DISABLE_NEW();
  BuiltinClosure(T_sp name, T_sp sp, Symbol_sp k)
      : FunctionClosure(name, sp, k, _Nil<T_O>()){};
  BuiltinClosure(T_sp name)
      : FunctionClosure(name) {}
  void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
    this->_lambdaListHandler = llh;
    this->kind = k;
  }
  virtual T_sp lambdaList() const;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "BuiltinClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  bool builtinP() const { return true; };
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
};
#endif
};

namespace core {
SMART(LambdaListHandler);
SMART(Function);
class CompiledFunction_O : public Function_O {
  LISP_BASE1(Function_O);
  LISP_CLASS(core, ClPkg, CompiledFunction_O, "CompiledFunction");

#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
public:
  CompiledFunction_O() : Base(){};
  virtual ~CompiledFunction_O(){};

public:
  static CompiledFunction_sp make(gctools::tagged_pointer<Closure> c) {
    GC_ALLOCATE(CompiledFunction_O, f);
    f->closure = c;
    //            printf("%s:%d Returning CompiledFunction_sp func=%p &f=%p\n", __FILE__, __LINE__, f.px_ref(), &f);
    return f;
  }

public:
};
};
template <>
struct gctools::GCInfo<core::CompiledFunction_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
TRANSLATE(core::CompiledFunction_O);

#endif //)
