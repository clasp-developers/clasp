/*
    File: arguments.h
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
#ifndef _core_arguments_H
#define _core_arguments_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbol.h>

namespace core {

#define SUB_LAMBDA_LIST -3
#define SPECIAL_TARGET -2
#define UNDEFINED_TARGET -1
class Argument {
public:
  T_sp _ArgTarget;
  int _ArgTargetFrameIndex;
  explicit Argument() : _ArgTarget(_Nil<T_O>()), _ArgTargetFrameIndex(UNDEFINED_TARGET) {}
  explicit Argument(T_sp target) : _ArgTarget(target), _ArgTargetFrameIndex(UNDEFINED_TARGET){};
  DECLARE_onHeapScanGCRoots();
  int targetFrameIndex() const {
    return this->_ArgTargetFrameIndex;
  }
  void clear() {
    _G();
    this->_ArgTarget = _Nil<T_O>();
    this->_ArgTargetFrameIndex = UNDEFINED_TARGET;
  }
  List_sp classified() const;
  inline bool isDefined() const { return (this->_ArgTarget) && (this->_ArgTarget.notnilp()); };
  inline bool _symbolP() const { return cl_symbolp(this->_ArgTarget); };
  Symbol_sp symbol() const;
  inline bool _lambdaListHandlerP() const { return af_lambda_list_handler_p(this->_ArgTarget); };
  LambdaListHandler_sp lambdaListHandler() const;
  inline bool _lambdaListP() const { return cl_consp(this->_ArgTarget); };
  List_sp lambdaList() const;
  inline bool targetIsLexical() const { return this->_ArgTargetFrameIndex != SPECIAL_TARGET; }
  virtual string asString() const;
};

class ArgumentWithDefault : public Argument {
public:
  typedef Argument Base;
  T_sp _Default;
  ArgumentWithDefault() : _Default(_Nil<T_O>()){};
  ArgumentWithDefault(T_sp target, T_sp def) : Argument(target), _Default(def){};
  DECLARE_onHeapScanGCRoots();
  string asString() const;
};

class RequiredArgument : public Argument {
public:
  typedef Argument Base;
  RequiredArgument(){};
  RequiredArgument(T_sp target) : Argument(target){};
  RequiredArgument(T_sp target, int frameIndex) : Argument(target) { this->_ArgTargetFrameIndex = frameIndex; };
  DECLARE_onHeapScanGCRoots();
  string asString() const;
};

class OptionalArgument : public ArgumentWithDefault {
public:
  typedef ArgumentWithDefault Base;
  Argument _Sensor;
  OptionalArgument(){};
  OptionalArgument(T_sp target, T_sp def, T_sp sensor) : ArgumentWithDefault(target, def), _Sensor(sensor){};
  DECLARE_onHeapScanGCRoots();
  string asString() const;
};

class RestArgument : public Argument {
public:
  bool VaRest;
  typedef Argument Base;
  explicit RestArgument() : Argument(), VaRest(false){};
  explicit RestArgument(T_sp target) : Argument(target), VaRest(false){};
  DECLARE_onHeapScanGCRoots();
  void setTarget(T_sp target) { this->_ArgTarget = target; };
  string asString() const;
};

class KeywordArgument : public ArgumentWithDefault {
public:
  typedef ArgumentWithDefault Base;
  T_sp _Keyword;
  Argument _Sensor;
  KeywordArgument() : ArgumentWithDefault(), _Keyword(_Nil<T_O>()), _Sensor(){};
  KeywordArgument(T_sp keyword, T_sp target, T_sp def, T_sp sensor) : ArgumentWithDefault(target, def), _Keyword(keyword), _Sensor(sensor){};
  DECLARE_onHeapScanGCRoots();
  string asString() const;
};

class AuxArgument : public Argument {
public:
  typedef Argument Base;
  T_sp _Expression;
  AuxArgument() : Argument(_Nil<T_O>()), _Expression(_Nil<T_O>()){};
  AuxArgument(T_sp target, T_sp exp) : Argument(target), _Expression(exp){};
  DECLARE_onHeapScanGCRoots();
  string asString() const;
};

// Don't derive from gc::GCObject so that these classes won't be treated as Lisp classes by GC system - thi
// this will be a problem because some of the have smart_ptr's in them
class DynamicScopeManager : gctools::StackBoundClass {
private:
  int _beginTop;
  int _endTop;

public:
  virtual void new_binding(const Argument &argument, T_sp val);
  virtual void va_rest_binding(const Argument &argument) { N_A_(); };
  virtual VaList_S &valist() { N_A_(); };
  virtual bool lexicalElementBoundP(const Argument &argument) { N_A_(); };
  void pushSpecialVariableAndSet(Symbol_sp sym, T_sp val);
  inline explicit DynamicScopeManager() {
    int top = _lisp->bindings().top();
    this->_beginTop = top;
    this->_endTop = top;
  }

  inline explicit DynamicScopeManager(Symbol_sp sym, T_sp newVal) {
    int top = _lisp->bindings().top();
    this->_beginTop = top;
    this->_endTop = top;
    this->pushSpecialVariableAndSet(sym, newVal);
  }

  void dump() const;

  virtual T_sp lexenv() const;

  virtual ~DynamicScopeManager() {
    DynamicBindingStack &bindings = _lisp->bindings();
    int numBindings = this->_endTop - this->_beginTop;
    for (int i = 0; i < numBindings; ++i) {
      bindings.pop();
    }
  }
};

class ValueEnvironmentDynamicScopeManager : public DynamicScopeManager {
private:
  ValueEnvironment_sp _Environment;

public:
  ValueEnvironmentDynamicScopeManager(ValueEnvironment_sp env) : _Environment(env){};

public:
  /*! This is used for creating binds for lambda lists */
  virtual void new_binding(const Argument &argument, T_sp val);
  void new_variable(List_sp classifiedVariable, T_sp val);
  void new_special(List_sp classifiedVariable);
  virtual bool lexicalElementBoundP(const Argument &argument);
  virtual T_sp lexenv() const { return this->_Environment; };
};

class ActivationFrameDynamicScopeManager : public DynamicScopeManager {
private:
  ActivationFrame_sp _Frame;

public:
  ActivationFrameDynamicScopeManager(ActivationFrame_sp frame) : _Frame(frame){};

public:
  virtual void new_binding(const Argument &argument, T_sp val);
  virtual bool lexicalElementBoundP(const Argument &argument);
  T_sp activationFrame() const { return this->_Frame; };
  virtual T_sp lexenv() const;
};

class StackFrameDynamicScopeManager : public DynamicScopeManager {
private:
  gc::frame::Frame &frame;

public:
  VaList_S VaRest;

public:
  StackFrameDynamicScopeManager(gc::frame::Frame &f) : frame(f){};

public:
  virtual VaList_S &valist() { return this->VaRest; };
  virtual void va_rest_binding(const Argument &argument);
  virtual void new_binding(const Argument &argument, T_sp val);
  virtual bool lexicalElementBoundP(const Argument &argument);
  //  T_sp activationFrame() const;
  virtual T_sp lexenv() const;
};
};

#endif
