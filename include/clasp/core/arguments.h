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
  inline bool _symbolP() const { return cl__symbolp(this->_ArgTarget); };
  Symbol_sp symbol() const;
  inline bool _lambdaListHandlerP() const { return core__lambda_list_handler_p(this->_ArgTarget); };
  LambdaListHandler_sp lambdaListHandler() const;
  inline bool _lambdaListP() const { return this->_ArgTarget.consp(); };
  List_sp lambda_list() const;
  inline bool targetIsLexical() const { return this->_ArgTargetFrameIndex != SPECIAL_TARGET; }
  virtual string asString() const;
};

class ArgumentWithDefault : public Argument {
public:
  typedef Argument Base;
  T_sp _Default;
  ArgumentWithDefault() : _Default(_Nil<T_O>()){};
  ArgumentWithDefault(T_sp target, T_sp def) : Argument(target), _Default(def){};
  string asString() const;
};

class RequiredArgument : public Argument {
public:
  typedef Argument Base;
  RequiredArgument(){};
  RequiredArgument(T_sp target) : Argument(target){};
  RequiredArgument(T_sp target, int frameIndex) : Argument(target) { this->_ArgTargetFrameIndex = frameIndex; };
  string asString() const;
};

class OptionalArgument : public ArgumentWithDefault {
public:
  typedef ArgumentWithDefault Base;
  Argument _Sensor;
  OptionalArgument(){};
  OptionalArgument(T_sp target, T_sp def, T_sp sensor) : ArgumentWithDefault(target, def), _Sensor(sensor){};
  string asString() const;
};

class RestArgument : public Argument {
public:
  bool VaRest;
  typedef Argument Base;
  explicit RestArgument() : Argument(), VaRest(false){};
  explicit RestArgument(T_sp target) : Argument(target), VaRest(false){};
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
  string asString() const;
};

class AuxArgument : public Argument {
public:
  typedef Argument Base;
  T_sp _Expression;
  AuxArgument() : Argument(_Nil<T_O>()), _Expression(_Nil<T_O>()){};
  AuxArgument(T_sp target, T_sp exp) : Argument(target), _Expression(exp){};
  string asString() const;
};

// Don't derive from gc::GCObject so that these classes won't be treated as Lisp classes by GC system - thi
// this will be a problem because some of the have smart_ptr's in them
class DynamicScopeManager : gctools::StackBoundClass {
private:
  Symbol_sp _OldVar;
  T_sp _OldBinding;
public:
  virtual void new_binding(const Argument &argument, T_sp val);
  virtual void va_rest_binding(const Argument &argument) { N_A_(); };
  virtual Vaslist &valist() { N_A_(); };
  virtual bool lexicalElementBoundP(const Argument &argument) { N_A_(); };
  inline void pushSpecialVariableAndSet_(Symbol_sp sym, T_sp val) {
    this->_OldVar = sym;
    this->_OldBinding = my_thread->bindings().push_binding(sym,&sym->_GlobalValue,val);
// NEW_DBS    sym->setf_symbolValue(val);
  }
  
  inline explicit DynamicScopeManager(Symbol_sp sym, T_sp newVal) {
    this->pushSpecialVariableAndSet_(sym, newVal);
  }

  void dump() const;

  virtual T_sp lexenv() const;

  virtual ~DynamicScopeManager() {
    DynamicBindingStack &bindings = my_thread->bindings();
    bindings.pop_binding(this->_OldVar,this->_OldBinding);
  }
};

struct SpecialBinding {
  T_sp _Var;
  T_sp _Val;
};

#define MAKE_SPECIAL_BINDINGS_HOLDER(_newnum_,_vla_,_num_) \
  size_t _newnum_ = _num_; \
  core::SpecialBinding _vla_[_num_];

struct ScopeManager {
  size_t          _NumberOfBindings;
  size_t          _NextBindingIndex;
  SpecialBinding* _Bindings;
  ScopeManager(size_t numberOfBindings, SpecialBinding* bindings) : _NumberOfBindings(numberOfBindings), _NextBindingIndex(0), _Bindings(bindings) {};
  ~ScopeManager();
  void new_special_binding(Symbol_sp var, T_sp val);
  virtual void new_binding(const Argument &argument, T_sp val) = 0;
  virtual T_sp lexenv() const = 0;
  virtual Vaslist &valist() { N_A_(); };
  virtual void va_rest_binding(const Argument &argument) { N_A_(); };
  virtual bool lexicalElementBoundP(const Argument &argument) { N_A_(); };
};

class ValueEnvironmentDynamicScopeManager : public ScopeManager {
private:
  ValueEnvironment_sp _Environment;
  Vaslist _VaRest;
public:
  ValueEnvironmentDynamicScopeManager(size_t numberOfBindings, SpecialBinding* bindings, ValueEnvironment_sp env) : ScopeManager(numberOfBindings,bindings), _Environment(env){};
public:
  /*! This is used for creating binds for lambda lists */
  virtual Vaslist &valist() { return this->_VaRest; };
  virtual void va_rest_binding(const Argument &argument);
  virtual void new_binding(const Argument &argument, T_sp val);
  void new_variable(List_sp classifiedVariable, T_sp val);
  void new_special(List_sp classifiedVariable);
  virtual bool lexicalElementBoundP(const Argument &argument);
  virtual T_sp lexenv() const { return this->_Environment; };
};

#if 0
class ActivationFrameDynamicScopeManager : public ScopeManager {
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
#endif

class StackFrameDynamicScopeManager : public ScopeManager {
private:
  gc::Frame &frame;

public:
  Vaslist VaRest;

public:
  StackFrameDynamicScopeManager(size_t numberOfBindings, SpecialBinding* bindings, gc::Frame* fP) : ScopeManager(numberOfBindings,bindings), frame(*fP) {};
public:
  virtual Vaslist &valist() { return this->VaRest; };
  virtual void va_rest_binding(const Argument &argument);
  virtual void new_binding(const Argument &argument, T_sp val);
  virtual bool lexicalElementBoundP(const Argument &argument);
  //  T_sp activationFrame() const;
  virtual T_sp lexenv() const;
};
};

#endif
