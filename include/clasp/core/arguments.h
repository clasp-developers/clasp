#pragma once
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
  explicit Argument() : _ArgTarget(nil<T_O>()), _ArgTargetFrameIndex(UNDEFINED_TARGET) {}
  explicit Argument(T_sp target) : _ArgTarget(target), _ArgTargetFrameIndex(UNDEFINED_TARGET){};
  int targetFrameIndex() const { return this->_ArgTargetFrameIndex; }
  void clear() {
    _G();
    this->_ArgTarget = nil<T_O>();
    this->_ArgTargetFrameIndex = UNDEFINED_TARGET;
  }
  List_sp classified() const;
  inline bool isDefined() const { return (this->_ArgTarget) && (this->_ArgTarget.notnilp()); };
  inline bool _symbolP() const { return cl__symbolp(this->_ArgTarget); };
  Symbol_sp symbol() const;
  inline bool _lambdaListP() const { return this->_ArgTarget.consp(); };
  List_sp lambda_list() const;
  inline bool targetIsLexical() const { return this->_ArgTargetFrameIndex != SPECIAL_TARGET; }
  virtual string asString() const;
};

class ArgumentWithDefault : public Argument {
public:
  typedef Argument Base;
  T_sp _Default;
  ArgumentWithDefault() : _Default(nil<T_O>()){};
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
  KeywordArgument() : ArgumentWithDefault(), _Keyword(nil<T_O>()), _Sensor(){};
  KeywordArgument(T_sp keyword, T_sp target, T_sp def, T_sp sensor)
      : ArgumentWithDefault(target, def), _Keyword(keyword), _Sensor(sensor){};
  string asString() const;
};

class AuxArgument : public Argument {
public:
  typedef Argument Base;
  T_sp _Expression;
  AuxArgument() : Argument(nil<T_O>()), _Expression(nil<T_O>()){};
  AuxArgument(T_sp target, T_sp exp) : Argument(target), _Expression(exp){};
  string asString() const;
};

// Don't derive from gc::GCObject so that these classes won't be treated as Lisp classes by GC system - thi
// this will be a problem because some of the have smart_ptr's in them
class DynamicScopeManager {
private:
  VariableCell_sp _Cell;
  T_sp _OldBinding;

public:
  inline explicit DynamicScopeManager(VariableCell_sp cell, T_sp val) {
    _Cell = cell;
    _OldBinding = _Cell->bind(val);
  }
  // Compatibility
  inline explicit DynamicScopeManager(Symbol_sp sym, T_sp val) {
    _Cell = sym->ensureVariableCell();
    _OldBinding = _Cell->bind(val);
  }
  virtual ~DynamicScopeManager() { _Cell->unbind(_OldBinding); }
  // used in unwind.h
  inline T_sp oldBinding() { return _OldBinding; }
};

}; // namespace core
