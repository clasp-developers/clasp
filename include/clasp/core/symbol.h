#pragma once
/*
    File: symbol.h
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

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/array.h>
#include <clasp/core/function.h>
#include <clasp/core/environment.fwd.h>
// #include <clasp/core/lisp.h>

#define KW(s) (_lisp->internKeyword(s))

#define IS_SPECIAL 0x01
#define IS_CONSTANT 0x02
#define IS_MACRO 0x04

template <> struct gctools::GCInfo<core::VariableCell_O> {
  static bool constexpr NeedsInitialization = false;
#ifdef CLASP_THREADS
  // Gotta release the binding index.
  static bool constexpr NeedsFinalization = true;
#else
  static bool constexpr NeedsFinalization = false;
#endif
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::Symbol_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

//
// A special class used to invoke a Symbol_O constructor
// for startup symbols.
//
struct only_at_startup {};

#define NO_THREAD_LOCAL_BINDINGS std::numeric_limits<uint32_t>::max()
SMART(Package);
FORWARD(ClassHolder);

// FIXME: Not the best place for this class, probably.
FORWARD(VariableCell);
class VariableCell_O : public General_O {
  LISP_CLASS(core, CorePkg, VariableCell_O, "VariableCell", General_O);

public:
  VariableCell_O(T_sp name) : _GlobalValue(unbound<T_O>()), _BindingIdx(NO_THREAD_LOCAL_BINDINGS), _Name(name) {}
#ifdef CLASP_THREADS
  virtual ~VariableCell_O() {
    uint32_t idx = bindingIndex();
    if (idx != NO_THREAD_LOCAL_BINDINGS)
      my_thread->_Bindings.release_binding_index(idx);
  }
#endif
public:
  std::atomic<T_sp> _GlobalValue;
  mutable std::atomic<uint32_t> _BindingIdx;
  T_sp _Name; // used for error messages and printing only
public:
  CL_LISPIFY_NAME(VariableCell/make)
  CL_DEF_CLASS_METHOD
  static VariableCell_sp make(T_sp name);

private:
  inline uint32_t bindingIndex() const { return _BindingIdx.load(std::memory_order_relaxed); }
  [[noreturn]] void unboundError() const;

public:
  inline T_sp name() const { return this->_Name; }
  inline T_sp globalValueUnsafe() const { return _GlobalValue.load(std::memory_order_relaxed); }
  inline T_sp globalValueUnsafeSeqCst() const { return _GlobalValue.load(); }
  inline void set_globalValue(T_sp val) { _GlobalValue.store(val, std::memory_order_relaxed); }
  inline void set_globalValueSeqCst(T_sp val) { _GlobalValue.store(val); }
  inline T_sp cas_globalValueSeqCst(T_sp cmp, T_sp val) {
    _GlobalValue.compare_exchange_strong(cmp, val);
    return cmp;
  }

  // Make sure the binding index is coherent.
  // This is used when doing local special bindings.
  // Hypothetically, we could do it even earlier at load time,
  // but only when a cell is actually bound and not just global.
  uint32_t ensureBindingIndex() const;

  // Return the value, or UNBOUND if unbound.
  CL_LISPIFY_NAME(VariableCell/ValueUnsafe)
  CL_DEFMETHOD
  T_sp valueUnsafe() const {
#ifdef CLASP_THREADS
    uint32_t index = this->_BindingIdx.load(std::memory_order_relaxed);
    auto& bindings = my_thread->_Bindings;
    if (bindings.thread_local_boundp(index))
      return bindings.thread_local_value(index);
    else
#endif
      return globalValueUnsafe();
  }
  T_sp valueUnsafeSeqCst() const {
#ifdef CLASP_THREADS
    uint32_t index = this->_BindingIdx.load(std::memory_order_relaxed);
    auto& bindings = my_thread->_Bindings;
    if (bindings.thread_local_boundp(index))
      return bindings.thread_local_value(index);
    else
#endif
      return globalValueUnsafeSeqCst();
  }
  CL_LISPIFY_NAME(VariableCell/boundp)
  CL_DEFMETHOD
  bool boundP() const { return !(valueUnsafe().unboundp()); }

  // Return the value or signal an error if unbound.
  CL_LISPIFY_NAME(VariableCell/value)
  CL_DEFMETHOD
  T_sp value() const {
    T_sp val = valueUnsafe();
    if (val.unboundp())
      unboundError();
    else
      return val;
  }
  void set_value(T_sp value) {
#ifdef CLASP_THREADS
    uint32_t index = _BindingIdx.load(std::memory_order_relaxed);
    auto& bindings = my_thread->_Bindings;
    if (bindings.thread_local_boundp(index))
      bindings.set_thread_local_value(value, index);
    else
#endif
      set_globalValue(value);
  }
  inline T_sp globalValue() const {
    T_sp val = globalValueUnsafe();
    if (val.unboundp())
      unboundError();
    else
      return val;
  }
  CL_LISPIFY_NAME(VariableCell/makunbound)
  CL_DEFMETHOD
  void makunbound() { set_value(unbound<T_O>()); }
  inline T_sp valueSeqCst() const {
    T_sp val = valueUnsafeSeqCst();
    if (val.unboundp())
      unboundError();
    else
      return val;
  }
  void set_valueSeqCst(T_sp value) {
#ifdef CLASP_THREADS
    uint32_t index = _BindingIdx.load(std::memory_order_relaxed);
    auto& bindings = my_thread->_Bindings;
    if (bindings.thread_local_boundp(index))
      bindings.set_thread_local_value(value, index);
    else
#endif
      set_globalValueSeqCst(value);
  }
  T_sp cas_valueSeqCst(T_sp cmp, T_sp new_value) {
#ifdef CLASP_THREADS
    uint32_t index = this->_BindingIdx.load(std::memory_order_relaxed);
    auto& bindings = my_thread->_Bindings;
    if (bindings.thread_local_boundp(index)) {
      // Not actually atomic, since local bindings are only
      // accessible within their thread. For now at least.
      T_sp actual = bindings.thread_local_value(index);
      if (actual == cmp) {
        bindings.set_thread_local_value(new_value, index);
        return actual;
      } else
        return cmp;
    } else
#endif
      return cas_globalValueSeqCst(cmp, new_value);
  }
  // Used by DynamicScopeManager.
  // Give the cell a new thread local value and return the old value,
  // which may be an unboundedness marker.
  T_sp bind(T_sp nval) {
    auto& bindings = my_thread->_Bindings;
    uint32_t index = ensureBindingIndex();
    T_sp oval = bindings.thread_local_value(index);
    bindings.set_thread_local_value(nval, index);
    return oval;
  }
  void unbind(T_sp oval) {
    // If this always follows a bind call, the _BindingIdx has already
    // been ensured so we don't need to check again.
    uint32_t index = _BindingIdx.load(std::memory_order_relaxed);
    my_thread->_Bindings.set_thread_local_value(oval, index);
  }

public:
  virtual void __write__(T_sp stream) const; // in write_ugly.cc
  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    // Reset the _BindingIdx (erasing any local bindings).
    if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp)
      _BindingIdx.store(NO_THREAD_LOCAL_BINDINGS);
  }
};

FORWARD(Symbol);
class Symbol_O : public General_O {
public:
  // This MUST match the layout for %symbol% in cmpintrinsics.lisp and the sanity check core__verify_symbol_layout
  SimpleString_sp _Name;                       // offset 8
  std::atomic<T_sp> _HomePackage;              // offset=16 NIL or Package
  mutable std::atomic<VariableCell_sp> _Value; // offset=24
  std::atomic<FunctionCell_sp> _Function;      // offset=32
  std::atomic<FunctionCell_sp> _SetfFunction;  // offset=40
  std::atomic<uint32_t> _Flags;
  std::atomic<T_sp> _PropertyList;

  friend class Instance_O;
  friend class Package_O;
  friend class CoreExposer;
  friend void core__verify_symbol_layout(T_sp);
  LISP_CLASS(core, ClPkg, Symbol_O, "Symbol", General_O);

public:
  /*! Create a Symbol that doesn't have a package or Metaclass defined */
  //	static Symbol_sp create_classless_packageless(string const& name);

  /*! Only used when creating special symbols at boot time.
          Before NIL, UNBOUND etc are defined */
  static Symbol_sp create_from_string(const string& nm);
  static Symbol_sp create(SimpleString_sp snm) {
    // This is used to allocate roots that are pointed
    // to by global variable _sym_XXX  and will never be collected
    Symbol_sp n = gctools::GC<Symbol_O>::allocate(only_at_startup());
    n->setf_name(snm);
    //    ASSERTF(nm != "", "You cannot create a symbol without a name");
    return n;
  };

public:
public:
  string formattedName(bool prefixAlways) const;
  string safeFormattedName() const;
  ;

private:
  inline uint32_t getFlags() const { return _Flags.load(std::memory_order_relaxed); }
  inline void setFlag(bool flag, uint32_t n) {
    if (flag)
      _Flags.fetch_or(n, std::memory_order_relaxed);
    else
      _Flags.fetch_and(~n, std::memory_order_relaxed);
  }

public: // Flags access
  bool macroP() const { return !!(getFlags() & IS_MACRO); };
  void setf_macroP(bool m) { setFlag(m, IS_MACRO); }
  bool getReadOnly() const { return !!(getFlags() & IS_CONSTANT); }
  void setReadOnly(bool m) { setFlag(m, IS_CONSTANT); }
  bool specialP() const { return !!(getFlags() & IS_SPECIAL); };
  void setf_specialP(bool m) { setFlag(m, IS_SPECIAL); }
  void makeSpecial(); // TODO: Redundant, remove?
public:               // Hashing
  void sxhash_equal(HashGenerator& hg) const override;
  void sxhash_equalp(HashGenerator& hg) const override { this->sxhash_equal(hg); };

public: // Miscellaneous
  bool isKeywordSymbol();
  Symbol_sp asKeywordSymbol();

  void setf_name(SimpleString_sp nm) { this->_Name = nm; };

#ifdef SYMBOL_CLASS
  void setf_find_class(T_sp class_);
  T_sp find_class();
  ClassHolder_sp find_class_holder();
#endif
  List_sp plist() const { return gc::As_unsafe<List_sp>(_PropertyList.load(std::memory_order_relaxed)); }
  void setf_plist(List_sp plist) { _PropertyList.store(plist, std::memory_order_relaxed); }
  List_sp atomic_plist() const { return gc::As_unsafe<List_sp>(_PropertyList.load(std::memory_order_seq_cst)); }
  void atomic_setf_plist(List_sp plist) { _PropertyList.store(plist, std::memory_order_seq_cst); }
  List_sp cas_plist(List_sp cmp, List_sp new_plist) {
    T_sp tcmp = cmp;
    T_sp tnew_plist = new_plist;
    this->_PropertyList.compare_exchange_strong(tcmp, tnew_plist);
    return gc::As<List_sp>(tcmp);
  }

  Symbol_sp copy_symbol(T_sp copy_properties) const;
  bool isExported();

public: // value slot access
  inline VariableCell_sp variableCell() const { return _Value.load(std::memory_order_relaxed); }
  VariableCell_sp ensureVariableCell();

  /*! Return the value slot of the symbol - throws if unbound */
  T_sp symbolValue() const;
  T_sp atomicSymbolValue() const;
  void setf_symbolValue(T_sp obj);
  void set_atomicSymbolValue(T_sp nv);
  T_sp casSymbolValue(T_sp cmp, T_sp new_value);
  bool boundP() const;

  T_sp globalSymbolValue() const;
  void set_globalSymbolValue(T_sp nv);

  void makunbound();

  T_sp defparameter(T_sp obj);
  T_sp defconstant(T_sp obj);

public: // function value slots access
  inline FunctionCell_sp functionCell() const { return _Function.load(std::memory_order_relaxed); }
  inline FunctionCell_sp setfFunctionCell() const { return _SetfFunction.load(std::memory_order_relaxed); }
  inline void functionCellSet(FunctionCell_sp f) { _Function.store(f, std::memory_order_relaxed); }
  inline void setfFunctionCellSet(FunctionCell_sp f) { _SetfFunction.store(f, std::memory_order_relaxed); }
  FunctionCell_sp ensureFunctionCell();
  FunctionCell_sp ensureFunctionCell(Function_sp init);
  FunctionCell_sp ensureSetfFunctionCell();
  FunctionCell_sp ensureSetfFunctionCell(Function_sp init);

  void fmakunbound();

  void setSetfFdefinition(Function_sp fn);
  Function_sp getSetfFdefinition() const;
  bool fboundp_setf() const;
  void fmakunbound_setf();

  /*! Set the global function value of this symbol */
  void setf_symbolFunction(Function_sp exec);

  /*! Return the global bound function */
  Function_sp symbolFunction() const;

  /*! Return true if the symbol has a function bound*/
  bool fboundp() const;

  // These can be used when the result is going to be called immediately.
  // They don't check for fboundedness, because if un-fbound the cell
  // will just signal an error when it is called.
  inline Function_sp symbolFunctionCalled() { return ensureFunctionCell(); }
  inline Function_sp getSetfFdefinitionCalled() { return ensureSetfFunctionCell(); }

public: // packages, the name, misc
  string symbolNameAsString() const;

  SimpleString_sp symbolName() const { return this->_Name; };

  T_sp getPackage() const;
  T_sp homePackage() const { return this->getPackage(); }
  void setPackage(T_sp p);

  /*! Return the name of the symbol with the package prefix
   * unless this symbol is in the current package
   */
  string currentName() const;

  /*! Return the name of the symbol with the package prefix
   * always attached
   */
  string fullName() const;

  /*! Convenience function, export yourself and return yourself */
  Symbol_sp exportYourself(bool doit = true);

  // Error if our package is locked. fmt takes one argument (the sym)
  void check_package_lock(const char* fmt);

  void dump() override;

  void __write__(T_sp stream) const override; // in write_symbol.cc

  string __repr__() const override;

public: // ctor/dtor for classes with shared virtual base
  /*! Special constructor used when starting up the Lisp environment */
  explicit Symbol_O(const only_at_startup&);
  explicit Symbol_O(SimpleBaseString_sp name)
      : _Name(name), _Value(unbound<VariableCell_O>()), _Function(unbound<FunctionCell_O>()),
        _SetfFunction(unbound<FunctionCell_O>()){};

  /*! Used to finish setting up symbol when created with the above constructor */
  void finish_setup(Package_sp pkg, bool exportp, bool shadowp);

  /*! Return -1, 0, 1 if this is <, ==, > other by name */
  inline int order(core::Symbol_O other) {
    if (this->symbolNameAsString() <= other.symbolNameAsString()) {
      if (this->symbolNameAsString() == other.symbolNameAsString()) {
        return 0;
      }
      return -1;
    }
    return 1;
  }

  bool operator<(core::Symbol_O other) { return this->symbolNameAsString() < other.symbolNameAsString(); }

  void remove_package(Package_sp pkg);

public:
  explicit Symbol_O();
};

T_sp cl__symbol_value(const Symbol_sp sym);
SimpleString_sp cl__symbol_name(Symbol_sp sym);
T_sp cl__symbol_package(Symbol_sp sym);
Function_sp cl__symbol_function(Symbol_sp sym);
bool cl__boundp(Symbol_sp sym);
}; // namespace core

namespace core {
/*! This is used for SmallMultiMap<core::Symbol_sp,XXXX> */
struct SymbolComparer {
  static int order(Symbol_sp a, Symbol_sp b) {
    if (a->symbolNameAsString() <= b->symbolNameAsString()) {
      if (a->symbolNameAsString() == b->symbolNameAsString())
        return 0;
      return -1;
    }
    return 1;
  }
};
}; // namespace core
