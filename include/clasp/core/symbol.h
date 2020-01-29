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

#ifndef Symbol_H //[
#define Symbol_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/array.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/environment.fwd.h>
//#include <clasp/core/lisp.h>

#define KW(s) (_lisp->internKeyword(s))

#define IS_SPECIAL  0x01
#define IS_CONSTANT 0x02
#define IS_MACRO    0x04

namespace core {
#define NO_THREAD_LOCAL_BINDINGS std::numeric_limits<uint32_t>::max()
SMART(Package);
SMART(NamedFunction);
FORWARD(ClassHolder);

FORWARD(Symbol);
class Symbol_O : public General_O {
  struct metadata_bootstrap_class {};
  struct metadata_gc_do_not_move {};

 public: // FIXME: Probably oughta be private.
  // This MUST match the layout for %sym% in cmpintrinsics.lsp and the sanity check core__verify_symbol_layout
  SimpleString_sp _Name;
  std::atomic<T_sp> _HomePackage; // NIL or Package
  T_sp _GlobalValue;
  Function_sp _Function;
  Function_sp _SetfFunction;
  mutable std::atomic<uint32_t> _BindingIdx;
  uint32_t  _Flags;
  List_sp   _PropertyList;

  friend class Instance_O;
  friend class Package_O;
  friend class CoreExposer;
  friend void core__verify_symbol_layout(T_sp);
  LISP_CLASS(core, ClPkg, Symbol_O, "Symbol",General_O);

public:
  /*! Create a Symbol that doesn't have a package or Metaclass defined */
  //	static Symbol_sp create_classless_packageless(string const& name);

  /*! Only used when creating special symbols at boot time.
	  Before NIL, UNBOUND etc are defined */
  static Symbol_sp create_at_boot(const string &nm);
  static Symbol_sp create_from_string(const string &nm);
  static Symbol_sp create(SimpleString_sp snm) {
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
    Symbol_sp n = gctools::GC<Symbol_O>::root_allocate(true);
    n->setf_name(snm);
    n->fmakunbound();
    n->fmakunbound_setf();
//    ASSERTF(nm != "", BF("You cannot create a symbol without a name"));
    return n;
  };
public:
  string formattedName(bool prefixAlways) const;
public:
  void sxhash_(HashGenerator &hg) const;
  void sxhash_equal(HashGenerator &hg) const;
  void sxhash_equalp(HashGenerator &hg) const {this->sxhash_equal(hg);};

 public: // Flags and miscellaneous
  bool isKeywordSymbol();
  Symbol_sp asKeywordSymbol();

  bool macroP() const { return !!(this->_Flags&IS_MACRO);};
  void setf_macroP(bool m) {
    if (m) this->_Flags = this->_Flags|IS_MACRO;
    else this->_Flags = this->_Flags&(~IS_MACRO);
  }

  void setf_name(SimpleString_sp nm) { this->_Name = nm; };

#ifdef SYMBOL_CLASS
  void setf_find_class(T_sp class_);
  T_sp find_class();
  ClassHolder_sp find_class_holder();
#endif
  List_sp plist() const { return _PropertyList; }
  void setf_plist(List_sp plist) { _PropertyList = plist; }
  
  bool getReadOnly() const { return !!(this->_Flags&IS_CONSTANT);};
  void setReadOnly(bool m) {
    if (m) this->_Flags = this->_Flags|IS_CONSTANT;
    else this->_Flags = this->_Flags&(~IS_CONSTANT);
  }

  /*! Return true if the symbol is dynamic/special */
  bool specialP() const { return !!(this->_Flags&IS_SPECIAL);};
  void setf_specialP(bool m) {
    if (m) this->_Flags = this->_Flags|IS_SPECIAL;
    else this->_Flags = this->_Flags&(~IS_SPECIAL);
  }
  void makeSpecial(); // TODO: Redundant, remove?

  Symbol_sp copy_symbol(T_sp copy_properties) const;
  bool isExported();

  void symbolUnboundError() const;

 public: // value slot access

  inline T_sp globalValue() const { return _GlobalValue; }
  inline void set_globalValue(T_sp val) { _GlobalValue = val; }
  
  inline T_sp threadLocalSymbolValue() const {
#ifdef CLASP_THREADS
    return my_thread->_Bindings.thread_local_value(this);
#else
    return _GlobalValue;
#endif
  }

  inline void set_threadLocalSymbolValue(T_sp value) {
#ifdef CLASP_THREADS
    my_thread->_Bindings.set_thread_local_value(value, this);
#else
    _GlobalValue = value;
#endif
  }

  /*! Return the value slot of the symbol or UNBOUND if unbound */
  inline T_sp symbolValueUnsafe() const {
#ifdef CLASP_THREADS
    if (my_thread->_Bindings.thread_local_boundp(this))
      return my_thread->_Bindings.thread_local_value(this);
    else
#endif
      return _GlobalValue;
  };
  
  /*! Return the value slot of the symbol - throws if unbound */
  inline T_sp symbolValue() const {
    T_sp val = symbolValueUnsafe();
    if (val.unboundp()) this->symbolUnboundError();
    return val;
  }

  inline T_sp symbolValueFromCell(Cons_sp cell, T_sp unbound_marker) const {
    T_sp val = symbolValueUnsafe();
    if (val.unboundp()) val = CONS_CAR(cell);
    // FIXME: SICL allows many unbound values, but we don't even pick one properly,
    // i.e. we just check for both rather than checking TLS.unboundp() and global.eq(marker).
    if (val.unboundp() || val == unbound_marker) this->symbolUnboundError();
    return val;
  }

  inline bool boundP() const { return !(symbolValueUnsafe().unboundp()); };

  inline bool boundPFomCell(Cons_sp cell) {
    T_sp val = symbolValueUnsafe();
    if (val.unboundp()) val = CONS_CAR(cell);
    return !(val.unboundp());
  }

  Symbol_sp makunbound();
  //Symbol_sp makunboundFromCell(Cons_sp cell);

  T_sp defparameter(T_sp obj);
  T_sp defconstant(T_sp obj);

  inline T_sp setf_symbolValue(T_sp obj) {
#ifdef CLASP_THREADS
    if (my_thread->_Bindings.thread_local_boundp(this))
      set_threadLocalSymbolValue(obj);
    else
#endif
      _GlobalValue = obj;
    return obj;
  }

  inline T_sp setf_symbolValueFromCell(T_sp val, Cons_sp cell) {
#ifdef CLASP_THREADS
    if (my_thread->_Bindings.thread_local_boundp(this))
      set_threadLocalSymbolValue(val);
    else
#endif
      CONS_CAR(cell) = val;
    return val;
  }

 public: // function value slots access

  void fmakunbound();
  
  void setSetfFdefinition(Function_sp fn) { this->_SetfFunction = fn; };
  inline Function_sp getSetfFdefinition() { return this->_SetfFunction; };
  bool fboundp_setf() const;
  void fmakunbound_setf();
  

  /*! Set the global function value of this symbol */
  void setf_symbolFunction(Function_sp exec);

  /*! Return the global bound function */
  inline Function_sp symbolFunction() const { return this->_Function; };

  /*! Return true if the symbol has a function bound*/
  bool fboundp() const;

 public: // packages, the name, misc

  string symbolNameAsString() const;

  SimpleString_sp symbolName() const { return this->_Name; };

  T_sp getPackage() const;
  T_sp homePackage() const { return this->getPackage(); };
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

  void dump();

  void __write__(T_sp stream) const; // in write_symbol.cc

  string __repr__() const;

public: // ctor/dtor for classes with shared virtual base
  /*! Special constructor used when starting up the Lisp environment */
  explicit Symbol_O(bool dummy); // string const &name);
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

  bool operator<(core::Symbol_O other) {
    return this->symbolNameAsString() < other.symbolNameAsString();
  }

  void remove_package(Package_sp pkg);
public:
  explicit Symbol_O();
  virtual ~Symbol_O(){
#ifdef CLASP_THREAD
    if (this->_BindingIdx.load() != NO_THREAD_LOCAL_BINDINGS) {
      my_thread->_Bindings.release_binding_index(this->_BindingIdx.load());
    }
#endif
  };
};

T_sp cl__symbol_value(const Symbol_sp sym);
SimpleString_sp cl__symbol_name(Symbol_sp sym);
T_sp cl__symbol_package(Symbol_sp sym);
Function_sp cl__symbol_function(Symbol_sp sym);
bool cl__boundp(Symbol_sp sym);
};

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
};

template <>
struct gctools::GCInfo<core::Symbol_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif //]
