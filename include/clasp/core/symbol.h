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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/environment.fwd.h>
#include <clasp/core/lisp.h>

#define KW(s) (_lisp->internKeyword(s))

namespace core {

SMART(Package);
SMART(Function);

FORWARD(Symbol);
class Symbol_O : public T_O {
  struct metadata_bootstrap_class {};
  struct metadata_gc_do_not_move {};

public:
  Str_sp _Name;
  T_sp _HomePackage; // NIL or Package
  T_sp _Value;
  T_sp _Function;
  T_sp _SetfFunction;
  bool _IsSpecial;
  bool _IsConstant;
  bool _ReadOnlyFunction;
  List_sp _PropertyList;

private:
  friend class Class_O;
  friend class Package_O;
  friend class CoreExposer;
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, Symbol_O, "Symbol");

public:
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
public:
  /*! Create a Symbol that doesn't have a package or Metaclass defined */
  //	static Symbol_sp create_classless_packageless(string const& name);

  /*! Only used when creating special symbols at boot time.
	  Before NIL, UNBOUND etc are defined */
  static Symbol_sp create_at_boot(const string &nm);
  static Symbol_sp create(const string &nm);

public:
  string formattedName(bool prefixAlways) const;

public:
  //  T_sp apply();
  //  T_sp funcall();

  // Include the apply function for symbols up to some high arity
  //#include "symbol_apply_inc.h"

  void sxhash_(HashGenerator &hg) const;

  bool isKeywordSymbol();
  Symbol_sp asKeywordSymbol();

  bool amp_symbol_p() const;

  /*! Return a pointer to the value cell */
  inline T_sp *valueReference() { return &(this->_Value); };

  void setf_name(Str_sp nm) { this->_Name = nm; };

  List_sp plist() const;
  void setf_plist(List_sp plist);

  void setReadOnly(bool b) { this->_IsConstant = true; };
  bool getReadOnly() const { return this->_IsConstant; };

  void setReadOnlyFunction(bool b) { this->_ReadOnlyFunction = true; };
  bool getReadOnlyFunction() const { return this->_ReadOnlyFunction; };

  /*! Return true if the symbol is dynamic/special */
  bool specialP() const { return this->_IsSpecial; };

  Symbol_sp copy_symbol(T_sp copy_properties) const;
  bool isExported();

  /*! Return the value slot of the symbol - throws if unbound */
  T_sp symbolValue() const;

  /*! Return the address of the value slot of the symbol */
  inline T_sp &symbolValueRef() { return this->_Value; };

  /*! Return the value slot of the symbol or UNBOUND if unbound */
  T_sp symbolValueUnsafe() const;

  void makeSpecial();

  void makeConstant(T_sp val);

  bool boundP() const;

  void makunbound();

  T_sp defparameter(T_sp obj);
  T_sp defconstant(T_sp obj);

  T_sp setf_symbolValue(T_sp obj);

  void setf_symbolValueReadOnlyOverRide(T_sp obj);

  void setSetfFdefinition(Function_sp fn) { this->_SetfFunction = fn; };
  inline T_sp getSetfFdefinition() { return this->_SetfFunction; };
  inline bool setf_fboundp() const { return !this->_SetfFunction.unboundp(); };
  void resetSetfFdefinition() { this->_SetfFunction = _Unbound<Function_O>(); };

  bool isConstant() const { return this->_IsConstant; };

  /*! Set the global fnction value of this symbol */
  void setf_symbolFunction(T_sp exec);

  /*! Return the global bound function */
  inline T_sp symbolFunction() { return this->_Function; };

  /*! Return true if the symbol has a function bound*/
  bool fboundp() const { return !this->_Function.unboundp(); };

  const char *permanentName() const;

  string symbolNameAsString() const;

  Str_sp symbolName() const { return this->_Name; };
  Str_sp identifierName() const { return this->symbolName(); };

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
  string __str__() { return _rep_(gctools::make_tagged_other<Symbol_O>(this)); };
  //	string description() const { return "Symbol("+this->_Value+")";};

public: // ctor/dtor for classes with shared virtual base
  /*! Special constructor used when starting up the Lisp environment */
  explicit Symbol_O(bool dummy); // string const &name);
  /*! Used to finish setting up symbol when created with the above constructor */
  void finish_setup(Package_sp pkg, bool exportp);

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

public:
  explicit Symbol_O();
  virtual ~Symbol_O(){};
};

T_sp af_symbolValue(const Symbol_sp sym);
Str_sp af_symbolName(Symbol_sp sym);
T_sp af_symbolPackage(Symbol_sp sym);
Function_sp af_symbolFunction(Symbol_sp sym);
bool af_boundp(Symbol_sp sym);
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

TRANSLATE(core::Symbol_O);
template <>
struct gctools::GCInfo<core::Symbol_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif //]
