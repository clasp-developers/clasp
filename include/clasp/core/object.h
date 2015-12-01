/*
    File: object.h
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

#include <vector>
#include <string>
#include <map>
#include <typeinfo>
#ifdef __GNUG__
#include <cxxabi.h>
#endif
#ifndef OBJECT_H //[
#define OBJECT_H

#include <clasp/core/foundation.h>
#include <clasp/core/newhash.h>
#include <clasp/core/commonLispPackage.fwd.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/keywordPackage.fwd.h>
#include <clasp/core/extensionPackage.fwd.h>
#include <clasp/core/compPackage.fwd.h>
#include <clasp/core/closPackage.fwd.h>
#include <clasp/core/grayPackage.fwd.h>
//#include <clasp/core/objRef.fwd.h>
#include <clasp/core/lisp.fwd.h>
#include <clasp/core/lispStream.fwd.h>

/*!Use the _Class static class variable instead of findClass */
#define USE_STATIC_METACLASS 1

//
// When debugging how a lisp environment is created set this to 1 otherwise 0
//
#define DEBUG_ENVIRONMENT_CREATION 1

//
// Try using Plug_Os directly rather than O_RequiredPlugs
//
#define USE_PLUGS 1

// I need nil, wxWidgets defines it as NULL - stupid
#ifdef nil
#undef nil
#endif

#define NO_CLASS_ID 0

/*! State that a class has an external base class.
  This is scraped out by registerClasses.py
*/
#define EXTERNAL_BASE_CLASS(m)

namespace core {

class Archive_O;
typedef gctools::smart_ptr<Archive_O> Archive_sp;
class SNode_O;
typedef gctools::smart_ptr<SNode_O> SNode_sp;
typedef gctools::smart_ptr<SNode_O> ArchiveP;
class Function_O;
typedef gctools::smart_ptr<Function_O> Function_sp;

bool cl_eq(T_sp x, T_sp y);
bool cl_eql(T_sp x, T_sp y);
bool cl_equal(T_sp x, T_sp y);
bool cl_equalp(T_sp x, T_sp y);
bool clasp_charEqual2(T_sp, T_sp);
};

/*! \page objects How to use the Object hierarchy
 *
 * The Object hierarchy consists of objects that contain a weak_ptr reference to themselves and can
 * be archived using the core::archive facility.
 *
 * To subclass into the Object hierarchy
 *
 * Within the header file...\n
 *
 * 	-# Make your object a subclass of Object or one of its subclasses.
 *	-# Add the following macro at the top of the class:\n
 *		- DEFI NE_CLASS( O_class, classNameString, O_superClass )
 *	-# Add a archive method with the form:\n
 *		- virtual void archive(core::ArchiveP node);
 *	-# If this is a base class then add a archive method with the form:\n
 *		- virtual void archiveBase(core::ArchiveP node);
 *
 * Within the code file...\n
 *
 *	-# Add the following macro at the top of the source file that contains the methods\n
 *		- REGISTER_CLASS(aNamespace,O_class);
 *	-# Use SIMPLE_PYTHON_INTERFACE(_className(No _O)) or...\n
 *		-# Remove all initializers from the boost::python::class_ template function using:\n
 *			- boost::python::no_init
 *		-# Set up the base class in the boost::python::class_ template function:\n
 *			- boost::python::bases<O_superClass>
 *		-# Create a function to allow python to create the object called:\n
 *			- boost::python::def("create_class",&RP_OLD_Create<O_class> );\n
 *			- Or create static methods that call RP_OLD_Create<O_className> and initialize the object.
 *
 * Within other files...\n
 *
 *	-# Use RP_OLD_Create<O_className> to create instances of the class.
 *
 * (This text is in object.h)
 */

//----------------------------------------------------
//----------------------------------------------------
//----------------------------------------------------
//
// Forward references for important classes
//
//
namespace core {
SMART(Class);
SMART(Record);
SMART(BuiltInClass);
SMART(StandardClass);
SMART(Model);
SMART(LambdaListHandler);
SMART(Binder);
SMART(Symbol);
SMART(CandoDatabase);
SMART(Cons);
};

#define NO_BASE_CLASS "-NOBASE-"

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//
// Environment stuff
//

// #if 0 // moving this functionality into intrusiveRefCountGarbageCollection

// /*!
//  * Template function for creating objects of a specific class
//  *
//  * See gctools/brclMemoryPoolSystem.h for GC_RESERVE_TRY/GC_RESERVE_GET
//  */
// template <class oClass>
// gctools::smart_ptr<oClass> RP_OLD_Create(bool initialize)
// {_G();
//     gctools::smart_ptr<oClass> obj= gctools::smart_ptr<oClass>(new oClass()); // (new oClass(mc));
// //    if (initialize) obj->initialize(); // Initialize instance everything that depends on class hierarchy
//     return obj;
// }

// /*!
//  * Template function for creating objects of a specific class
//  *
//  * See gctools/brclMemoryPoolSystem.h for GC_RESERVE_TRY/GC_RESERVE_GET
//  */
// template <class oClass, class... ARGS>
// gctools::smart_ptr<oClass> RP_OLD_Create_VARIADIC(bool initialize,ARGS&&... args)
// {_G();
//     gctools::smart_ptr<oClass> obj= gctools::smart_ptr<oClass>(new oClass(args...)); // (new oClass(mc));
// //    if (initialize) obj->initialize(); // Initialize instance everything that depends on class hierarchy
//     return obj;
// }

// /*!
//  * Set to true once all of the Classes have been initialized
//  */

// template <class o_class>
// core::T_sp	copy_Object(const o_class* cur)
// {_G();
//     core::T_sp	obj;
//     obj = (gctools::smart_ptr<o_class>)(new o_class(*cur));
//     // Any further initialization should be handled by a copy or deepCopy method
//     return obj;
// }

// template <class Dumb_Class>
// gctools::smart_ptr<Dumb_Class> RP_OLD_Copy(const Dumb_Class* c)
// {
//     return gctools::dynamic_pointer_cast<Dumb_Class>(copy_Object<Dumb_Class>(c));
// }

// template <class Dumb_Class>
// gctools::smart_ptr<Dumb_Class> RP_OLD_Copy(gctools::smart_ptr<Dumb_Class> c)
// {
//     return gctools::dynamic_pointer_cast<Dumb_Class>(copy_Object<Dumb_Class>(c.get()));
// }

// #endif

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//      OOOO     OOOO         OO  OOOOOO   OO   OOOOOO
//    OO    OO   OO  OO       OO  OO     OO  OO   OO
//    OO    OO   OO  OO       OO  OO     OO       OO
//    OO    OO   OOOO         OO  OOOO   OO       OO
//    OO    OO   OO  OO       OO  OO     OO       OO
//    OO    OO   OO  OO   OO  OO  OO     OO  OO   OO
//      OOOO     OOOO       OO    OOOOOO   OO     OO
//
//
//

class _RootDummyClass : public gctools::GCObject {
private:
public:
  static core::Symbol_sp static_classSymbol() { return UNDEFINED_SYMBOL; };
  static void ___set_static_creator(gc::tagged_pointer<core::Creator> cb){};

public:
  explicit _RootDummyClass();
  virtual ~_RootDummyClass(){};
};

template <class T_Base>
struct LispBases1 {
  typedef T_Base Base1;
  static inline bool baseClassSymbolsDefined() {
    if (IS_SYMBOL_UNDEFINED(Base1::static_classSymbol())) {
      THROW_HARD_ERROR(BF("Base class is not defined yet"));
    }
    return true;
  }

  static inline core::Symbol_sp baseClass1Id() {
    return Base1::static_classSymbol();
  }

  static inline core::Symbol_sp baseClass2Id() {
    return UNDEFINED_SYMBOL;
  }
};

template <class T_VirtualBase0, class T_Base1, class T_Base2>
struct LispVirtualBases2 {
  typedef T_VirtualBase0 VirtualBase0;
  typedef T_Base1 Base1;
  typedef T_Base2 Base2;

  static inline bool baseClassSymbolsDefined() {
    if (IS_SYMBOL_UNDEFINED(Base1::static_classSymbol())) {
      core::Symbol_sp base1ClassSymbol = Base1::static_classSymbol();
      THROW_HARD_ERROR(BF("Base1 class[%d] is not defined yet") % base1ClassSymbol);
    }
    if (IS_SYMBOL_UNDEFINED(Base2::static_classSymbol())) {
      core::Symbol_sp base2ClassSymbol = Base2::static_classSymbol();
      THROW_HARD_ERROR(BF("Base2 class[%d] is not defined yet") % base2ClassSymbol);
    }
    return true;
  }

  static inline core::Symbol_sp baseClass1Id() {
    return Base1::static_classSymbol();
  }

  static inline core::Symbol_sp baseClass2Id() {
    return Base2::static_classSymbol();
  }
};

template <class T_Base1, class T_Base2>
struct LispBases2 {
  typedef T_Base1 Base1;
  typedef T_Base2 Base2;

  static inline bool baseClassSymbolsDefined() {
    if (IS_SYMBOL_UNDEFINED(Base1::static_classSymbol())) {
      core::Symbol_sp base1ClassSymbol = Base1::static_classSymbol();
      THROW_HARD_ERROR(BF("Base1 class[%d] is not defined yet") % base1ClassSymbol);
    }
    if (IS_SYMBOL_UNDEFINED(Base2::static_classSymbol())) {
      core::Symbol_sp base2ClassSymbol = Base2::static_classSymbol();
      THROW_HARD_ERROR(BF("Base2 class[%d] is not defined yet") % base2ClassSymbol);
    }
    return true;
  }

  static inline core::Symbol_sp baseClass1Id() {
    return Base1::static_classSymbol();
  }

  static inline core::Symbol_sp baseClass2Id() {
    return Base2::static_classSymbol();
  }
};

namespace core {

class KeyValueMapper {
public:
  /*! Return true if the mapper should continue */
  virtual bool mapKeyValue(T_sp key, T_sp value) = 0;
};

//#define DEBUG_HASH_GENERATOR
class HashGenerator {
  static const int MaxParts = 32;

private:
  int _NextPartIndex;
  Fixnum _Parts[MaxParts];
#ifdef DEBUG_HASH_GENERATOR
  bool _debug;
#endif
public:
  HashGenerator(bool debug = false) : _NextPartIndex(0)
#ifdef DEBUG_HASH_GENERATOR
                                      ,
                                      _debug(debug)
#endif
                                      {};

  bool addPart(Fixnum part) {
    if (this->_NextPartIndex >= MaxParts)
      return false;
    this->_Parts[this->_NextPartIndex] = part;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d Added part[%d] --> %ld\n", __FILE__, __LINE__, this->_NextPartIndex, part);
    }
#endif
    ++this->_NextPartIndex;
    return true;
  }

  /*Add the bignum across multiple parts, return true if everything was added */
  bool addPart(const mpz_class &bignum);

  /*! Return true if not accepting any more parts */
  bool isFull() const {
    return this->_NextPartIndex >= MaxParts;
  }

  /*! Return true if can still accept parts */
  bool isFilling() const {
    return this->_NextPartIndex < MaxParts;
  }

  gc::Fixnum hash(gc::Fixnum bound = 0) const {
    gc::Fixnum hash = 5381;
    for (int i = 0; i < this->_NextPartIndex; i++) {
      hash = (gc::Fixnum)hash_word((cl_intptr_t)hash, (cl_intptr_t) this->_Parts[i]);
#ifdef DEBUG_HASH_GENERATOR
      if (this->_debug) {
        printf("%s:%d  calculated hash = %lu with part[%d] --> %lu\n", __FILE__, __LINE__, hash, i, this->_Parts[i]);
      }
#endif
      //		hash = ((hash << 5) + hash) + this->_Parts[i];
    }
    if (bound)
      return ((cl_intptr_t)hash) % bound;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d  final hash = %lu\n", __FILE__, __LINE__, hash);
    }
#endif
    return hash;
  }

  string asString() const {
    std::stringstream ss;
    ss << "#<HashGenerator ";
    for (int i = 0; i < this->_NextPartIndex; i++) {
      ss << this->_Parts[i] << "-";
    }
    ss << ">";
    return ss.str();
  }

  void hashObject(T_sp obj);
};

//    extern Symbol_sp _sym_list;
//    extern Symbol_sp _sym_nil;
//    extern Symbol_sp _sym_t;
class T_O : public _RootDummyClass {
public:
  virtual ~T_O(){};

private:
  friend class CoreExposer;

#define __COMMON_VIRTUAL_CLASS_PARTS(oNamespace, oPackage, oClass, oclassName)                                         \
  FRIEND_GC_SCANNER(oNamespace::oClass);                                                                               \
                                                                                                                       \
public:                                                                                                                \
  template <class DestClass> gctools::smart_ptr</* TODO: const */ DestClass> const_sharedThis() const {                \
    oClass *not_const_this_gc_safe = const_cast<oClass *>(this); /* Should be GC-safe because this should be a root */ \
    return gctools::smart_ptr<DestClass>(not_const_this_gc_safe);                                                      \
  };                                                                                                                   \
  template <class DestClass> gctools::smart_ptr<DestClass> sharedThis() {                                              \
    return gctools::smart_ptr<DestClass>(this);                                                                        \
  };                                                                                                                   \
  gctools::smart_ptr<oClass> asSmartPtr() const { return this->const_sharedThis<oClass>(); };                          \
  gctools::smart_ptr<oClass> asSmartPtr() { return this->sharedThis<oClass>(); };                                      \
                                                                                                                       \
public:                                                                                                                \
  /*    static gctools::smart_ptr<oClass> nil(core::Lisp_sp lisp);	*/                                                  \
  typedef oClass ThisClass;                                                                                            \
  typedef gctools::smart_ptr<oClass> smart_ptr;                                                                        \
                                                                                                                       \
public:                                                                                                                \
  static core::Symbol_sp ___staticClassSymbol;                                                                         \
  static core::Class_sp ___staticClass;                                                                                \
  static gctools::tagged_pointer<core::Creator> static_creator;                                                        \
  static int static_Kind;                                                                                              \
  /* static gctools::smart_ptr<oClass> _nil; depreciate this in favor of _Nil<oClass>()? */                            \
  /* static gctools::smart_ptr<oClass> _unbound; depreciate this in favor of _Unbound<oClass>()? */                    \
  /*    static oClass* ___staticDereferencedNilInstance;	*/                                                            \
  /*    static oClass* ___staticDereferencedUnboundInstance; */                                                        \
public:                                                                                                                \
  static void ___set_static_ClassSymbol(core::Symbol_sp i) { oClass::___staticClassSymbol = i; };                      \
  static void ___set_static_creator(gctools::tagged_pointer<core::Creator> al) { oClass::static_creator = al; };       \
  static string static_packageName() { return oPackage; };                                                             \
  static string static_className() { return core::lispify_symbol_name(oclassName); };                                  \
  static core::Symbol_sp static_classSymbol() { return oClass::___staticClassSymbol; };                                \
  virtual core::Symbol_sp virtual_classSymbol() { return oClass::___staticClassSymbol; };                              \
  virtual core::Class_sp __class() const {                                                                             \
    return oClass::___staticClass;                                                                                     \
  }                                                                                                                    \
  static string Package() { return oClass::static_packageName(); };                                                    \
  static string Pkg() { return Package(); };                                                                           \
  static void exposeCando(core::Lisp_sp);                                                                              \
  static void exposePython(core::Lisp_sp);

#define __COMMON_CLASS_PARTS(oNamespace, oPackage, oClass, oclassName)   \
  __COMMON_VIRTUAL_CLASS_PARTS(oNamespace, oPackage, oClass, oclassName) \
  static gctools::smart_ptr<oClass> create() {                           \
    GC_ALLOCATE(oClass, obj);                                            \
    return obj;                                                          \
  };

#define LISP_TEMPLATE_CLASS(oClass) \
  __COMMON_VIRTUAL_CLASS_PARTS(CurrentPkg, oClass, typeid(oClass).name())

#define LISP_META_CLASS(x) // nothing
#ifndef SCRAPING
#define LISP_BASE1(b1) \
public:                \
  typedef b1 Base;     \
  typedef LispBases1<b1> Bases;
#endif
#define LISP_VIRTUAL_BASE2(v0, b1, b2) \
public:                                \
  typedef LispVirtualBases2<v0, b1, b2> Bases;

#define LISP_BASE2(b1, b2) \
public:                    \
  typedef LispBases2<b1, b2> Bases;

#define LISP_TEMPLATE_BASE1(b1) \
  /*no-scrape*/ LISP_BASE1(b1);

#ifndef SCRAPING
#define LISP_CLASS(aNamespace, aPackage, aClass, aClassName) \
  __COMMON_CLASS_PARTS(aNamespace, aPackage, aClass, aClassName);

#define LISP_VIRTUAL_CLASS(aNamespace, aPackage, aClass, aClassName) \
  __COMMON_VIRTUAL_CLASS_PARTS(aNamespace, aPackage, aClass, aClassName);
#endif

  LISP_BASE1(_RootDummyClass);
  LISP_VIRTUAL_CLASS(core, ClPkg, T_O, "T");

#define DECLARE_INIT_GLOBALS() \
public:                        \
  static void lisp_initGlobals(core::Lisp_sp lisp);

#define DECLARE_INIT()

#define DECLARE_MAKE_INIT()

protected: // T_O instance variables, these use memory
           /*! Every object points to its class
		 */
public:
  virtual core::Lisp_sp lisp();
  virtual core::Lisp_sp lisp() const;

public:
  //	void invoke__init__(core::Function_sp e, core::List_sp args, core::Environment_sp environment, core::Lisp_sp lisp);

public:
  /*! Return a shallow copy of the object */
  virtual T_sp shallowCopy() const;
  /*! Return a deep copy of the object */
  virtual T_sp deepCopy() const;

public:
  virtual void sxhash_(HashGenerator &hg) const;

public:
public:
  string className() const;

public: // ----------------
  void setTrackName(const string &nm);
  void setTrackNameUsingAddress();

  //! Initialize member variables and those that depend on sharedThis
  virtual void initialize();

  //	void	__setClass(Class_sp mc) { this->_Class = mc.get(); };
  //	void	__setClass_using_Symbol(Symbol_sp mcSym);
  //	void    __setClassPointer(core::Class_O* mcp) { this->_Class = mcp;};
  //	void	__resetInitializationOwner() { this->_InitializationOwner.reset();};

public: // Introspection stuff -----------------------------
        //    core::Class_sp baseClass();

  virtual bool isStandardObject() { return false; };

  /*! Return true if this object is assignable from obj
	 * or if this isADescendant of obj
	 * in the default environment hierarchy
	 */
  bool isAInstanceOf(core::Class_sp mc);

  /*! Return true if this isADescendant of obj
	 * in the given environment hierarchy
	 */
  //	    bool hierarchy_isA(core::Hierarchy_sp h, T_sp obj);

  //	bool isOfClassByClassSymbol(Symbol_sp cid);
  //	template <class o_class> bool isOfClass() { return this->isOfClassByClassSymbol(o_class::static_classSymbol()); };
  /*! Check if this is a subclass of cid */

  bool isAssignableToByClassSymbol(Symbol_sp cid) const;
  bool isAssignableToClass(core::Class_sp mc) const;

  template <class o_class>
  bool isAssignableTo() const { return this->isAssignableToByClassSymbol(o_class::static_classSymbol()); }

#if 0
	/*! isA is a shorter way to write isAssignableTo */
	template <class o_class>
	bool i s A() const { return this->isAssignableToByClassSymbol(o_class::static_classSymbol());};
#endif

  /*! If this object can render itself into a graphics object then
	 * return true
	 */
  //	virtual bool canRender() { return false; };
  /*! If this object can render itself into a graphics object then
	 * return the graphics
	 */
  //	virtual core::Render_sp rendered(core::List_sp options) {_G(); THROW_HARD_ERROR(BF("Sub-class(%s) must implement") % this->className() ); };
  /*! Return the center of geometry of the object.
	 * If it doesn't have one then return nil
	 */
  //	virtual core::OVector3_sp centerOfRenderedGeometry();

  //	core::Render_sp boost_rendered();

public: // predicates
public: // Serialization stuff -----------------------------
        /*! New serializer that uses s-exp */
#if defined(OLD_SERIALIZE)
  virtual void serialize(serialize::SNode node);
#endif

  //! Serialize a base object of an object .
  //Only called by subclasses that want to archive their base
  virtual void archiveBase(core::ArchiveP node);

  /*! Finalize an object loaded from an archive.
	 * objects that need to finalize their contents once the entire archive
	 * has loaded can register with the loadArchive
	 * that they want loadFinalize called once everything is loaded
	 * Return true if finalization was successful.
	 * Return false if it was not.
	 *
	 * The node is passed only for throwing errors in finalization that can be traced back to a node
	 */
  virtual bool loadFinalize(core::ArchiveP node);

  //! Save an object to an archive.
  //If you implement archive you don't need to implement this.
  virtual void save(core::ArchiveP node) {
    _OF();
    HARD_SUBCLASS_MUST_IMPLEMENT();
  };

  bool sameAs(T_sp obj) {
    _OF();
    SIMPLE_ERROR(BF("Seriously???? Am I still using this?"));
    //	    return ( this == obj.get() );
  };

  virtual string asXmlString() const;

#if defined(XML_ARCHIVE)
  virtual string asXmlStringWrap(const string &nodeName, const string &rawAttributes);
  void saveXmlAs(const string &fileName, const string &uid);

#endif // defined(XML_ARCHIVE)

  /*! Objects can catch signals from Models
	 * but only Model's and subclasses can send them
	 */
  virtual void catchSignal(core::Symbol_sp signal, core::Model_sp sender, core::List_sp data) { _G(); /*Do nothing*/ };
  virtual void propagateSignal(core::Symbol_sp signal){}; // Objects don't propagate signals

public: // Description stuff
        //! A technical string representation
  virtual string description() const;
  //! Just describe the contents
  virtual string descriptionOfContents() const;
  //! A pretty-print representation
  virtual string descriptionNoConst() { return this->description(); };
  //! A pretty-print representation
  virtual string __repr__() const { return this->description(); };
  //! Common Lisp __write__(T_sp strm)
  virtual void __write__(T_sp strm) const;
  //! A pretty-print representation
  virtual string __str__() { return _rep_(this->sharedThis<T_O>()); };
  virtual void describe(T_sp stream);
  virtual void dump() { this->describe(lisp_true()); };

public:
  //! Encode this object as an a-list
  virtual core::List_sp encode();
  //! Decode this object from an a-list
  virtual void decode(core::List_sp);
  virtual void initialize(core::List_sp alist);
  virtual bool fieldsp() const { return false; };
  virtual void fields(Record_sp record) { SUBIMP(); };

public:
  string descriptionNonConst();

public:
  /* Return true if the two objects equal each other
	 * "equal" means different things for different objects.
	 * For instance strings compare their string values
	 * Integers will compare integer values
	 * other objects will punt and just compare their pointers to
	 * see if they are identical.
	 *
	 * Lisp’s equality operators are:
	 *
	 * = compares only numbers, regardless of type.
	 * eq compares object addresses. Two objects are eq iff they are actually the
	 * same object in memory. Don’t use it for numbers and characters.
	 * eql compares symbols similarly to eq, numbers (type sensitive) and characters (case sensitive)
	 * equal compares more general objects. Two objects are equal iff they are eql,
	 * strings of eql characters, bit vectors of the same contents, or lists of equal objects.
	 * For anything else, eq is used.
	 * equalp is like equal, just more advanced. Comparison of numbers is type insensitive.
	 * Comparison of chars and strings is case insensitive.
	 * Lists, hashes, arrays and structures are equalp if their members are equalp. For anything else, eq is used.
	 */
  /*! Return true iff the two objects are the same object.
	 * Meaning their memory addresses are identical
	 */
  inline bool eq(T_sp obj) const { return obj.objectp() && (this == obj.get()); };
  /*! Return true if the two objects are the same object.
	 * If they aren't the same object for numbers and values
	 * the values are compared.
	 */
  virtual bool eql_(T_sp obj) const;

  /*! Only compares numbers. Return true if the two numbers are equivalent ignoring type.
	 * If they aren't the same object for numbers and values
	 * the values are compared.
	 */
  //	virtual bool eqn(T_sp obj) const;
  /*! Return true the two objects are equivalent or structuraly simular
	 * regardless of where they are located in memory.
	 * This works like Common LISP's "equal"
	 */
  virtual bool equal(T_sp obj) const;
  /*! Return true the two objects are equivalent or structuraly simular
	 * regardless of where they are located in memory.
	 * It uses "==" (which is type insensitive for numbers).
	 * This works like Common LISP's "equalp"
	 */
  virtual bool equalp(T_sp obj) const;

  /*! Return true if the this is less than obj
	 * All other relative comparisons call this one
	 * so if you define this in a subclass then all the other comparisons
	 * will work
	 * but you can define the others if you want
	 *
	 * -lt- is the only one that has to be defined if you want relative comparisons
	 * but the others can be defined to improve efficiency
	 */
  virtual bool operator<(T_sp obj) const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  }; // This is the only one that absolutely has to be defined in a subclass
  virtual bool operator<=(T_sp obj) const {
    if (cl_eql(this->asSmartPtr(), obj))
      return true;
    return this->operator<(obj);
  };
  virtual bool operator>(T_sp obj) const { return !this->operator<=(obj); };
  virtual bool operator>=(T_sp obj) const { return !this->operator<(obj); };

public: // Instance protocol
  //! Some Class objects will create instances of classes different from themselves
  virtual core::Class_sp _instanceClass() const { return this->__class(); };

  virtual T_sp instanceClassSet(Class_sp mc);

  /*! Allocate space for (slots) slots and initialize them */
  virtual void initializeSlots(int slots);

  /*! ECL slot handling, slots are indexed with integers */
  virtual T_sp instanceRef(int idx) const;
  /*! ECL slot handling, slots are indexed with integers */
  virtual T_sp instanceSet(int idx, T_sp val);

  /*! Set the signature (metaclass slot definitions) for the instance */
  virtual T_sp instanceSigSet();
  /*! Get the signature (metaclass slot definitions) for the instance */
  virtual T_sp instanceSig() const;

  /*! Return number of slots if instance of Instance_O or StructureObject_O class
	  otherwise return nil */
  virtual T_sp oinstancepSTAR() const { return _Nil<T_O>(); };
  /*! Return number of slots if instance of Instance_O otherwise return nil */
  virtual T_sp oinstancep() const { return _Nil<T_O>(); }; //
  bool instancep() const { return oinstancep().isTrue(); };
  virtual bool environmentp() const { return false; };
  virtual bool genericFunctionP() const { return false; };
  /*! Return number of slots if instance of Instance_O otherwise return nil */
  virtual T_sp ofuncallableInstanceP() const { return _Nil<T_O>(); }; //
  bool funcallableInstanceP() const { return ofuncallableInstanceP().isTrue(); };

public:
  /*! Some objects can return contained objects references by class and name
	 */
  //  virtual T_sp oGetReference(core::ObjRef_sp ref) { return _Nil<T_O>(); };
};

inline void clasp_sxhash(T_sp obj, HashGenerator &hg) {
  if (obj.fixnump()) {
    hg.addPart(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    hg.addPart((gc::Fixnum)std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    hg.addPart(obj.unsafe_character());
    return;
  }
  obj->sxhash_(hg);
};
};

namespace core {

#define ARGS_cl_eq "(x y)"
#define DECL_cl_eq ""
#define DOCS_cl_eq "eq"
inline bool cl_eq(T_sp x, T_sp y) {
  return (x == y);
};

#define ARGS_cl_eql "(x y)"
#define DECL_cl_eql ""
#define DOCS_cl_eql "eql"
inline bool cl_eql(T_sp x, T_sp y) {
  if (x.fixnump()) {
    return x.raw_() == y.raw_();
  } else if (x.single_floatp()) {
    if (y.single_floatp()) {
      return gc::tagged_single_float_masked(x.raw_()) == gc::tagged_single_float_masked(y.raw_());
    }
    return false;
  } else if (x.characterp()) {
    if (y.characterp()) {
      return x.unsafe_character() == y.unsafe_character();
    }
    return false;
  }
  return x->eql_(y);
};

#define ARGS_cl_equal "(x y)"
#define DECL_cl_equal ""
#define DOCS_cl_equal "equal"
inline bool cl_equal(T_sp x, T_sp y) {
  if (x.fixnump()) {
    return x.raw_() == y.raw_();
  } else if (x.single_floatp()) {
    if (y.single_floatp()) {
      return gc::tagged_single_float_masked(x.raw_()) == gc::tagged_single_float_masked(y.raw_());
    }
    return false;
  } else if (x.characterp()) {
    if (y.characterp()) {
      return x.unsafe_character() == y.unsafe_character();
    }
    return false;
  }
  return x->equal(y);
};

extern int basic_compare(Number_sp na, Number_sp nb);

#define ARGS_cl_equalp "(x y)"
#define DECL_cl_equalp ""
#define DOCS_cl_equalp "equalp"
inline bool cl_equalp(T_sp x, T_sp y) {
  if (x.fixnump()) {
    if (y.fixnump()) {
      return x.raw_() == y.raw_();
    } else if (y.single_floatp()) {
      return (x.unsafe_fixnum() == y.unsafe_single_float());
    } else if (Number_sp ny = y.asOrNull<Number_O>()) {
      return basic_compare(x, y) == 0;
    }
    return false;
  } else if (x.single_floatp()) {
    if (y.single_floatp()) {
      return x.unsafe_single_float() == y.unsafe_single_float();
    } else if (y.fixnump()) {
      return x.unsafe_single_float() == y.unsafe_fixnum();
    } else if (Number_sp ny = y.asOrNull<Number_O>()) {
      return basic_compare(x, y);
    }
    return false;
  } else if (x.characterp()) {
    return clasp_charEqual2(x, y);
  }
  return x->equalp(y);
};
};

namespace core {

template <class _W_>
class LispObjectCreator : public core::Creator {
public:
  typedef core::Creator TemplatedBase;

public:
  DISABLE_NEW();
  size_t templatedSizeof() const { return sizeof(LispObjectCreator<_W_>); };
  virtual void describe() const {
    printf("LispObjectCreator for class %s  sizeof_instances-> %zu\n", _rep_(reg::lisp_classSymbol<_W_>()).c_str(), sizeof(_W_));
  }
  virtual core::T_sp allocate() {
    GC_ALLOCATE(_W_, obj);
    return obj;
  }
  virtual void searcher(){};
};
};

template <typename T>
class gctools::GCKind<core::LispObjectCreator<T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename core::LispObjectCreator<T>::TemplatedBase>::Kind;
};

namespace core {
template <class oclass>
T_sp new_LispObject() {
  _G();
  T_sp obj = oclass::static_creator->allocate();
  //	GC_ALLOCATE(oclass,obj );
  return obj;
};
}

template <class o_class>
inline gctools::smart_ptr<o_class> downcast(core::T_sp c) {
  return c.as<o_class>();
}
// ------------------------------------------------------------
//
// New way of declaring and registering classes
//
// To define a class use:
//
//
// Within the cc file that contains the code and within
// the namespace of the class use:
// INITIALIZE_CLASS(_class_)
//
// That's it, everything else will be taken care of

#if defined(XML_ARCHIVE)
#define DECLARE_ARCHIVE() \
public:                   \
  void archiveBase(core::ArchiveP node);
#endif // defined(XML_ARCHIVE)

#define DECLARE_STANDARD_LISP_FUNCTIONS() \
  DECLARE_ARCHIVE();                      \
  DECLARE_INIT();                         \
                                          \
public:                                   \
  void initialize();

#define __DEFAULT_INITIALIZATION() \
public:                            \
  inline void initialize() {       \
    this->Base::initialize();      \
  }

#define DEFAULT_CTOR(oClass) \
public:                      \
  explicit oClass() : oClass::Base(){}; /* default ctor */
#define DEFAULT_DTOR(oClass) \
public:                      \
  ~oClass(){};
#define DEFAULT_CTOR_DTOR(oClass) \
  DEFAULT_CTOR(oClass);           \
  DEFAULT_DTOR(oClass);           \
/*end*/

#define DEFAULT_CTOR_WITH_VIRTUAL_BASE(oClass, oVirtualBase) \
public:                                                      \
  explicit oClass() : oVirtualBase(), oClass::Base(){}; /* default ctor */
#define DEFAULT_CTOR_DTOR_WITH_VIRTUAL_BASE(oClass, oVirtualBase) \
  DEFAULT_CTOR_WITH_VIRTUAL_BASE(oClass, oVirtualBase);           \
  DEFAULT_DTOR(oClass);                                           \
/*end*/

//------------------------------------------------------------
//------------------------------------------------------------
//------------------------------------------------------------
//------------------------------------------------------------
//
//
// Register classes
//
//
//

/*! Register all classes with this method
 */
template <class oClass>
inline void registerClass(core::ExposeCandoFunction exposeCandoFunction,
                          core::ExposePythonFunction exposePythonFunction,
                          core::InitializationCallback initGlobalCallback,
                          core::Lisp_sp lisp) {
  _G();
  string nobase;
  core::Symbol_sp classSymbol;
  if (oClass::static_classSymbol() != core::T_O::static_classSymbol()) {
    if (!oClass::Bases::baseClassSymbolsDefined()) {
      _G();
      THROW_HARD_ERROR(boost::format("You are trying to register class(%s) "
                                     "but you have not registered its baseClass") %
                       oClass::static_className());
    }
  }
  // If the class is already initialized then skip assigning it a classSymbol
  //    if ( oClass::static_classSymbol() == UndefinedUnsignedInt )
  if (!IS_SYMBOL_DEFINED(oClass::static_classSymbol())) {
    core::Symbol_sp classSymbol = core::lisp_intern(oClass::static_className(), oClass::static_packageName());
    LOG(BF("Setting staticClassSymbol for class to: %d") % classSymbol);
    oClass::___set_static_ClassSymbol(classSymbol);
    if (!oClass::static_creator) {
      gctools::tagged_pointer<core::LispObjectCreator<oClass>> lispObjectCreator = gctools::ClassAllocator<core::LispObjectCreator<oClass>>::allocateClass();
      oClass::___set_static_creator(lispObjectCreator);
    }
  }
  LOG(BF("REGISTERING class(%s::%s)  classSymbol(%2d)") % oClass::static_packageName() % oClass::static_className() % oClass::static_classSymbol());
  if (exposeCandoFunction != NULL) {
    (exposeCandoFunction)(lisp);
  }
  // Sometimes we need to initialize globals - a callback can be setup by exposeCandoFunction
  if (initGlobalCallback) {
    initGlobalCallback(_lisp); // lisp_installGlobalInitializationCallback(initGlobalCallback);
  }
};

// ^^^^^^^^^^^^
// ^^^^^^^^^^^^
// ^^^^^^^^^^^^

//
// Defines the class method that defines nil for this class
// and avoids the static initialization fiasco
//
// uint oClass::___staticClassSymbol; = UndefinedUnsignedInt;
#define _REGISTER_CLASS_HEADER_COMMON(oClass)   \
  core::Symbol_sp oClass::___staticClassSymbol; \
  core::Class_sp oClass::___staticClass;        \
  int oClass::static_Kind;                      \
  gctools::tagged_pointer<core::Creator> oClass::static_creator;

#define STATIC_CLASS_INFO(oClass)                            \
/*	oClass* oClass::___staticDereferencedNilInstance;	*/      \
/*  oClass* oClass::___staticDereferencedUnboundInstance;	*/ \
/* gctools::smart_ptr<oClass> oClass::_nil; */               \
/* gctools::smart_ptr<oClass> oClass::_unbound;	*/

#define _REGISTER_CLASS_HEADER(ns, oClass)                      \
  _REGISTER_CLASS_HEADER_COMMON(oClass);                        \
  void Call_exposePython_##ns##__##oClass(core::Lisp_sp lisp) { \
    oClass::exposePython(lisp);                                 \
  }                                                             \
  void Register_##ns##__##oClass(core::Lisp_sp lisp)

#define _REGISTER_CLASS_HEADER_ROOT_NAMESPACE(oClass)   \
  _REGISTER_CLASS_HEADER_COMMON(oClass);                \
  void Call_exposePython_##oClass(core::Lisp_sp lisp) { \
    oClass::exposePython(lisp);                         \
  }                                                     \
  void Register_##oClass(core::Lisp_sp lisp)

/*! Register a class.
 * Return True if it could be registered otherwise False.
 * If this class depends on other classes being registered
 * then return False and classes will be repeatedly registered until they
 * are all registered.
 */
#define REGISTER_CLASS(ns, oClass)                                              \
  _REGISTER_CLASS_HEADER(ns, oClass) {                                          \
    _G();                                                                       \
    registerClass<oClass>(&core::defaultExposeCando<oClass>, NULL, NULL, lisp); \
  };                                                                            \
  void oClass::exposePython(core::Lisp_sp lisp) {}

/*end*/

/*!
 * Use this from now on.
 * 1) Assumes "c" namespace
 * 2) Exposers are static functions within the class (exposeCando,exposePython)
 */
#define EXPOSE_CLASS_ROOT_NAMESPACE(oClass)                                         \
  _REGISTER_CLASS_HEADER_ROOT_NAMESPACE(oClass) {                                   \
    _G();                                                                           \
    registerClass<oClass>(&oClass::exposeCando, &oClass::exposePython, NULL, lisp); \
  };

#define EXPOSE_CLASS(ns, oClass)                                                    \
  _REGISTER_CLASS_HEADER(ns, oClass) {                                              \
    _G();                                                                           \
    registerClass<oClass>(&oClass::exposeCando, &oClass::exposePython, NULL, lisp); \
  };

#define EXPOSE_CLASS_AND_GLOBALS(ns, oClass)                                                               \
  _REGISTER_CLASS_HEADER(ns, oClass) {                                                                     \
    _G();                                                                                                  \
    ::registerClass<oClass>(&oClass::exposeCando, &oClass::exposePython, &oClass::lisp_initGlobals, lisp); \
  };

#define EXPOSE_CLASS_NO_PYTHON(ns, oClass)                                          \
  void oClass::exposePython(core::Lisp_sp lisp) {}                                  \
  _REGISTER_CLASS_HEADER(ns, oClass) {                                              \
    _G();                                                                           \
    registerClass<oClass>(&oClass::exposeCando, &oClass::exposePython, NULL, lisp); \
  };

#define EXPOSE_CLASS_AND_GLOBALS_NO_PYTHON(ns, oClass)                                                     \
  void oClass::exposePython(core::Lisp_sp lisp) {}                                                         \
  _REGISTER_CLASS_HEADER(ns, oClass) {                                                                     \
    _G();                                                                                                  \
    ::registerClass<oClass>(&oClass::exposeCando, &oClass::exposePython, &oClass::lisp_initGlobals, lisp); \
  };

/*!
 * Create a default python interface
 *
 * Don't use the initArgs string, just pass arguments through to the __init__ function
 */
#define PYTHON__init__(pkgName, className, docString) \
  {}

#define PYTHON_CLASS(pkgName, className, initArgs, docString, ___lisp) \
  PYTHON__init__(pkgName, className, docString);                       \
  boost::python::class_<className##_O,                                 \
                        gctools::smart_ptr<className##_O>,             \
                        boost::python::bases<className##_O::Base>,     \
                        boost::noncopyable>(#className "_O", boost::python::no_init)

#define PYTHON_CLASS_2BASES(pkgName, className, initArgs, docString, ___lisp)                           \
  PYTHON__init__(pkgName, className, docString);                                                        \
  boost::python::class_<className##_O,                                                                  \
                        gctools::smart_ptr<className##_O>,                                              \
                        boost::python::bases<className##_O::Bases::Base1, className##_O::Bases::Base2>, \
                        boost::noncopyable>(#className "_O", boost::python::no_init)

/*!
 * Create a default python interface with an init
 */
#define PYTHON_INIT(className)                                             \
  boost::python::def("create_" #className, &RP_OLD_Create<className##_O>); \
  boost::python::class_<className##_O,                                     \
                        gctools::smart_ptr<className##_O>,                 \
                        boost::python::bases<className##_O::Base>,         \
                        boost::noncopyable>(#className, init<core::Lisp_sp>)

namespace core {
Class_sp af_classOf(T_sp obj);
bool cl_eq(T_sp x, T_sp y);
bool cl_eql(T_sp x, T_sp y);
bool cl_equal(T_sp x, T_sp y);
bool cl_equalp(T_sp x, T_sp y);
};

#include <clasp/core/glue.h>
#include <clasp/core/conditions.h>

TRANSLATE(core::T_O);

/*! Used to indicate that methods should be inherited from sequence */
#define INHERIT_SEQUENCE

#include <clasp/core/metaClass.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/tagged_cast_specializations.h>
#include <clasp/core/cxxObject.h>

namespace core {
void initialize_object();
};
#endif //]
