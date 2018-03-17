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

bool cl__eq(T_sp x, T_sp y);
bool cl__eql(T_sp x, T_sp y);
bool cl__equal(T_sp x, T_sp y);
bool cl__equalp(T_sp x, T_sp y);
bool clasp_charEqual2(T_sp, T_sp);
};


//----------------------------------------------------
//----------------------------------------------------
//
// Forward references for important classes
//
//
namespace core {
  FORWARD(Record);
  FORWARD(LambdaListHandler);
  FORWARD(Binder);
  FORWARD(Symbol);
  FORWARD(CandoDatabase);
  FORWARD(Cons);
  FORWARD(General);
};

#define NO_BASE_CLASS "-NOBASE-"


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
  static void set_static_creator(gc::smart_ptr<core::Creator_O> cb){};

public:
  explicit _RootDummyClass() {};
};
template <class T_Base>
struct LispBases1 {
  typedef T_Base Base1;
  static inline bool baseClassSymbolsDefined() {
    if (IS_SYMBOL_UNDEFINED(Base1::static_classSymbol())) {
      throw_hard_error("Base class is not defined yet");
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


namespace core {
  class KeyValueMapper {
  public:
  /*! Return true if the mapper should continue */
    virtual bool mapKeyValue(T_sp key, T_sp value) = 0;
  };

  /* A lighter weight hash generator for EQ and EQL tests. 
     It has only a single part. */
class Hash1Generator {
 public:
  Fixnum _Part;
 public:
  gc::Fixnum hash(gc::Fixnum bound = 0) const {
    gc::Fixnum hash = 5381;
    hash = (gc::Fixnum)hash_word((cl_intptr_t)5381,(cl_intptr_t)this->_Part);
    if (bound) return ((cl_intptr_t)hash) % bound;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d  final hash = %lu\n", __FILE__, __LINE__, hash);
    }
#endif
    return hash;
  }
  gc::Fixnum hashBound(gc::Fixnum bound) const {
    gc::Fixnum hash = 5381;
    hash = (gc::Fixnum)hash_word((cl_intptr_t)5381,(cl_intptr_t)this->_Part);
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d  final hash = %lu\n", __FILE__, __LINE__, hash);
    }
#endif
    return ((cl_intptr_t)hash) % bound;
  }
  bool addPart(Fixnum part) {
    this->_Part = part;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d Added part --> %ld\n", __FILE__, __LINE__, part);
    }
#endif
    return true;
  }
  bool isFilling() const { return true; };
  /*Add the bignum across multiple parts, return true if everything was added */
  bool addPart(const mpz_class &bignum);
  void hashObject(T_sp obj);
};
 
//#define DEBUG_HASH_GENERATOR
  class HashGenerator {
    static const int MaxParts = 32;
    static const int MaxDepth = 8;
  private:
    int _Depth;
    int _NextPartIndex;
    Fixnum _Parts[MaxParts];
#ifdef DEBUG_HASH_GENERATOR
    bool _debug;
#endif
  public:
  HashGenerator(bool debug = false) : _Depth(0), _NextPartIndex(0)
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

    bool addPart0(Fixnum part) {
      this->_Parts[0] = part;
      this->_NextPartIndex=1;
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
      return (this->_NextPartIndex < MaxParts) || (this->_Depth>= MaxDepth);
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
};

#define COMMON_CLASS_PARTS(oNamespace, oPackage, oClass, oclassName) \
  FRIEND_GC_SCANNER(oNamespace::oClass);                                \
 public:                                                                \
  template <class DestClass>                                            \
    gctools::smart_ptr<DestClass> const_sharedThis() const {            \
    oClass *not_const_this_gc_safe = const_cast<oClass *>(this);        \
    return gctools::smart_ptr<DestClass>(not_const_this_gc_safe);       \
  };                                                                    \
  template <class DestClass> gctools::smart_ptr<DestClass> sharedThis() { \
    return gctools::smart_ptr<DestClass>(this);                         \
  };                                                                    \
  gctools::smart_ptr<oClass> asSmartPtr() const                         \
  { return this->const_sharedThis<oClass>(); };                         \
  gctools::smart_ptr<oClass> asSmartPtr()                               \
  { return this->sharedThis<oClass>(); };                               \
 public:                                                                \
  typedef oClass my_type;                                               \
  typedef gctools::smart_ptr<oClass> smart_ptr_type;                 \
 public:                                                                \
  static core::Symbol_sp static_class_symbol;                           \
  static core::Class_sp static_class;                                   \
  static gctools::smart_ptr<core::Creator_O> static_creator;            \
  static gctools::Header_s::Value static_HeaderValue;                   \
 public:                                                                \
  static void set_static_class_symbol(core::Symbol_sp i)                \
  { oClass::static_class_symbol = i; };                                 \
  static void set_static_creator(gctools::smart_ptr<core::Creator_O> al) \
  { oClass::static_creator = al; };                                     \
  static string static_packageName() { return oPackage; };              \
  static string static_className()                                      \
  { return core::lispify_symbol_name(oclassName); };                    \
  static core::Symbol_sp static_classSymbol()                           \
  { return oClass::static_class_symbol; };                              \
  static string Package() { return oClass::static_packageName(); };     \
  static string Pkg() { return Package(); };                            \
  static void register_class_with_redeye() {                            \
    gctools::GCObjectAllocator<oClass>::register_class_with_redeye();   \
  }                                                                     \
  static void expose_to_clasp();

#define LISP_TEMPLATE_CLASS(oClass) \
  COMMON_CLASS_PARTS(CurrentPkg, oClass, typeid(oClass).name())

#ifndef SCRAPING
#define LISP_META_CLASS(x) // nothing

#define LISP_CLASS(aNamespace, aPackage, aClass, aClassName, aBaseClass) \
  public:                                                               \
    typedef aBaseClass Base;                                            \
  typedef LispBases1<Base> Bases;                                       \
  COMMON_CLASS_PARTS(aNamespace, aPackage, aClass, aClassName);         \
  static gctools::smart_ptr<aClass> create() {                          \
      return gctools::GC<aClass>::allocate_with_default_constructor();  \
    };                                                                  \
  virtual core::Class_sp __class() const {                     \
    return aClass::static_class;                                        \
  }                                                                     \
  /* end LISP_CLASS */
    
#define LISP_ABSTRACT_CLASS(aNamespace, aPackage, aClass, aClassName,b1) \
  public:                                                               \
    typedef b1 Base;                                                    \
  typedef LispBases1<Base> Bases;                                       \
  COMMON_CLASS_PARTS(aNamespace, aPackage, aClass, aClassName); 
#endif

#if 0
#define DECLARE_INIT_GLOBALS() \
  public:                        \
    static void lisp_initGlobals(core::Lisp_sp lisp);
#endif

#if 0
#define __DEFAULT_INITIALIZATION() \
public:                            \
  inline void initialize() {       \
    this->Base::initialize();      \
  }
#endif


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



template <>
struct gctools::GCInfo<core::T_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  class T_O : public _RootDummyClass {
  private:
    friend class CoreExposer;
    LISP_ABSTRACT_CLASS(core, ClPkg, T_O, "T",::_RootDummyClass);
  public:
    bool isAInstanceOf(core::Class_sp mc);
  };

};

#include <clasp/core/cons.h>


//------------------------------------------------------------
//------------------------------------------------------------
//
//
namespace core {

  Class_sp instance_class(T_sp obj);

  class General_O : public T_O {
    LISP_CLASS(core, CorePkg, General_O, "General", T_O );
  public:
//    General_O& &operator=(const General_O &) { return *this; };

    virtual void sxhash_(HashGenerator &hg) const;
    virtual void sxhash_equal(HashGenerator &hg,LocationDependencyPtrT ptr) const;
    virtual void sxhash_equalp(HashGenerator &hg,LocationDependencyPtrT ptr) const {return this->sxhash_equal(hg,ptr);};
    
    virtual size_t templatedSizeof() const { return 0; };

      //! Initialize member variables and those that depend on sharedThis
    virtual void initialize();
    virtual bool isStandardObject() { return false; };
  /*! Objects can catch signals from Models
	 * but only Model's and subclasses can send them
	 */
    string className() const;

    T_sp deepCopy() const;

    inline bool eq(T_sp o) const {
      return (o.generalp() && this == o.unsafe_general());
    }
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
    virtual void fields(Record_sp record);
  /*! Return true if the two objects are the same object.
	 * If they aren't the same object for numbers and values
	 * the values are compared.
	 */
    virtual bool eql_(T_sp obj) const;

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
      if (cl__eql(this->asSmartPtr(), obj))
        return true;
      return this->operator<(obj);
    };
    virtual bool operator>(T_sp obj) const { return !this->operator<=(obj); };
    virtual bool operator>=(T_sp obj) const { return !this->operator<(obj); };

//    virtual void validate() const {};

    //! This is to support Derivable<T> classes in clbind
    virtual void* pointerToAlienWithin() { SUBIMP(); };
  
  public: // Instance protocol
  //! Some Class objects will create instances of classes different from themselves
    virtual core::Class_sp _instanceClass() const { return this->__class(); };

    virtual T_sp instanceClassSet(Class_sp mc);

  /*! Allocate space for (slots) slots and initialize them */
    virtual void initializeSlots(Fixnum stamp, size_t slots);

  /*! ECL slot handling, slots are indexed with integers */
    virtual T_sp instanceRef(size_t idx) const;
  /*! ECL slot handling, slots are indexed with integers */
    virtual T_sp instanceSet(size_t idx, T_sp val);

  /*! Set the signature (metaclass slot definitions) for the instance */
    virtual T_sp instanceSigSet();
  /*! Get the signature (metaclass slot definitions) for the instance */
    virtual T_sp instanceSig() const;

  /*! Return number of slots if instance of Instance_O otherwise return nil */
    virtual T_sp oinstancep() const { return _Nil<T_O>(); }; //
    bool instancep() const { return oinstancep().isTrue(); };
    virtual bool environmentp() const { return false; };
    virtual bool genericFunctionP() const { return false; };
  /*! Return number of slots if instance of Instance_O otherwise return nil */
    virtual T_sp ofuncallableInstanceP() const { return _Nil<T_O>(); }; //
    bool funcallableInstanceP() const { return ofuncallableInstanceP().isTrue(); };
    virtual Fixnum get_stamp_() const { lisp_error_no_stamp((void*)this); };
  };
};

#include <clasp/core/functor.h>
#include <clasp/core/creator.h>
#include <clasp/gctools/gcweak.h>


namespace core {
  template <class oclass>
    inline T_sp new_LispObject() {
    T_sp obj = oclass::static_creator->creator_allocate();
  //	GC_ALLOCATE(oclass,obj );
    return obj;
  };
};



namespace core {

  CL_LAMBDA(x y);
  CL_DECLARE();
  CL_DOCSTRING("eq");
  inline CL_DEFUN bool cl__eq(T_sp x, T_sp y) {
    return (x == y);
  };

  CL_LAMBDA(x y);
  CL_DECLARE();
  CL_DOCSTRING("eql");
  inline CL_DEFUN bool cl__eql(T_sp x, T_sp y) {
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
    } else if ( x.consp() ) {
      return x.raw_() == y.raw_();
    } else if ( x.generalp() ) {
      General_O* general = x.unsafe_general();
      return general->eql_(y);
    }
    SIMPLE_ERROR_SPRINTF("Bad eql comparison");
  };

  CL_LAMBDA(x y);
  CL_DECLARE();
  CL_DOCSTRING("Underlying eql. Only valid on general objects (not fixnums, single floats, characters, or conses)");
  inline CL_DEFUN bool core__eql_underlying(T_sp x, T_sp y) {
    General_O* general = x.unsafe_general();
    return general->eql_(y);
  };

  CL_LAMBDA(x y);
  CL_DECLARE();
  CL_DOCSTRING("equal");
  inline CL_DEFUN bool cl__equal(T_sp x, T_sp y) {
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
    } else if (x.consp() ) {
      Cons_O* cons = x.unsafe_cons();
      return cons->equal(y);
    } else if (x.generalp() ) {
      General_O* general = x.unsafe_general();
      return general->equal(y);
    }
    SIMPLE_ERROR_SPRINTF("Bad equal comparison");
  };

  extern int basic_compare(Number_sp na, Number_sp nb);

  bool cl__equalp(T_sp x, T_sp y);
};


namespace core {
  inline void clasp_sxhash(T_sp obj, HashGenerator &hg) {
    if (obj.fixnump()) {
      hg.addPart(obj.unsafe_fixnum());
      return;
    } else if (obj.single_floatp()) {
      hg.addPart((gc::Fixnum)::std::abs((int)::floor(obj.unsafe_single_float())));
      return;
    } else if (obj.characterp()) {
      hg.addPart(obj.unsafe_character());
      return;
    } else if (obj.consp() ) {
      Cons_O* cons = obj.unsafe_cons();
      cons->sxhash_(hg);
      return;
    } else if ( obj.generalp() ) {
      General_O* general = obj.unsafe_general();
      general->sxhash_(hg);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Handle sxhash_ for object");
  };
  inline void clasp_sxhash(T_sp obj, Hash1Generator &hg) {
    if (obj.fixnump()) {
      hg.addPart(obj.unsafe_fixnum());
      return;
    } else if (obj.single_floatp()) {
      hg.addPart((gc::Fixnum)::std::abs((int)::floor(obj.unsafe_single_float())));
      return;
    } else if (obj.characterp()) {
      hg.addPart(obj.unsafe_character());
      return;
    } else if (obj.consp() ) {
      Cons_O* cons = obj.unsafe_cons();
      hg.addPart((gc::Fixnum)cons);
      return;
    } else if ( obj.generalp() ) {
      General_O* general = obj.unsafe_general();
      hg.addPart((gc::Fixnum)general);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Handle sxhash_ for object");
  };
};

namespace core {
  Class_sp cl__class_of(T_sp obj);
  bool cl__eq(T_sp x, T_sp y);
  bool cl__eql(T_sp x, T_sp y);
  bool cl__equal(T_sp x, T_sp y);
  bool cl__equalp(T_sp x, T_sp y);

  List_sp encode(T_sp);
};


#include <clasp/core/glue.h>


#include <clasp/core/conditions.h>


/*! Used to indicate that methods should be inherited from sequence */
#define INHERIT_SEQUENCE

#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/tagged_cast_specializations.h>
#include <clasp/core/cxxObject.h>

#endif //]
