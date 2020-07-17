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

/*! State that a class has an external base class.
  This is scraped out by registerClasses.py
*/
#define EXTERNAL_BASE_CLASS(m)

namespace core {

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
  FORWARD(Cons);
  FORWARD(General);

};

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

class HashGeneratorBase {
};

typedef enum { hashEmpty, hashFilling, hashFull } FillingType;
class Hash1Generator : public HashGeneratorBase {
 public:
  uint64_t _Part;
  bool     _PartIsPointer;
 public:
  // Add a value  
  bool addValue(Fixnum part) {
    this->_Part = part;
    this->_PartIsPointer = false;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d Added part --> %ld\n", __FILE__, __LINE__, part);
    }
#endif
    return true;
  }
  /* Add the limbs of the bignum - the value*/
  bool addValue(const mpz_class &bignum);

    // Add an address - this may need to work with location dependency
  bool addConsAddress(void* part) {
    ASSERT(!(((uintptr_t)part)&gctools::ptag_mask));
    this->_Part = (uintptr_t)part;
    this->_PartIsPointer = true;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d Added part --> %ld\n", __FILE__, __LINE__, part);
    }
#endif
    return true;
  }

  // Add an address - this may need to work with location dependency
  bool addGeneralAddress(const General_O* part);
  
  // Hash1Generator is always filling
  bool isFilling() const { return true; };
  void hashObject(T_sp obj);
  
  gc::Fixnum rawhash() const {
    gc::Fixnum hash(5381);
    hash = (gc::Fixnum)hash_word(hash,(uintptr_t)this->_Part);
    return hash;
  }
  
  gc::Fixnum hash() const {
    return rawhash();
  }
  
  gc::Fixnum hashBound(gc::Fixnum bound) const {
    gc::Fixnum hash = rawhash();
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d  final hash = %lu\n", __FILE__, __LINE__, hash);
    }
#endif
    return ((uintptr_t)hash) % bound;
  }
#if 0 // def USE_MPS
  void addAddressesToLocationDependency(mps_ld_t ld) {
    if (this->_PartIsPointer) {
      mps_ld_add(ld,global_arena,(mps_addr_t)this->_Part);
    }
  };
  bool isstale(mps_ld_t ld) {
    if (this->_PartIsPointer) {
      return mps_ld_isstale(ld,global_arena,(mps_addr_t)this->_Part);
    }
    return false;
  }
#endif
};
 
//#define DEBUG_HASH_GENERATOR
class HashGenerator : public HashGeneratorBase {
  static const int MaxParts = 32;
  static const int MaxDepth = 8;
private:
  int _Depth;
  int _NextPartIndex;
  int _NextAddressIndex;
  Fixnum _Parts[MaxParts];
#ifdef DEBUG_HASH_GENERATOR
  bool _debug;
#endif
public:
  HashGenerator(bool debug = false) : _Depth(0), _NextPartIndex(0), _NextAddressIndex(MaxParts-1)
#ifdef DEBUG_HASH_GENERATOR
                                    , _debug(debug)
#endif
  {};

    /*! Return true if can still accept parts */
  bool isFilling() const {
    return (this->_NextPartIndex <= this->_NextAddressIndex) || (this->_Depth>= MaxDepth);
  }

  /*! Return true if not accepting any more parts */
  bool isFull() const {
    return this->_NextPartIndex > this->_NextAddressIndex;
  }

  bool addValue(Fixnum part) {
    if (this->isFull()) {
      return false;
    }
    this->_Parts[this->_NextPartIndex] = part;
#ifdef DEBUG_HASH_GENERATOR
    if (this->_debug) {
      printf("%s:%d Added part[%d] --> %ld\n", __FILE__, __LINE__, this->_NextPartIndex, part);
    }
#endif
    ++this->_NextPartIndex;
    return true;
  }

  bool addValue0(Fixnum part) {
    this->_Parts[0] = part;
    this->_NextPartIndex=1;
    return true;
  }

  bool addConsAddress(void* part) {
    ASSERT(!(((uintptr_t)part)&gctools::ptag_mask));
    if (this->isFull()) {
      return false;
    }
    this->_Parts[this->_NextAddressIndex] = (uintptr_t)part;
    this->_NextAddressIndex--;
    return true;
  }

  bool addGeneralAddress(const General_O* part);
  
  /*Add the bignum across multiple parts, return true if everything was added */
  bool addValue(const mpz_class &bignum);

  gc::Fixnum rawhash() const {
    gc::Fixnum hash = 5381;
    for (int i = 0; i < this->_NextPartIndex; i++) {
      hash = (gc::Fixnum)hash_word((uintptr_t)hash, (uintptr_t) this->_Parts[i]);
#ifdef DEBUG_HASH_GENERATOR
      if (this->_debug) {
        printf("%s:%d  calculated hash = %lu with part[%d] --> %lu\n", __FILE__, __LINE__, hash, i, this->_Parts[i]);
      }
#endif
    }
    for (int ia = this->_NextAddressIndex+1; ia < MaxParts; ia++) {
      hash = (gc::Fixnum)hash_word((uintptr_t)hash, (uintptr_t) this->_Parts[ia]);
#ifdef DEBUG_HASH_GENERATOR
      if (this->_debug) {
        printf("%s:%d  calculated hash = %lu with part[%d] --> %lu\n", __FILE__, __LINE__, hash, i, this->_Parts[ia]);
      }
#endif
    }
    return hash;
  }

  gc::Fixnum hashBound(gc::Fixnum bound = 0) const {
    gc::Fixnum hash = this->rawhash();
    if (bound)
      return ((uintptr_t)hash) % bound;
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
    ss << "|-";
    for (int i = this->_NextAddressIndex+1; i < MaxParts; i++) {
      ss << this->_Parts[i] << "-";
    }
    ss << ">";
    return ss.str();
  }

  void hashObject(T_sp obj);

#if 0 // def USE_MPS
  void addAddressesToLocationDependency(mps_ld_t ld) {
    for (int ia = this->_NextAddressIndex+1; ia < MaxParts; ia++) {
      mps_ld_add(ld,global_arena,(mps_addr_t)this->_Parts[ia]);
    }
  };
  bool isstale(mps_ld_t ld) {
    for (int ia = this->_NextAddressIndex+1; ia < MaxParts; ia++) {
      if (mps_ld_isstale(ld,global_arena,(mps_addr_t)this->_Parts[ia])) return true;
    }
    return false;
  }
#endif

};
};


namespace core {
Instance_sp lisp_getStaticClass(gctools::Header_s::StampWtagMtag);
void lisp_setStaticClass(gctools::Header_s::StampWtagMtag value, Instance_sp class_);
Symbol_sp lisp_getStaticClassSymbol(gctools::Header_s::StampWtagMtag value);
void lisp_setStaticClassSymbol(gctools::Header_s::StampWtagMtag value, Symbol_sp class_);
Creator_sp lisp_getStaticInstanceCreator(gctools::Header_s::StampWtagMtag value);
void lisp_setStaticInstanceCreator(gctools::Header_s::StampWtagMtag value, Creator_sp creator);
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
  static core::Creator_sp staticCreator()                               \
    { return core::lisp_getStaticInstanceCreator(oClass::static_StampWtagMtag);} \
  static void setStaticClass(core::Instance_sp ci)                         \
  { return core::lisp_setStaticClass(oClass::static_StampWtagMtag,ci);} \
  static core::Instance_sp staticClass()                                  \
    { return core::lisp_getStaticClass(oClass::static_StampWtagMtag);} \
 public:                                                                \
  typedef oClass my_type;                                               \
  typedef gctools::smart_ptr<oClass> smart_ptr_type;                 \
 public:                                                                \
  static gctools::Header_s::StampWtagMtag static_StampWtagMtag;                   \
 public:                                                                \
 static void set_static_class_symbol(core::Symbol_sp symbol)                \
 { core::lisp_setStaticClassSymbol(oClass::static_StampWtagMtag,symbol); }; \
 static void set_static_creator(gctools::smart_ptr<core::Creator_O> al) \
 { core::lisp_setStaticInstanceCreator(oClass::static_StampWtagMtag,al); }; \
  static string static_packageName() { return oPackage; };              \
  static string static_className()                                      \
  { return core::lispify_symbol_name(oclassName); };                    \
  static core::Symbol_sp static_classSymbol()                           \
  { return core::lisp_getStaticClassSymbol(oClass::static_StampWtagMtag);};     \
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
  virtual core::Instance_sp __class() const {                           \
    return core::lisp_getStaticClass(aClass::static_StampWtagMtag);       \
  }                                                                     \
  /* end LISP_CLASS */
    
#define LISP_ABSTRACT_CLASS(aNamespace, aPackage, aClass, aClassName,b1) \
  public:                                                               \
    typedef b1 Base;                                                    \
  typedef LispBases1<Base> Bases;                                       \
  COMMON_CLASS_PARTS(aNamespace, aPackage, aClass, aClassName); 
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
  };

};

#include <clasp/core/cons.h>


//------------------------------------------------------------
//------------------------------------------------------------
//
//
namespace core {

  Instance_sp instance_class(T_sp obj);

  class General_O : public T_O {
    LISP_CLASS(core, CorePkg, General_O, "General", T_O );
  public:
    size_t _badge;
  public:
    General_O() : _badge(my_thread->random()) {};
  public:
    virtual void sxhash_(HashGenerator &hg) const;
    virtual void sxhash_equal(HashGenerator &hg) const;
    virtual void sxhash_equalp(HashGenerator &hg) const {return this->sxhash_equal(hg);};
    
    virtual size_t templatedSizeof() const { return 0; };

      //! Initialize member variables and those that depend on sharedThis
    virtual void initialize();
  /*! Objects can catch signals from Models
	 * but only Model's and subclasses can send them
	 */
    string className() const;

    inline bool eq(T_sp o) const {
      return (o.generalp() && this == o.unsafe_general());
    }
        //! A technical string representation
    virtual string description() const;
  //! Just describe the contents
    virtual string descriptionOfContents() const;
  //! A pretty-print representation
    virtual string __repr__() const { return this->description(); };
  //! Common Lisp __write__(T_sp strm)
    virtual void __write__(T_sp strm) const;
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

    //! This is to support Derivable<T> classes in clbind
    virtual void* pointerToAlienWithin() { SUBIMP(); };
  
  public: // Instance protocol
  //! Some Class objects will create instances of classes different from themselves
    virtual core::Instance_sp _instanceClass() const { return this->__class(); };

    virtual T_sp instanceClassSet(Instance_sp mc);

  /*! ECL slot handling, slots are indexed with integers */
    virtual T_sp instanceRef(size_t idx) const;
  /*! ECL slot handling, slots are indexed with integers */
    virtual T_sp instanceSet(size_t idx, T_sp val);

  /*! Set the signature (metaclass slot definitions) for the instance */
    virtual T_sp instanceSigSet();
  /*! Get the signature (metaclass slot definitions) for the instance */
    virtual T_sp instanceSig() const;

    virtual Fixnum get_stamp_() const { lisp_error_no_stamp((void*)this); };
  };
};

#include <clasp/core/functor.h>
#include <clasp/core/creator.h>
#include <clasp/gctools/gcweak.h>


namespace core {
  template <class oclass>
    inline T_sp new_LispObject() {
    T_sp obj = oclass::staticCreator()->creator_allocate();
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
      if (x.raw_() == y.raw_()) return true;
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
      hg.addValue(obj.unsafe_fixnum());
      return;
    } else if (obj.single_floatp()) {
      hg.addValue((gc::Fixnum)::std::abs((int)::floor(obj.unsafe_single_float())));
      return;
    } else if (obj.characterp()) {
      hg.addValue(obj.unsafe_character());
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
      hg.addValue(obj.unsafe_fixnum());
      return;
    } else if (obj.single_floatp()) {
      hg.addValue((gc::Fixnum)::std::abs((int)::floor(obj.unsafe_single_float())));
      return;
    } else if (obj.characterp()) {
      hg.addValue(obj.unsafe_character());
      return;
    } else if (obj.consp() ) {
      Cons_O* cons = obj.unsafe_cons();
      hg.addConsAddress((void*)cons);
      return;
    } else if ( obj.generalp() ) {
      General_O* general = obj.unsafe_general();
      hg.addGeneralAddress(general);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Handle sxhash_ for object");
  };
};

namespace core {
  Instance_sp cl__class_of(T_sp obj);
  bool cl__eq(T_sp x, T_sp y);
  bool cl__eql(T_sp x, T_sp y);
  bool cl__equal(T_sp x, T_sp y);
  bool cl__equalp(T_sp x, T_sp y);

  List_sp encode(T_sp);
};


namespace core {
SMART(AtomicT_Holder);
class AtomicT_Holder_O : public core::General_O {
  LISP_CLASS(core, CorePkg, AtomicT_Holder_O, "AtomicT_Holder",core::General_O);
public:
  static AtomicT_Holder_sp create(T_sp cl) {
    GC_ALLOCATE_VARIADIC(AtomicT_Holder_O,ch,cl);
    return ch;
  }
  std::atomic<T_sp> _Object;
public:
  bool object_unboundp() const;
  T_sp object_get() const;
  void object_set(T_sp cl);
  void object_makunbound();
  
  explicit AtomicT_Holder_O(T_sp c) : _Object(c) {};
};
};

namespace core {
SMART(AtomicFixnumHolder);
class AtomicFixnumHolder_O : public core::General_O {
  LISP_CLASS(core, CorePkg, AtomicFixnumHolder_O, "AtomicFixnumHolder",core::General_O);
public:
  static AtomicFixnumHolder_sp make_atomic_fixnum(T_sp cl);
  std::atomic<Fixnum> _Object;
public:
  T_sp atomic_fixnum_get() const;
  void atomic_fixnum_set_unsafe(T_sp cl);
  void atomic_fixnum_incf_unsafe(T_sp val);
  explicit AtomicFixnumHolder_O(Fixnum c) : _Object(c) {};
};
};


namespace core {
// A dummy class for unused tag 
class Unused_dummy_O : public T_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, Unused_dummy_O, "UNUSED",T_O);
};
};


#include <clasp/core/glue.h>



#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/cxxObject.h>

#endif //]
