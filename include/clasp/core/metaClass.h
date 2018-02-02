/*
    File: metaClass.h
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
#ifndef Class_H
#define Class_H

#include <list>
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/iterator_adaptors.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop
#include <clasp/core/array.h>
#include <clasp/core/object.fwd.h>
#include <clasp/core/numbers.fwd.h>
//#include <clasp/core/holder.h>
#include <clasp/core/instance.h>

#if 0
template <>
struct gctools::GCInfo<core::Class_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
#endif


namespace core {

  typedef Instance_O Class_O;
  typedef Instance_sp Class_sp;

  Fixnum_sp clasp_make_fixnum(gc::Fixnum v);
  
//
//  Class access functions for when we only have a forward
//  definition for the Class_O class
//

/*! Class spoofs ECL>>Instance_O classes by doing the following.

  virtual T_sp Class_O::instanceClass() is overloaded in StandardClass_O and BuiltInClass_O
  and they each return XXXX::___staticClass.
  This should never change because you should never change the meta classes of instances of StandardClass and BuiltInClass

  virtual T_sp Class_O::instanceSig() is overloaded in StandardClass_O and BuiltInClass_O
  and they each return XXXXClass::___staticClass->slots()
  This should never change because you should never change the class slots of StandardClass_O or BuiltInClass_O

  
*/

#define REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS 27
#define REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS 32 // was 28
#define REF_CLASS_NUMBER_OF_SLOTS_IN_DERIVABLE_CXX_CLASS REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS
  

#if 0
class Class_O : public StandardObject_O {
  struct metadata_bootstrap_class {};
  struct metadata_gc_do_not_move {};
  LISP_META_CLASS(::_lisp->_Roots._StandardClass);
  LISP_CLASS(core, ClPkg, Class_O, "class",StandardObject_O);
  //
  // Friend functions for bootup
  //
  friend class CoreExposer;
  template <typename u>
  friend void dump_info(Class_sp co, Lisp_sp prog);
  template <typename u>
  friend void define_base_class(Class_sp co, Class_sp cob, uint &i);
  template <typename u>
  friend BuiltInClass_sp hand_initialize_class(uint &classesHandInitialized, Lisp_sp prog, BuiltInClass_sp c);
  template <typename u>
  friend BuiltInClass_sp hand_initialize_allocatable_class(uint &classesHandInitialized, Lisp_sp prog, BuiltInClass_sp c);

 public:
  /*! A Fixnum that represents the object stamp(aka KIND) of each instance of this class */
  gctools::Stamp _instance_stamp;
  Class_sp  _MetaClass;
  /*! Callback function to allocate instances */
  SimpleVector_sp _Rack;
  /*! Mimic ECL Instance::sig */
  T_sp _Signature_ClassSlots;
  Creator_sp _theCreator;
//  size_t              _NumberOfSlots;
#ifdef METER_ALLOCATIONS
  // Keep track of allocations
  std::atomic<size_t> _allocation_counter;
  std::atomic<size_t> _allocation_total_size;
#endif
 public: // The hard-coded indexes above are defined below to be used by Class
  // These must match the +class-slots+ defined in hierarchy.lsp
private:
  void accumulateSuperClasses(HashTableEq_sp supers, VectorObjects_sp arrayedSupers, Class_sp mc);

public:
  static Class_sp create(Symbol_sp symbol,Class_sp metaClass);
  static Class_sp createUncollectable(gctools::Stamp is,Class_sp metaClass, size_t number_of_slots);

public:
  /*! This is a factory function that returns either a BuiltInClass or a StandardClass depending on the
	  type of metaClass */
  static Class_sp allocateRawClass(Class_sp orig, Class_sp metaClass, int slots);

public:
  void __setup_stage1_with_sharedPtr_lisp_sid(T_sp theThis, Symbol_sp instanceClassSymbol) {
    this->instanceSet(REF_CLASS_CLASS_NAME, instanceClassSymbol);
  }

  void __setup_stage2_with_classSymbol(Symbol_sp csid) {
    _OF();
  }

  void __setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp isid);

  /*! Setup the instance nil value */
  //	void setupInstanceNil();
public:
  CL_NAME("CORE:GET-INSTANCE-STAMP");
  CL_DEFMETHOD Fixnum get_instance_stamp() const { return this->_instance_stamp;}
  CL_NAME("CORE:REINITIALIZE-CLASS");
  CL_DEFMETHOD virtual void reinitialize_class() {
    this->_instance_stamp = gctools::NextStamp();
  }; 
private:
  void lowLevel_calculateClassPrecedenceList();
public: // Mimic CLOS classes that are represented by Instance_O
  void initializeSlots(Fixnum stamp, size_t slots);

  /*! Return this classes metaclass */
  virtual Class_sp _instanceClass() const;
  /*! Set this classes metaclass */
  T_sp instanceClassSet(Class_sp mc);

  /*! ECL slot handling, slots are indexed with integers */
  virtual T_sp instanceRef(size_t idx) const;
  /*! ECL slot handling, slots are indexed with integers */
  virtual T_sp instanceSet(size_t idx, T_sp val);

  /*! I don't know what this does, I'm mimicking ECL ecl>>instance.d>>instance_sig_set */
  virtual T_sp instanceSigSet();
  /*! I think this should just return the __staticClass->slots() I'm mimicking ECL ecl>>instance.d>>instance_sig */
  virtual T_sp instanceSig() const;

  T_sp slots() const { return this->instanceRef(REF_CLASS_SLOTS); };

  T_sp copyInstance() const;
public:
  void inheritDefaultAllocator(List_sp directSuperclasses);
  void setCreator(Creator_sp cb);

  CL_NAME("CORE:CLASS-CREATOR");
  CL_DEFMETHOD T_sp class_creator() const { return (bool)(this->_theCreator) ? gctools::As<core::T_sp>(this->_theCreator) : _Nil<core::T_O>(); };
  CL_LISPIFY_NAME("CORE:HAS-CREATOR");
  CL_DEFMETHOD   bool has_creator() const { return (bool)(this->_theCreator); };


  CL_DEFMETHOD Class_sp meta_class() const { return this->_MetaClass;};
  /*! I have GOT to clean up all this class-name stuff
	  Reduce the clutter to one function to get the name and one to set the name */

  void setName(Symbol_sp id) { this->instanceSet(REF_CLASS_CLASS_NAME, id); };
  Symbol_sp name() const { return gc::As<Symbol_sp>(this->instanceRef(REF_CLASS_CLASS_NAME)); };

  Fixnum stamp() const;
  void stamp_set(Fixnum s);

  Symbol_sp className() const;
  string classNameAsString() const;
  string instanceClassName() { return this->getPackagedName(); };
  string instanceClassName() const { return this->getPackagedName(); };

  CL_DEFMETHOD List_sp core__min_class_precedence_list() const { return List_sp(this->instanceRef(REF_CLASS_CLASS_PRECEDENCE_LIST));};
  
  /*! Return the name of the class with its Package name prefixed
	 */
  string getPackagedName() const;

  virtual string dumpInfo();

  /*! Return the name of the package that this class belongs to */
  string getPackageName() const;

  //	virtual Function_sp getMethodOrNil(Symbol_sp methodSymbol, T_sp receiver );
  //	virtual void addMethod(Symbol_sp methodSymbol, Function_sp exec );

  template <typename oclass>
  bool isSubClassOf() const {
    return this->isSubClassOf(lisp_classFromClassSymbol(oclass::static_classSymbol()));
  }

  virtual bool isSubClassOf(Class_sp mc) const;

  //	virtual bool isOfClass(Class_sp mc) const;

  void setInstanceBaseClasses(List_sp classes);
  void addInstanceBaseClassDoNotCalculateClassPrecedenceList(Symbol_sp cl);
  void addInstanceBaseClass(Symbol_sp cl);
  string __repr__() const;

  /*! Allocate an object of this class
	  But don't call initialize!!!!!
	*/
//  virtual T_sp allocate_newNil();
  virtual T_sp allocate_newClass(Class_sp metaClass, int slots);

  T_sp make_instance();

  /*! Return the direct superclasses */
  List_sp directSuperclasses() const;

  void appendDirectSuperclassAndResetClassPrecedenceList(Class_sp superClass);

  /*! Return the unboundValue for this class - use lazy initialization to define the unboundValue
	  if it hasn't been defined yet */
  //	T_sp unboundValue();

  //    virtual T_sp new_Instance(Function_sp e, List_sp args,  Environment_sp environ, Lisp_sp lisp);
  virtual void describe(T_sp stream);

  /*! predicate if this is a BuiltInClass class */
  virtual bool builtInClassP() const { return this == &*lisp_builtInClass(); };

  /*! predicate if this is a raw C++ class that is wrapped with clbind
          - it can only be used to derive other classes if cxxDerivableClassP is true */
  virtual bool cxxClassP() const { return false; };

  /*! cxxDerivableClass is a class that inherits from a raw C++ class and
          the clbind::Adapter class - this allows it to be derived from */
  virtual bool cxxDerivableClassP() const { return false; };

  /*! primaryCxxDerivableClassP is a predicate that returns true if
          this class is the primary derivable C++ class */
  virtual bool primaryCxxDerivableClassP() const { return false; };

#ifdef METER_ALLOCATIONS
  CL_DEFMETHOD T_mv allocation_meter() const {
    return Values(clasp_make_fixnum(this->_allocation_counter), clasp_make_fixnum(this->_allocation_total_size));
  }
#endif
  explicit Class_O(gctools::Stamp is,Class_sp metaClass, size_t slots) : Class_O::Base(), _instance_stamp(is),
#ifdef METER_ALLOCATIONS
    _allocation_counter(0),
    _allocation_total_size(0),
#endif
    _MetaClass(metaClass),
    _Signature_ClassSlots(_Unbound<T_O>()),
    _theCreator()
//    ,_NumberOfSlots(slots)
    {};

  explicit Class_O();

  virtual ~Class_O(){};
};

#endif
};



namespace core {
  // Specialize BuiltInObjectCreator for DummyStandardClass_O
    template <>
    class BuiltInObjectCreator<Class_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
  public:
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<Class_O>); };
    bool creates_classes() const { return true; };
    virtual core::T_sp creator_allocate() {
      // BuiltInObjectCreator<Class_O> uses a different allocation method
      // that assigns the next Clos Stamp to the new Class
      GC_ALLOCATE_VARIADIC(Class_O, obj, lisp_standard_class() /*,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS */);
//      printf("%s:%d  creating class\n", __FILE__, __LINE__ );
      return obj;
    }
    virtual void searcher(){};
  };
};

namespace core {

/*!Return true if low is a subclass of high */
bool core__subclassp(T_sp low, T_sp high);

/*! Return true if the object is of the class _class */
bool af_ofClassP(T_sp object, T_sp _class);


};
#endif
