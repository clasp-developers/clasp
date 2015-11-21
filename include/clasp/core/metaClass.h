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
#include <clasp/core/foundation.h>
#include <clasp/core/object.fwd.h>
#include <clasp/core/numbers.fwd.h>
#include <clasp/core/vectorObjectsWithFillPtr.fwd.h>
#include <clasp/core/specializer.h>
#include <clasp/core/holder.h>

namespace core {

//
//  Class access functions for when we only have a forward
//  definition for the Class_O class
//
SMART(Class);

/*! Class spoofs ECL>>Instance_O classes by doing the following.

  virtual T_sp Class_O::instanceClass() is overloaded in StandardClass_O and BuiltInClass_O
  and they each return XXXX::___staticClass.
  This should never change because you should never change the meta classes of instances of StandardClass and BuiltInClass

  virtual T_sp Class_O::instanceSig() is overloaded in StandardClass_O and BuiltInClass_O
  and they each return XXXXClass::___staticClass->slots()
  This should never change because you should never change the class slots of StandardClass_O or BuiltInClass_O

  
*/
class Class_O : public Specializer_O {
  struct metadata_bootstrap_class {};
  struct metadata_gc_do_not_move {};

  LISP_META_CLASS(StandardClass);
  LISP_BASE1(Specializer_O);
  LISP_CLASS(core, ClPkg, Class_O, "class");
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
#if defined(XML_ARCHIVE)
  void archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();

public: // The hard-coded indexes above are defined below to be used by Class
  // These must match the +class-slots+ defined in hierarchy.lsp
  static const int REF_EQL_SPECIALIZER_FLAG = 0;
  static const int REF_SPECIALIZER_DIRECT_METHODS = 1;
  static const int REF_SPECIALIZER_DIRECT_GENERIC_FUNCTIONS = 2;
  static const int REF_CLASS_NAME = 3;
  static const int REF_DIRECT_SUPERCLASSES = 4;
  static const int REF_DIRECT_SUBCLASSES = 5;
  static const int REF_SLOTS = 6;
  static const int REF_CLASS_PRECEDENCE_LIST = 7;
  static const int REF_DIRECT_SLOTS = 8;
  static const int REF_DIRECT_DEFAULT_INITARGS = 9;
  static const int REF_DEFAULT_INITARGS = 10;
  static const int REF_FINALIZED = 11;
  static const int REF_DOCSTRING = 12;
  static const int REF_SIZE = 13;
  static const int REF_SEALEDP = 14;
  static const int REF_PROTOTYPE = 15;
  static const int REF_DEPENDENTS = 16;
  static const int REF_VALID_INITARGS = 17;
  static const int REF_SLOT_TABLE = 18;
  static const int REF_LOCATION_TABLE = 19;
  static const int REF_OPTIMIZE_SLOT_ACCESS = 20;
  static const int REF_FORWARD = 21;
  static const int REF_NUMBER_OF_SLOTS_IN_CLASSES = 22;

private:
  void accumulateSuperClasses(HashTableEq_sp supers, VectorObjectsWithFillPtr_sp arrayedSupers, Class_sp mc);

public:
  /*! NumberOfClassSlots has to match the number of entries in
	  ECL clos::+class-slots+.  This is checked by a call to the function MAKE-SURE-CLOS-CLASS-SLOTS-MATCH-META-CLASS
	  These slots are accessed with instanceRef and instanceSet methods and
	  some slot accesses are redirected to C++ instance variables like _DirectSubClasses.
	*/
  static const int NumberOfClassSlots = 20; // Corresponds to the number of entries in

public:
  /*! Mimic ECL Instance::sig */
  T_sp _Signature_ClassSlots;
  /*! Callback function to allocate instances */
  gc::tagged_pointer<Creator> _theCreator;
  gctools::Vec0<T_sp> _MetaClassSlots;

public:
  /*! This is a factory function that returns either a BuiltInClass or a StandardClass depending on the
	  type of metaClass */
  static Class_sp allocateRawClass(Class_sp orig, Class_sp metaClass, int slots);

public:
  void __setup_stage1_with_sharedPtr_lisp_sid(T_sp theThis, Lisp_sp lisp, Symbol_sp instanceClassSymbol) {
    this->instanceSet(REF_CLASS_NAME, instanceClassSymbol);
  }

  void __setup_stage2_with_classSymbol(Symbol_sp csid) {
    _OF();
  }

  void __setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp isid);

  /*! Setup the instance nil value */
  //	void setupInstanceNil();
public:
private:
  void lowLevel_calculateClassPrecedenceList();

public: // Mimic CLOS classes that are represented by Instance_O
  void initializeSlots(int slots);

  /*! Return this classes metaclass */
  virtual Class_sp _instanceClass() const;
  /*! Set this classes metaclass */
  T_sp instanceClassSet(Class_sp mc);

  /*! ECL slot handling, slots are indexed with integers */
  virtual T_sp instanceRef(int idx) const;
  /*! ECL slot handling, slots are indexed with integers */
  virtual T_sp instanceSet(int idx, T_sp val);

  /*! I don't know what this does, I'm mimicking ECL ecl>>instance.d>>instance_sig_set */
  virtual T_sp instanceSigSet();
  /*! I think this should just return the __staticClass->slots() I'm mimicking ECL ecl>>instance.d>>instance_sig */
  virtual T_sp instanceSig() const;

  T_sp slots() const { return this->instanceRef(REF_SLOTS); };

public:
  void inheritDefaultAllocator(List_sp directSuperclasses);
  void setCreator(gc::tagged_pointer<Creator> cb) { this->_theCreator = cb; };
  gc::tagged_pointer<Creator> getCreator() const { return this->_theCreator; };
  bool hasCreator() const { return (bool)(this->_theCreator); };

  /*! I have GOT to clean up all this class-name stuff
	  Reduce the clutter to one function to get the name and one to set the name */

  void setName(Symbol_sp id) { this->instanceSet(REF_CLASS_NAME, id); };
  Symbol_sp name() const { return gc::As<Symbol_sp>(this->instanceRef(REF_CLASS_NAME)); };

  Symbol_sp className() const;
  string classNameAsString() const;
#if 0
	virtual Symbol_sp classNameSymbol() const {return this->name();};
	virtual Symbol_sp classSymbol() const;
	virtual Symbol_sp instanceClassSymbol() const { return this->name(); };
#endif
  string instanceClassName() { return this->getPackagedName(); };
  string instanceClassName() const { return this->getPackagedName(); };

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
  virtual T_sp allocate_newNil();

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
  virtual bool builtInClassP() const { return false; };

  /*! predicate if this is a raw C++ class that is wrapped with clbind
          - it can only be used to derive other classes if cxxDerivableClassP is true */
  virtual bool cxxClassP() const { return false; };

  /*! cxxDerivableClass is a class that inherits from a raw C++ class and
          the clbind::Adapter class - this allows it to be derived from */
  virtual bool cxxDerivableClassP() const { return false; };

  /*! primaryCxxDerivableClassP is a predicate that returns true if
          this class is the primary derivable C++ class */
  virtual bool primaryCxxDerivableClassP() const { return false; };

  explicit Class_O();
  virtual ~Class_O(){};
};
};
template <>
struct gctools::GCInfo<core::Class_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

/*!Return true if low is a subclass of high */
bool af_subclassp(T_sp low, T_sp high);

/*! Return true if the object is of the class _class */
bool af_ofClassP(T_sp object, T_sp _class);

class InstanceCreator : public Creator {
  FRIEND_GC_SCANNER(core::InstanceCreator);

public:
  Symbol_sp _className;

public:
  DECLARE_onHeapScanGCRoots();

public:
  DISABLE_NEW();
  InstanceCreator(Symbol_sp className) : _className(className){};
  void describe() const {
    printf("InstanceAllocatorFunctor for class %s\n", _rep_(this->_className).c_str());
  };
  T_sp allocate();
  virtual size_t templatedSizeof() const { return sizeof(InstanceCreator); };
};
};
TRANSLATE(core::Class_O);
#endif
