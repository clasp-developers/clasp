// ============================================================
// General arrays (hold T_sp)
//

namespace core { class SimpleVector_O; };
template <>
struct gctools::GCInfo<core::SimpleVector_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
namespace core {
  class SimpleVector_O;
  typedef template_SimpleVector<SimpleVector_O,T_sp,AbstractSimpleVector_O> specialized_SimpleVector;
  class SimpleVector_O : public specialized_SimpleVector {
    LISP_CLASS(core, ClPkg, SimpleVector_O, "simple-vector",AbstractSimpleVector_O);
    virtual ~SimpleVector_O() {};
  public:
    typedef specialized_SimpleVector TemplatedBase;
  public:
    static value_type default_initial_element(void) {return _Nil<T_O>();}
    static value_type from_object(T_sp obj) {return obj; };
    static T_sp to_object(const value_type& v) { return v; };
  public:
    // Simple vectors include pointers, so they can't have uninitialized contents.
    // Therefore we always pass initialElementSupplied=true.
  SimpleVector_O(size_t length, value_type initialElement=default_initial_element(), bool initialElementSupplied=true, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleVector_sp make(size_t length, T_sp initialElement=_Nil<T_O>(), bool initialElementSupplied=true, size_t initialContentsSize=0, const T_sp* initialContents=NULL,
                                bool static_vector_p = false) {
      auto bs = gctools::GC<SimpleVector_O>::allocate_container(static_vector_p,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
    // Used in one place in lisp.cc. FIXME: Maybe remove?
    static SimpleVector_sp make(const gc::Vec0<T_sp>& objs) {
      size_t len = objs.size();
      if (len == 0) return make(0);
      else return make(len, _Nil<T_O>(), true, len, &(objs[0]));
    }
  public:
    virtual T_sp type_of() const final {return Cons_O::createList(cl::_sym_simple_vector,clasp_make_fixnum(this->length()));};
    virtual T_sp element_type() const override { return cl::_sym_T_O; };
  public:
  public:
    virtual bool equal(T_sp other) const override { return this->eq(other);};
  };
}; // namespace core

namespace core
{
  class MDArrayT_O : public template_Array< MDArrayT_O, SimpleMDArrayT_O, SimpleVector_O, MDArray_O >
  {
    LISP_CLASS(core, CorePkg, MDArrayT_O, "MDArrayT",MDArray_O);
    virtual ~MDArrayT_O() {};
  public:
    typedef template_Array< MDArrayT_O, SimpleMDArrayT_O, SimpleVector_O, MDArray_O> TemplatedBase;
  public: // make array
  MDArrayT_O(size_t rank,
             List_sp dimensions,
             Array_sp data,
             bool displacedToP,
             Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  };
}; // namespace core


namespace core {
  class SimpleMDArrayT_O : public template_SimpleArray<SimpleMDArrayT_O,SimpleVector_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayT_O, "SimpleMDArrayT",SimpleMDArray_O);
    virtual ~SimpleMDArrayT_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayT_O,SimpleVector_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArrayT_O(size_t rank,
                   List_sp dimensions,
                   Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
}; // namespace core

namespace core {
FORWARD(ComplexVector_T);
class ComplexVector_T_O : public template_Vector<ComplexVector_T_O, SimpleVector_O, ComplexVector_O >
{
  LISP_CLASS(core, CorePkg, ComplexVector_T_O, "ComplexVector_T",ComplexVector_O);
  virtual ~ComplexVector_T_O() {};
public:
  typedef template_Vector<ComplexVector_T_O, SimpleVector_O, ComplexVector_O> TemplatedBase;
public: // make vector
    ComplexVector_T_O(size_t rank1,
                      size_t dimension,
                      T_sp fillPointer,
                      Array_sp data,
                      bool displacedToP,
                      Fixnum_sp displacedIndexOffset) : TemplatedBase(dimension, fillPointer, data,displacedToP, displacedIndexOffset ) {};

  static ComplexVector_T_sp make( size_t dimension,
                                  T_sp initialElement /* =_Nil<T_O>() */,
                                  T_sp fillPointer /* =_Nil<T_O>() */,
                                  T_sp dataOrDisplacedTo /* =_Nil<T_O>() */,
                                  bool displacedToP /* = false */,
                                  Fixnum_sp displacedIndexOffset /* = clasp_make_fixnum(0) */ )
  {
    LIKELY_if (dataOrDisplacedTo.nilp())
      dataOrDisplacedTo = SimpleVector_O::make(dimension,initialElement,true);
    ComplexVector_T_sp array = gctools::GC<ComplexVector_T_O>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    return array;
  }
  static smart_ptr_type make(size_t dimension,
                             simple_element_type initialElement,
                             bool initialElementSuppliedP,
                             T_sp fillPointer,
                             T_sp dataOrDisplacedTo,
                             bool displacedToP,
                             Fixnum_sp displacedIndexOffset) {
    (void)initialElementSuppliedP;
    return make(dimension, initialElement, fillPointer, dataOrDisplacedTo, displacedToP, displacedIndexOffset);
  }
  static ComplexVector_T_sp make(size_t dimension, T_sp initialElement) {
    return make(dimension,initialElement,_Nil<T_O>(),_Nil<T_O>(),false,clasp_make_fixnum(0));
  }
  static ComplexVector_T_sp make(size_t dimension, T_sp initialElement, T_sp fillPointer ) {
    return make(dimension,initialElement,fillPointer,_Nil<T_O>(),false,clasp_make_fixnum(0));
  }
 };
}; // namespace core
