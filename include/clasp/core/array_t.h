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
    static value_type initial_element_from_object(T_sp obj, bool supplied) {return supplied ? obj : _Nil<T_O>();};
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
  public:
    // Specific to SimpleVector_O
    virtual void __write__(T_sp stream) const final;
  public:
    virtual T_sp type_of() const final {return Cons_O::createList(cl::_sym_simple_vector,clasp_make_fixnum(this->length()));};
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return cl::_sym_T_O; };
  public:
    virtual clasp_elttype elttype() const { return clasp_aet_object; };
    virtual T_sp arrayElementType() const override { return cl::_sym_T_O; };
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
  public: // make vector
  MDArrayT_O( size_t dummy_rank_1,
              size_t dimension,
              T_sp fillPointer,
              Array_sp data,
              bool displacedToP,
              Fixnum_sp displacedIndexOffset) : TemplatedBase( Rank1(), dimension, fillPointer, data,displacedToP, displacedIndexOffset ) {};
    static MDArrayT_sp make( size_t dimension,
                             T_sp initialElement /* =_Nil<T_O>() */,
                             T_sp fillPointer /* =_Nil<T_O>() */,
                             T_sp dataOrDisplacedTo /* =_Nil<T_O>() */,
                             bool displacedToP /* = false */,
                             Fixnum_sp displacedIndexOffset /* = clasp_make_fixnum(0) */ )
    {
      LIKELY_if ( dataOrDisplacedTo.nilp() )
      {
        dataOrDisplacedTo = SimpleVector_O::make(dimension,initialElement,true);
      }
      MDArrayT_sp array = gctools::GC<MDArrayT_O>::allocate_container(false,1,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
    static MDArrayT_sp make(size_t dimension, T_sp initialElement) {
      return make(dimension,initialElement,_Nil<T_O>(),_Nil<T_O>(),false,clasp_make_fixnum(0));
    }
    static MDArrayT_sp make(size_t dimension, T_sp initialElement, T_sp fillPointer ) {
      return make(dimension,initialElement,fillPointer,_Nil<T_O>(),false,clasp_make_fixnum(0));
    }

  public: // make array
  MDArrayT_O(size_t rank,
             List_sp dimensions,
             Array_sp data,
             bool displacedToP,
             Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
    static MDArrayT_sp make_multi_dimensional(List_sp dim_desig, T_sp initialElement, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = SimpleVector_O::make(arrayTotalSize,initialElement,true);
      }
      MDArrayT_sp array = gctools::GC<MDArrayT_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
  public: // specific to MDArrayT_O
    static MDArrayT_sp create(const gctools::Vec0<T_sp> &objs);
  public:
//    virtual bool equalp(T_sp o) const final;
  };
}; // namespace core


namespace core {
  class SimpleMDArrayT_O : public template_SimpleArray<SimpleMDArrayT_O,SimpleVector_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayT_O, "SimpleMDArrayT",SimpleMDArray_O);
    virtual ~SimpleMDArrayT_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayT_O,SimpleVector_O,SimpleMDArray_O> TemplatedBase;
  public: // make vector
  SimpleMDArrayT_O(size_t rank, size_t dimension, Array_sp data) : TemplatedBase(dimension,data) {};
    static SimpleMDArrayT_sp make(size_t dimension, T_sp initialElement/*=_Nil<T_O>()*/, T_sp data/*=_Nil<T_O>()*/) {
      LIKELY_if (data.nilp()) {
        data = SimpleVector_O::make(dimension,initialElement,true);
      }
      SimpleMDArrayT_sp array = gctools::GC<SimpleMDArrayT_O>::allocate_container(false,1,dimension,gc::As_unsafe<Array_sp>(data));
      return array;
    }
    static SimpleMDArrayT_sp make(size_t dimension, T_sp initialElement) {
      return make(dimension,initialElement,_Nil<T_O>());
    }
  public: // make array
  SimpleMDArrayT_O(size_t rank,
                   List_sp dimensions,
                   Array_sp data) : TemplatedBase(rank,dimensions,data) {};
    static SimpleMDArrayT_sp make_multi_dimensional(List_sp dim_desig, T_sp initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = SimpleVector_O::make(arrayTotalSize,initialElement,true);
      }
      SimpleMDArrayT_sp array = gctools::GC<SimpleMDArrayT_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
      return array;
    }
  public:
//    virtual bool equalp(T_sp o) const final;
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
  ComplexVector_T_O( size_t dummy_rank_1,
                     size_t dimension,
                     T_sp fillPointer,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset) : TemplatedBase( Rank1(), dimension, fillPointer, data,displacedToP, displacedIndexOffset ) {};

  static ComplexVector_T_sp make( size_t dimension,
                                  T_sp initialElement /* =_Nil<T_O>() */,
                                  T_sp fillPointer /* =_Nil<T_O>() */,
                                  T_sp dataOrDisplacedTo /* =_Nil<T_O>() */,
                                  bool displacedToP /* = false */,
                                  Fixnum_sp displacedIndexOffset /* = clasp_make_fixnum(0) */ )
  {
    LIKELY_if ( dataOrDisplacedTo.nilp() )
    {
      dataOrDisplacedTo = SimpleVector_O::make(dimension,initialElement,true);
    }
    ComplexVector_T_sp array = gctools::GC<ComplexVector_T_O>::allocate_container(false,1,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    return array;
  }
  static ComplexVector_T_sp make(size_t dimension, T_sp initialElement) {
    return make(dimension,initialElement,_Nil<T_O>(),_Nil<T_O>(),false,clasp_make_fixnum(0));
  }
  static ComplexVector_T_sp make(size_t dimension, T_sp initialElement, T_sp fillPointer ) {
    return make(dimension,initialElement,fillPointer,_Nil<T_O>(),false,clasp_make_fixnum(0));
  }

public: // specific to ComplexVector_T_O
  static ComplexVector_T_sp create(const gctools::Vec0<T_sp> &objs);
public:
//    virtual bool equalp(T_sp o) const final;
 };
}; // namespace core
