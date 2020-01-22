// ============================================================
// Arrays specialized for SPECIALIZE_ME
//
namespace core {
  FORWARD(SimpleVector_SPECIALIZE_ME);
  FORWARD(MDArray_SPECIALIZE_ME);
  FORWARD(SimpleMDArray_SPECIALIZE_ME);
  FORWARD(ComplexVector_SPECIALIZE_ME);
};
template <>
struct gctools::GCInfo<core::SimpleVector_SPECIALIZE_ME_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  class SimpleVector_SPECIALIZE_ME_O;
  typedef template_SimpleVector<SimpleVector_SPECIALIZE_ME_O,SPECIALIZE_ME,AbstractSimpleVector_O> specialized_SimpleVector_SPECIALIZE_ME;
  class SimpleVector_SPECIALIZE_ME_O : public specialized_SimpleVector_SPECIALIZE_ME {
    LISP_CLASS(core, CorePkg,SimpleVector_SPECIALIZE_ME_O, "SimpleVector_SPECIALIZE_ME",AbstractSimpleVector_O);
    virtual ~SimpleVector_SPECIALIZE_ME_O() {};
  public:
    typedef specialized_SimpleVector_SPECIALIZE_ME TemplatedBase;
  public:
    static value_type default_initial_element(void) {return 0;}
    static value_type from_object(T_sp obj) { return clasp_to_SPECIALIZE_ME(obj); };
    static T_sp to_object(const value_type& v) { return clasp_make_SPECIALIZE_ME(v); };
  public:
  SimpleVector_SPECIALIZE_ME_O(size_t length, value_type initialElement=value_type(),
                          bool initialElementSupplied=false,
                          size_t initialContentsSize=0,
                          const value_type* initialContents=NULL)
    : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static smart_ptr_type make(size_t length,
                               value_type initialElement=value_type(),
                               bool initialElementSupplied=false,
                               size_t initialContentsSize=0,
                               const value_type* initialContents=NULL,
                               bool static_vector_p = false) {
      auto bs = gctools::GC<my_type>::allocate_container(static_vector_p,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
  public:
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return core::_sym_SPECIALIZE_ME;};
  };
};


namespace core {
  class MDArray_SPECIALIZE_ME_O : public template_Array<MDArray_SPECIALIZE_ME_O,SimpleMDArray_SPECIALIZE_ME_O,SimpleVector_SPECIALIZE_ME_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_SPECIALIZE_ME_O, "MDArray_SPECIALIZE_ME",MDArray_O);
    virtual ~MDArray_SPECIALIZE_ME_O() {};
  public:
    typedef template_Array<MDArray_SPECIALIZE_ME_O,SimpleMDArray_SPECIALIZE_ME_O,SimpleVector_SPECIALIZE_ME_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_SPECIALIZE_ME_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(arrayTotalSize,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
  };
};

namespace core {
  class SimpleMDArray_SPECIALIZE_ME_O : public template_SimpleArray<SimpleMDArray_SPECIALIZE_ME_O,SimpleVector_SPECIALIZE_ME_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_SPECIALIZE_ME_O, "SimpleMDArray_SPECIALIZE_ME",SimpleMDArray_O);
    virtual ~SimpleMDArray_SPECIALIZE_ME_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_SPECIALIZE_ME_O,SimpleVector_SPECIALIZE_ME_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArray_SPECIALIZE_ME_O(size_t rank,
                           List_sp dimensions,
                           Array_sp data) : TemplatedBase(rank,dimensions,data) {};
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = SimpleVector_SPECIALIZE_ME_O::make(arrayTotalSize,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
    }
  };
};

namespace core {
  class ComplexVector_SPECIALIZE_ME_O : public template_Vector<ComplexVector_SPECIALIZE_ME_O,SimpleVector_SPECIALIZE_ME_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_SPECIALIZE_ME_O, "ComplexVector_SPECIALIZE_ME",ComplexVector_O);
    virtual ~ComplexVector_SPECIALIZE_ME() {};
  public:
    typedef template_Vector<ComplexVector_SPECIALIZE_ME_O,SimpleVector_SPECIALIZE_ME_O,ComplexVector_O> TemplatedBase;
  public: // make vector
      ComplexVector_SPECIALIZE_ME_O(size_t rank1,
                                    size_t dimension,
                                  T_sp fillPointer,
                                  Array_sp data,
                                  bool displacedToP,
                                  Fixnum_sp displacedIndexOffset) : TemplatedBase(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement,
                               bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo,
                               bool displacedToP, Fixnum_sp displacedIndexOffset) {
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(dimension,initialElement,initialElementSuppliedP);
      return gctools::GC<my_type>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
  };
};
