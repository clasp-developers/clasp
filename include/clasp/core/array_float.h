// ----------------------------------------------------------------------
// Arrays specialized for float
//

namespace core {
  FORWARD(SimpleVectorFloat);
  FORWARD(SimpleMDArrayFloat);
};
template <>
struct gctools::GCInfo<core::SimpleVectorFloat_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleVectorFloat_O;
  typedef template_SimpleVector<SimpleVectorFloat_O,float,AbstractSimpleVector_O> specialized_SimpleVectorFloat;
  class SimpleVectorFloat_O : public specialized_SimpleVectorFloat {
    LISP_CLASS(core, CorePkg, SimpleVectorFloat_O, "SimpleVectorFloat",AbstractSimpleVector_O);
    virtual ~SimpleVectorFloat_O() {};
  public:
    typedef specialized_SimpleVectorFloat TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type default_initial_element(void) {return 0.0;}
    static value_type initial_element_from_object(T_sp obj, bool supplied) {
      if (supplied) {
        if (obj.single_floatp()) {
          return obj.unsafe_single_float();
        } else if (gc::IsA<General_sp>(obj)) {
          return clasp_to_float(gc::As_unsafe<General_sp>(obj));
        }
        TYPE_ERROR(obj,cl::_sym_single_float);
      }
      return 0.0;
    }
    static value_type from_object(T_sp obj) { if (obj.single_floatp()) return obj.unsafe_single_float(); TYPE_ERROR(obj,cl::_sym_single_float); };
    static T_sp to_object(const value_type& v) { return core::clasp_make_single_float(v);};
  public:
  SimpleVectorFloat_O(size_t length, value_type initialElement=value_type(),
                       bool initialElementSupplied=false,
                       size_t initialContentsSize=0,
                       const value_type* initialContents=NULL)
    : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleVectorFloat_sp make(size_t length,
                                      value_type initialElement=value_type(),
                                      bool initialElementSupplied=false,
                                      size_t initialContentsSize=0,
                                     const value_type* initialContents=NULL,
                                     bool static_vector_p = false) {
      auto bs = gctools::GC<SimpleVectorFloat_O>::allocate_container(static_vector_p,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
  public:
    // Specific to SimpleVectorFloat_O
//    virtual void __write__(T_sp stream) const final;
  public:
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return cl::_sym_single_float;};
    virtual T_sp arrayElementType() const override { return cl::_sym_single_float; };
    virtual clasp_elttype elttype() const { return clasp_aet_sf; };
    virtual void __write__(T_sp stream) const;
  };
};


namespace core {
  FORWARD(MDArrayFloat);
};
namespace core {
  class MDArrayFloat_O : public template_Array<MDArrayFloat_O,SimpleMDArrayFloat_O,SimpleVectorFloat_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArrayFloat_O, "MDArrayFloat",MDArray_O);
    virtual ~MDArrayFloat_O() {};
  public:
    typedef template_Array<MDArrayFloat_O,SimpleMDArrayFloat_O,SimpleVectorFloat_O,MDArray_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public: // make vector
  MDArrayFloat_O(size_t dummy_rank_1,
                  size_t dimension,
                  T_sp fillPointer,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : TemplatedBase(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static MDArrayFloat_sp make(size_t dimension, simple_element_type initialElement/*=simple_element_type()*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_fixnum(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(dimension,initialElement,true);
      }
      MDArrayFloat_sp array = gctools::GC<MDArrayFloat_O>::allocate_container(false,1,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
  public: // make array
  MDArrayFloat_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
    static MDArrayFloat_sp make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(arrayTotalSize,initialElement,true);
      }
      MDArrayFloat_sp array = gctools::GC<MDArrayFloat_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
  public:
//    virtual bool equalp(T_sp o) const final;
  };
};

namespace core {
  class SimpleMDArrayFloat_O : public template_SimpleArray<SimpleMDArrayFloat_O,SimpleVectorFloat_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayFloat_O, "SimpleMDArrayFloat",SimpleMDArray_O);
    virtual ~SimpleMDArrayFloat_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayFloat_O,SimpleVectorFloat_O,SimpleMDArray_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public: // make vector
  SimpleMDArrayFloat_O(size_t rank1, size_t dimension, Array_sp data) : TemplatedBase(dimension,data) {};
    static SimpleMDArrayFloat_sp make(size_t dimension, simple_element_type initialElement/*=_Nil<T_O>()*/, T_sp data/*=_Nil<T_O>()*/) {
      LIKELY_if (data.nilp()) {
        data = SimpleVectorFloat_O::make(dimension,initialElement,true);
      }
      SimpleMDArrayFloat_sp array = gctools::GC<SimpleMDArrayFloat_O>::allocate_container(false,1,dimension,gc::As_unsafe<Array_sp>(data));
      return array;
    }
    static SimpleMDArrayFloat_sp make(size_t dimension, simple_element_type initialElement) {
      return make(dimension,initialElement,_Nil<T_O>());
    }
  public: // make array
  SimpleMDArrayFloat_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data) : TemplatedBase(rank,dimensions,data) {};
    static SimpleMDArrayFloat_sp make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = SimpleVectorFloat_O::make(arrayTotalSize,initialElement,true);
      }
      SimpleMDArrayFloat_sp array = gctools::GC<SimpleMDArrayFloat_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
      return array;
    }
  };
};
