
// ----------------------------------------------------------------------
// Arrays specialized for double
//

namespace core {
  FORWARD(SimpleVectorDouble);
};
template <>
struct gctools::GCInfo<core::SimpleVectorDouble_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleVectorDouble_O;
  typedef template_SimpleVector<SimpleVectorDouble_O,double,AbstractSimpleVector_O> specialized_SimpleVectorDouble;
  class SimpleVectorDouble_O : public specialized_SimpleVectorDouble {
    LISP_CLASS(core, CorePkg, SimpleVectorDouble_O, "SimpleVectorDouble",AbstractSimpleVector_O);
    virtual ~SimpleVectorDouble_O() {};
  public:
    typedef specialized_SimpleVectorDouble TemplatedBase;
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
          return clasp_to_double(gc::As_unsafe<General_sp>(obj));
        }
        TYPE_ERROR(obj,cl::_sym_double_float);
      }
      return 0.0;
    }
    static value_type from_object(T_sp obj) { return clasp_to_double(gc::As_unsafe<DoubleFloat_sp>(obj));};
    static T_sp to_object(const value_type& v) { return core::clasp_make_double_float(v); };
  public:
  SimpleVectorDouble_O(size_t length, value_type initialElement=value_type(),
                       bool initialElementSupplied=false,
                       size_t initialContentsSize=0,
                       const value_type* initialContents=NULL)
    : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleVectorDouble_sp make(size_t length,
                                      value_type initialElement=value_type(),
                                      bool initialElementSupplied=false,
                                      size_t initialContentsSize=0,
                                      const value_type* initialContents=NULL) {
      auto bs = gctools::GC<SimpleVectorDouble_O>::allocate_container(length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
  public:
    // Specific to SimpleVectorDouble_O
//    virtual void __write__(T_sp stream) const final;
  public:
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return cl::_sym_double_float;};
    virtual T_sp arrayElementType() const override { return cl::_sym_double_float; };
    virtual clasp_elttype elttype() const { return clasp_aet_df; };
    virtual void __write__(T_sp stream) const;
  public: // Provide the API that I used for NVector_sp
    static SimpleVectorDouble_sp create(size_t sz) {
      return make(sz,0.0,false,0,NULL);
    }
    double& element(size_t i) { return this->operator[](i);};
    double& getElement(size_t i) { return this->operator[](i);};
    void setElement(size_t i, double v) { this->operator[](i) = v; };
    void addToElement(size_t i, double v) { this->operator[](i) += v; };
    void zero() { for(size_t i(0),iEnd(this->length()); i<iEnd;++i) this->operator[](i) = 0.0; };
    size_t size() const { return this->length(); };
  };
};


namespace core {
  FORWARD(MDArrayDouble);
};
namespace core {
  class MDArrayDouble_O : public template_Array<MDArrayDouble_O,SimpleMDArrayDouble_O,SimpleVectorDouble_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArrayDouble_O, "MDArrayDouble",MDArray_O);
    virtual ~MDArrayDouble_O() {};
  public:
    typedef template_Array<MDArrayDouble_O,SimpleMDArrayDouble_O,SimpleVectorDouble_O,MDArray_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public: // make vector
  MDArrayDouble_O(size_t dummy_rank_1,
                  size_t dimension,
                  T_sp fillPointer,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : TemplatedBase(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static MDArrayDouble_sp make(size_t dimension, simple_element_type initialElement/*=simple_element_type()*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_fixnum(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(dimension,initialElement,true);
      }
      MDArrayDouble_sp array = gctools::GC<MDArrayDouble_O>::allocate_container(1,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
  public: // make array
  MDArrayDouble_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
    static MDArrayDouble_sp make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(arrayTotalSize,initialElement,true);
      }
      MDArrayDouble_sp array = gctools::GC<MDArrayDouble_O>::allocate_container(rank,dim_desig,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
  public:
//    virtual bool equalp(T_sp o) const final;
  };
};

namespace core {
  class SimpleMDArrayDouble_O : public template_SimpleArray<SimpleMDArrayDouble_O,SimpleVectorDouble_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayDouble_O, "SimpleMDArrayDouble",SimpleMDArray_O);
    virtual ~SimpleMDArrayDouble_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayDouble_O,SimpleVectorDouble_O,SimpleMDArray_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public: // make vector
  SimpleMDArrayDouble_O(size_t rank1, size_t dimension, Array_sp data) : TemplatedBase(dimension,data) {};
    static SimpleMDArrayDouble_sp make(size_t dimension, simple_element_type initialElement/*=_Nil<T_O>()*/, T_sp data/*=_Nil<T_O>()*/) {
      LIKELY_if (data.nilp()) {
        data = SimpleVectorDouble_O::make(dimension,initialElement,true);
      }
      SimpleMDArrayDouble_sp array = gctools::GC<SimpleMDArrayDouble_O>::allocate_container(1,dimension,gc::As_unsafe<Array_sp>(data));
      return array;
    }
    static SimpleMDArrayDouble_sp make(size_t dimension, simple_element_type initialElement) {
      return make(dimension,initialElement,_Nil<T_O>());
    }
  public: // make array
  SimpleMDArrayDouble_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data) : TemplatedBase(rank,dimensions,data) {};
    static SimpleMDArrayDouble_sp make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = SimpleVectorDouble_O::make(arrayTotalSize,initialElement,true);
      }
      SimpleMDArrayDouble_sp array = gctools::GC<SimpleMDArrayDouble_O>::allocate_container(rank,dim_desig,gc::As<Array_sp>(data));
      return array;
    }
  };
};

