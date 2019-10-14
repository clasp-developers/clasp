// ============================================================
// Arrays specialized for double
//
namespace core {
FORWARD(SimpleVector_double);
FORWARD(MDArray_double);
FORWARD(SimpleMDArray_double);
FORWARD(ComplexVector_double);
};
template <>
struct gctools::GCInfo<core::SimpleVector_double_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  class SimpleVector_double_O;
  typedef template_SimpleVector<SimpleVector_double_O,double,AbstractSimpleVector_O> specialized_SimpleVector_double;
  class SimpleVector_double_O : public specialized_SimpleVector_double {
    LISP_CLASS(core, CorePkg, SimpleVector_double_O, "SimpleVector_double",AbstractSimpleVector_O);
    virtual ~SimpleVector_double_O() {};
  public:
    typedef specialized_SimpleVector_double TemplatedBase;
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
    static value_type from_object(T_sp obj) { return clasp_to_double(obj);};
    static T_sp to_object(const value_type& v) { return core::clasp_make_double_float(v); };
  public:
  SimpleVector_double_O(size_t length, value_type initialElement=value_type(),
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
    virtual T_sp element_type() const override { return cl::_sym_double_float;};
    virtual T_sp arrayElementType() const override { return cl::_sym_double_float; };
    virtual clasp_elttype elttype() const { return clasp_aet_df; };
    virtual void __write__(T_sp stream) const;
  public: // Provide the API that I used for NVector_sp
    static SimpleVector_double_sp create(size_t sz) {
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
  class MDArray_double_O : public template_Array<MDArray_double_O,SimpleMDArray_double_O,SimpleVector_double_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_double_O, "MDArray_double",MDArray_O);
    virtual ~MDArray_double_O() {};
  public:
    typedef template_Array<MDArray_double_O,SimpleMDArray_double_O,SimpleVector_double_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_double_O(size_t rank,
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
  class SimpleMDArray_double_O : public template_SimpleArray<SimpleMDArray_double_O,SimpleVector_double_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_double_O, "SimpleMDArray_double",SimpleMDArray_O);
    virtual ~SimpleMDArray_double_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_double_O,SimpleVector_double_O,SimpleMDArray_O> TemplatedBase;
  public: // make vector
  SimpleMDArray_double_O(size_t rank1, size_t dimension, Array_sp data) : TemplatedBase(dimension,data) {};
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement/*=_Nil<T_O>()*/, T_sp data/*=_Nil<T_O>()*/) {
      printf("%s:%d This should never be called\n", __FILE__, __LINE__);
      LIKELY_if (data.nilp()) {
        data = simple_type::make(dimension,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,1,dimension,gc::As_unsafe<Array_sp>(data));
    }
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement) {
      return make(dimension,initialElement,_Nil<T_O>());
    }
  public: // make array
  SimpleMDArray_double_O(size_t rank,
                           List_sp dimensions,
                           Array_sp data) : TemplatedBase(rank,dimensions,data) {};
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = SimpleVector_double_O::make(arrayTotalSize,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
    }
  };
};

namespace core {
  class ComplexVector_double_O : public template_Vector<ComplexVector_double_O,SimpleVector_double_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_double_O, "ComplexVector_double",ComplexVector_O);
    virtual ~ComplexVector_double_O() {};
  public:
    typedef template_Vector<ComplexVector_double_O,SimpleVector_double_O,ComplexVector_O> TemplatedBase;
  public: // make vector
  ComplexVector_double_O(size_t dimension,
                          T_sp fillPointer,
                          Array_sp data,
                          bool displacedToP,
                          Fixnum_sp displacedIndexOffset) : TemplatedBase(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make_vector(size_t dimension, simple_element_type initialElement/*=simple_element_type()*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_fixnum(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(dimension,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
    static smart_ptr_type make_vector(size_t dimension) {
      return make_vector(dimension,0,_Nil<T_O>(),_Nil<T_O>(),false,clasp_make_fixnum(0));
    }
  public:
//    virtual bool equalp(T_sp o) const final;
    // vectorPushExtend of the specialized type
    void vectorPushExtend(value_type newElement, size_t extension = 0) {
      unlikely_if (!this->_Flags.fillPointerP()) noFillPointerSpecializedArrayError(this->asSmartPtr());
      cl_index idx = this->_FillPointerOrLengthOrDummy;
      unlikely_if (idx >= this->_ArrayTotalSize) {
        if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
        cl_index new_size = this->_ArrayTotalSize+extension;
        unlikely_if (!cl::_sym_adjust_array || !lisp_boundp(cl::_sym_adjust_array)) {
          this->internalAdjustSize_(new_size);
        } else {
          lisp_adjust_array(this->asSmartPtr(),clasp_make_fixnum(new_size),clasp_make_fixnum(this->_FillPointerOrLengthOrDummy));
        }
      }
      (*this)[idx] = newElement;
      ++this->_FillPointerOrLengthOrDummy;
    }
  };
};
