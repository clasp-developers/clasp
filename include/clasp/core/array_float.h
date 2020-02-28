// ============================================================
// Arrays specialized for float
//
namespace core {
FORWARD(SimpleVector_float);
FORWARD(MDArray_float);
FORWARD(SimpleMDArray_float);
FORWARD(ComplexVector_float);
};
template <>
struct gctools::GCInfo<core::SimpleVector_float_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  class SimpleVector_float_O;
  typedef template_SimpleVector<SimpleVector_float_O,float,AbstractSimpleVector_O> specialized_SimpleVector_float;
  class SimpleVector_float_O : public specialized_SimpleVector_float {
    LISP_CLASS(core, CorePkg, SimpleVector_float_O, "SimpleVector_float",AbstractSimpleVector_O);
    virtual ~SimpleVector_float_O() {};
  public:
    typedef specialized_SimpleVector_float TemplatedBase;
  public:
    static value_type default_initial_element(void) {return 0.0;}
    static value_type from_object(T_sp obj) { if (obj.single_floatp()) return obj.unsafe_single_float(); TYPE_ERROR(obj,cl::_sym_single_float); };
    static T_sp to_object(const value_type& v) { return core::clasp_make_single_float(v);};
  public:
  SimpleVector_float_O(size_t length, value_type initialElement=value_type(),
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
    virtual T_sp element_type() const override { return cl::_sym_single_float;};
  public: // Provide the API that I used for NVector_sp
    static SimpleVector_float_sp create(size_t sz) {
      return make(sz,0.0,false,0,NULL);
    }
    float& element(size_t i) { return this->operator[](i);};
    float& getElement(size_t i) { return this->operator[](i);};
    void setElement(size_t i, float v) { this->operator[](i) = v; };
    void addToElement(size_t i, float v) { this->operator[](i) += v; };
    void zero() { for(size_t i(0),iEnd(this->length()); i<iEnd;++i) this->operator[](i) = 0.0; };
    size_t size() const { return this->length(); };
  };
};


namespace core {
  class MDArray_float_O : public template_Array<MDArray_float_O,SimpleMDArray_float_O,SimpleVector_float_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_float_O, "MDArray_float",MDArray_O);
    virtual ~MDArray_float_O() {};
  public:
    typedef template_Array<MDArray_float_O,SimpleMDArray_float_O,SimpleVector_float_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_float_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  };
};


namespace core {
  class SimpleMDArray_float_O : public template_SimpleArray<SimpleMDArray_float_O,SimpleVector_float_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_float_O, "SimpleMDArray_float",SimpleMDArray_O);
    virtual ~SimpleMDArray_float_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_float_O,SimpleVector_float_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArray_float_O(size_t rank,
                           List_sp dimensions,
                           Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
};

namespace core {
  class ComplexVector_float_O : public template_Vector<ComplexVector_float_O,SimpleVector_float_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_float_O, "ComplexVector_float",ComplexVector_O);
    virtual ~ComplexVector_float_O() {};
  public:
    typedef template_Vector<ComplexVector_float_O,SimpleVector_float_O,ComplexVector_O> TemplatedBase;
  public: // make vector
      ComplexVector_float_O(size_t rank1,
                            size_t dimension,
                          T_sp fillPointer,
                          Array_sp data,
                          bool displacedToP,
                          Fixnum_sp displacedIndexOffset) : TemplatedBase(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make_vector(size_t dimension, simple_element_type initialElement/*=simple_element_type()*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_fixnum(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(dimension,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
    static smart_ptr_type make_vector(size_t dimension) {
      return make_vector(dimension,0,_Nil<T_O>(),_Nil<T_O>(),false,clasp_make_fixnum(0));
    }
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement,
                               bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo,
                               bool displacedToP, Fixnum_sp displacedIndexOffset) {
      (void)initialElementSuppliedP;
      return make_vector(dimension, initialElement, fillPointer, dataOrDisplacedTo,
                         displacedToP, displacedIndexOffset);
    }
  };
};
