// ============================================================
// Arrays specialized for size_t
//

namespace core {
  FORWARD(SimpleVector_size_t);
  FORWARD(MDArray_size_t);
  FORWARD(SimpleMDArray_size_t);
};
template <>
struct gctools::GCInfo<core::SimpleVector_size_t_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  class SimpleVector_size_t_O;
  typedef template_SimpleVector<SimpleVector_size_t_O,size_t,AbstractSimpleVector_O> specialized_SimpleVector_size_t;
  class SimpleVector_size_t_O : public specialized_SimpleVector_size_t {
    LISP_CLASS(core, CorePkg, SimpleVector_size_t_O, "SimpleVector_size_t",AbstractSimpleVector_O);
    virtual ~SimpleVector_size_t_O() {};
  public:
    typedef specialized_SimpleVector_size_t TemplatedBase;
  public:
    static value_type default_initial_element(void) {return 0;}
    static value_type from_object(T_sp obj) { return clasp_to_size(obj); };
    static T_sp to_object(const value_type& v) {return clasp_make_integer(v); };
  public:
  SimpleVector_size_t_O(size_t length, value_type initialElement=value_type(),
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
    virtual T_sp element_type() const override { return ext::_sym_cl_index;};
  };
};


namespace core {
  class MDArray_size_t_O : public template_Array<MDArray_size_t_O,SimpleMDArray_size_t_O,SimpleVector_size_t_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_size_t_O, "MDArray_size_t",MDArray_O);
    virtual ~MDArray_size_t_O() {};
  public:
    typedef template_Array<MDArray_size_t_O,SimpleMDArray_size_t_O,SimpleVector_size_t_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_size_t_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  };
};

namespace core {
  class SimpleMDArray_size_t_O : public template_SimpleArray<SimpleMDArray_size_t_O,SimpleVector_size_t_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_size_t_O, "SimpleMDArray_size_t",SimpleMDArray_O);
    virtual ~SimpleMDArray_size_t_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_size_t_O,SimpleVector_size_t_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArray_size_t_O(size_t rank,
                           List_sp dimensions,
                           Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
};

namespace core {
  class ComplexVector_size_t_O : public template_Vector<ComplexVector_size_t_O,SimpleVector_size_t_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_size_t_O, "ComplexVector_size_t",ComplexVector_O);
    virtual ~ComplexVector_size_t_O() {};
  public:
    typedef template_Vector<ComplexVector_size_t_O,SimpleVector_size_t_O,ComplexVector_O> TemplatedBase;
  public: // make vector
      ComplexVector_size_t_O(size_t rank1,
                             size_t dimension,
                         T_sp fillPointer,
                         Array_sp data,
                         bool displacedToP,
                         Fixnum_sp displacedIndexOffset) : TemplatedBase(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make_vector(size_t dimension, simple_element_type initialElement/*=simple_element_type()*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_size_t(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(dimension,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
    static smart_ptr_type make_vector(size_t dimension) {
      return make_vector(dimension,0,_Nil<T_O>(),_Nil<T_O>(),false,make_fixnum(0));
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
