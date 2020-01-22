// ============================================================
// Arrays specialized for fixnum
//

namespace core {
  FORWARD(SimpleVector_fixnum);
  FORWARD(MDArray_fixnum);
  FORWARD(SimpleMDArray_fixnum);
};
template <>
struct gctools::GCInfo<core::SimpleVector_fixnum_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  class SimpleVector_fixnum_O;
  typedef template_SimpleVector<SimpleVector_fixnum_O,Fixnum,AbstractSimpleVector_O> specialized_SimpleVector_fixnum;
  class SimpleVector_fixnum_O : public specialized_SimpleVector_fixnum {
    LISP_CLASS(core, CorePkg, SimpleVector_fixnum_O, "SimpleVector_fixnum",AbstractSimpleVector_O);
    virtual ~SimpleVector_fixnum_O() {};
  public:
    typedef specialized_SimpleVector_fixnum TemplatedBase;
  public:
    static value_type default_initial_element(void) {return 0;}
    static value_type from_object(T_sp obj) { return clasp_to_fixnum(obj); };
    static T_sp to_object(const value_type& v) { return clasp_make_fixnum(v); };
  public:
  SimpleVector_fixnum_O(size_t length, value_type initialElement=value_type(),
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
    virtual T_sp element_type() const override { return cl::_sym_fixnum;};
  };
};


namespace core {
  class MDArray_fixnum_O : public template_Array<MDArray_fixnum_O,SimpleMDArray_fixnum_O,SimpleVector_fixnum_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_fixnum_O, "MDArray_fixnum",MDArray_O);
    virtual ~MDArray_fixnum_O() {};
  public:
    typedef template_Array<MDArray_fixnum_O,SimpleMDArray_fixnum_O,SimpleVector_fixnum_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_fixnum_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  };
};

namespace core {
  class SimpleMDArray_fixnum_O : public template_SimpleArray<SimpleMDArray_fixnum_O,SimpleVector_fixnum_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_fixnum_O, "SimpleMDArray_fixnum",SimpleMDArray_O);
    virtual ~SimpleMDArray_fixnum_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_fixnum_O,SimpleVector_fixnum_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArray_fixnum_O(size_t rank,
                           List_sp dimensions,
                           Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
};

namespace core {
  class ComplexVector_fixnum_O : public template_Vector<ComplexVector_fixnum_O,SimpleVector_fixnum_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_fixnum_O, "ComplexVector_fixnum",ComplexVector_O);
    virtual ~ComplexVector_fixnum_O() {};
  public:
    typedef template_Vector<ComplexVector_fixnum_O,SimpleVector_fixnum_O,ComplexVector_O> TemplatedBase;
  public: // make vector
      ComplexVector_fixnum_O(size_t rank1,
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
