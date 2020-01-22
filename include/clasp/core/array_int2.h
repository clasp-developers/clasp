// ============================================================
// Arrays holding unsigned two-bit integers
//

namespace core {
  FORWARD(SimpleVector_byte2_t);
  FORWARD(MDArray_byte2_t);
  FORWARD(SimpleMDArray_byte2_t);
  FORWARD(ComplexVector_byte2_t);
};
template <>
struct gctools::GCInfo<core::SimpleVector_byte2_t_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  typedef template_SimpleBitUnitVector<SimpleVector_byte2_t_O, 2, false> specialized_SimpleVector_byte2_t;
  class SimpleVector_byte2_t_O : public specialized_SimpleVector_byte2_t {
    LISP_CLASS(core, CorePkg, SimpleVector_byte2_t_O, "SimpleVector_byte2_t", AbstractSimpleVector_O);
    virtual ~SimpleVector_byte2_t_O() {};
  public:
    typedef specialized_SimpleVector_byte2_t TemplatedBase;
  public:
    SimpleVector_byte2_t_O(size_t length, bit_array_word initialElement, bool initialElementSupplied,
                           size_t initialContentsSize = 0, const bit_array_word* initialContents = NULL)
      : TemplatedBase(length, initialElement, initialElementSupplied, initialContentsSize,initialContents) {};
    static smart_ptr_type make(size_t length,
                               value_type initialElement = 0, bool initialElementSupplied = false,
                               size_t initialContentsSize=0, const bit_array_word* initialContents = NULL,
                               bool static_vector_p = false) {
      bit_array_word init = initialFillValue(initialElement);
      return gctools::GC<my_type>::allocate_bitunit_container(static_vector_p, length,
                                                              init, initialElementSupplied,
                                                              initialContentsSize, initialContents);
    }
    static T_sp static_element_type() { return ext::_sym_byte2; }
  };
};

namespace core {
  class MDArray_byte2_t_O : public template_Array<MDArray_byte2_t_O,SimpleMDArray_byte2_t_O,SimpleVector_byte2_t_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_byte2_t_O, "MDArray_byte2_t",MDArray_O);
    virtual ~MDArray_byte2_t_O() {};
  public:
    typedef template_Array<MDArray_byte2_t_O,SimpleMDArray_byte2_t_O,SimpleVector_byte2_t_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_byte2_t_O(size_t rank, List_sp dimensions, Array_sp data,
                    bool displacedToP, Fixnum_sp displacedIndexOffset)
    : TemplatedBase(rank, dimensions, data, displacedToP, displacedIndexOffset) {};
  };
};

namespace core {
  class SimpleMDArray_byte2_t_O : public template_SimpleArray<SimpleMDArray_byte2_t_O,SimpleVector_byte2_t_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_byte2_t_O, "SimpleMDArray_byte2_t",SimpleMDArray_O);
    virtual ~SimpleMDArray_byte2_t_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_byte2_t_O,SimpleVector_byte2_t_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArray_byte2_t_O(size_t rank,
                          List_sp dimensions,
                          Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
};

namespace core {
  class ComplexVector_byte2_t_O : public template_Vector<ComplexVector_byte2_t_O,SimpleVector_byte2_t_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_byte2_t_O, "ComplexVector_byte2_t",ComplexVector_O);
    virtual ~ComplexVector_byte2_t_O() {};
  public:
    typedef template_Vector<ComplexVector_byte2_t_O,SimpleVector_byte2_t_O,ComplexVector_O> TemplatedBase;
  public: // make vector
      ComplexVector_byte2_t_O(size_t rank1,
                              size_t dimension,
                          T_sp fillPointer,
                          Array_sp data,
                          bool displacedToP,
                          Fixnum_sp displacedIndexOffset) : TemplatedBase(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement,
                               bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo,
                               bool displacedToP, Fixnum_sp displacedIndexOffset) {
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(dimension, initialElement, initialElementSuppliedP);
      return gctools::GC<my_type>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
  };
};

// ============================================================
// Arrays holding unsigned two-bit integers
//

namespace core {
  FORWARD(SimpleVector_int2_t);
  FORWARD(MDArray_int2_t);
  FORWARD(SimpleMDArray_int2_t);
  FORWARD(ComplexVector_int2_t);
};
template <>
struct gctools::GCInfo<core::SimpleVector_int2_t_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  typedef template_SimpleBitUnitVector<SimpleVector_int2_t_O, 2, true> specialized_SimpleVector_int2_t;
  class SimpleVector_int2_t_O : public specialized_SimpleVector_int2_t {
    LISP_CLASS(core, CorePkg, SimpleVector_int2_t_O, "SimpleVector_int2_t", AbstractSimpleVector_O);
    virtual ~SimpleVector_int2_t_O() {};
  public:
    typedef specialized_SimpleVector_int2_t TemplatedBase;
  public:
    SimpleVector_int2_t_O(size_t length, bit_array_word initialElement, bool initialElementSupplied,
                           size_t initialContentsSize = 0, const bit_array_word* initialContents = NULL)
      : TemplatedBase(length, initialElement, initialElementSupplied, initialContentsSize,initialContents) {};
    static smart_ptr_type make(size_t length,
                               value_type initialElement = 0, bool initialElementSupplied = false,
                               size_t initialContentsSize=0, const bit_array_word* initialContents = NULL,
                               bool static_vector_p = false) {
      bit_array_word init = initialFillValue(initialElement);
      return gctools::GC<my_type>::allocate_bitunit_container(static_vector_p, length,
                                                              init, initialElementSupplied,
                                                              initialContentsSize, initialContents);
    }
    static T_sp static_element_type() { return ext::_sym_integer2; }
  };
};

namespace core {
  class MDArray_int2_t_O : public template_Array<MDArray_int2_t_O,SimpleMDArray_int2_t_O,SimpleVector_int2_t_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArray_int2_t_O, "MDArray_int2_t",MDArray_O);
    virtual ~MDArray_int2_t_O() {};
  public:
    typedef template_Array<MDArray_int2_t_O,SimpleMDArray_int2_t_O,SimpleVector_int2_t_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArray_int2_t_O(size_t rank, List_sp dimensions, Array_sp data,
                    bool displacedToP, Fixnum_sp displacedIndexOffset)
    : TemplatedBase(rank, dimensions, data, displacedToP, displacedIndexOffset) {};
  };
};

namespace core {
  class SimpleMDArray_int2_t_O : public template_SimpleArray<SimpleMDArray_int2_t_O,SimpleVector_int2_t_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArray_int2_t_O, "SimpleMDArray_int2_t",SimpleMDArray_O);
    virtual ~SimpleMDArray_int2_t_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArray_int2_t_O,SimpleVector_int2_t_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArray_int2_t_O(size_t rank,
                          List_sp dimensions,
                          Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
};

namespace core {
  class ComplexVector_int2_t_O : public template_Vector<ComplexVector_int2_t_O,SimpleVector_int2_t_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_int2_t_O, "ComplexVector_int2_t",ComplexVector_O);
    virtual ~ComplexVector_int2_t_O() {};
  public:
    typedef template_Vector<ComplexVector_int2_t_O,SimpleVector_int2_t_O,ComplexVector_O> TemplatedBase;
  public: // make vector
      ComplexVector_int2_t_O(size_t rank1,
                             size_t dimension,
                          T_sp fillPointer,
                          Array_sp data,
                          bool displacedToP,
                          Fixnum_sp displacedIndexOffset) : TemplatedBase(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement,
                               bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo,
                               bool displacedToP, Fixnum_sp displacedIndexOffset) {
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(dimension, initialElement, initialElementSuppliedP);
      return gctools::GC<my_type>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
  };
};
