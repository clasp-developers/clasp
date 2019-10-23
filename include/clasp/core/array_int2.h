// ============================================================
// Arrays holding two-bit integers
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
  typedef template_SimpleBitUnitVector<SimpleVector_byte2_t_O, 2, AbstractSimpleVector_O> specialized_SimpleVector_byte2_t;
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
      (void)static_vector_p; // See fixme in byte4
      bit_array_word init = initialFillValue(initialElement);
      return gctools::GC<my_type>::allocate_bitunit_container(length, init, initialElementSupplied,
                                                              initialContentsSize, initialContents);
    }
    smart_ptr_type copy(size_t length, value_type initialElement, bool initialElementSupplied) {
      return make(length, initialElement, initialElementSupplied,
                  MIN(bitunit_array_type::nwords_for_length(length), byteslen()),
                  bytes());
    }
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
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement,
                                                 T_sp dataOrDisplacedTo, bool displacedToP,
                                                 Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig, rank);
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(arrayTotalSize, initialElement, true);
      return gctools::GC<my_type>::allocate_container(false, rank, dim_desig, gc::As<Array_sp>(dataOrDisplacedTo), displacedToP, displacedIndexOffset);
    }
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
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = simple_type::make(arrayTotalSize,initialElement,true);
      }
      return gctools::GC<my_type>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
    }
  };
};

namespace core {
  class ComplexVector_byte2_t_O : public template_Vector<ComplexVector_byte2_t_O,SimpleVector_byte2_t_O,ComplexVector_O> {
    LISP_CLASS(core, CorePkg, ComplexVector_byte2_t_O, "ComplexVector_byte2_t",ComplexVector_O);
    virtual ~ComplexVector_byte2_t_O() {};
  public:
    typedef template_Vector<ComplexVector_byte2_t_O,SimpleVector_byte2_t_O,ComplexVector_O> TemplatedBase;
  public: // make vector
  ComplexVector_byte2_t_O(size_t dimension,
                          T_sp fillPointer,
                          Array_sp data,
                          bool displacedToP,
                          Fixnum_sp displacedIndexOffset) : TemplatedBase(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make(size_t dimension, simple_element_type initialElement,
                               bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo,
                               bool displacedToP, Fixnum_sp displacedIndexOffset) {
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(dimension, initialElement, initialElementSuppliedP);
      return gctools::GC<my_type>::allocate_container(false,dimension,fillPointer,gc::As_unsafe<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
  };
};
