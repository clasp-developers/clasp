// ============================================================
// Arrays holding four-bit integers
//

namespace core {
  FORWARD(SimpleVector_byte4_t);
  /*
  FORWARD(MDArray_byte4_t);
  FORWARD(SimpleMDArray_byte4_t);
  FORWARD(ComplexVector_byte4_t);
*/
};
template <>
struct gctools::GCInfo<core::SimpleVector_byte4_t_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  typedef template_SimpleBitUnitVector<SimpleVector_byte4_t_O, 4, AbstractSimpleVector_O> specialized_SimpleVector_byte4_t;
  class SimpleVector_byte4_t_O : public specialized_SimpleVector_byte4_t {
    LISP_CLASS(core, CorePkg, SimpleVector_byte4_t_O, "SimpleVector_byte4_t", AbstractSimpleVector_O);
    virtual ~SimpleVector_byte4_t_O() {};
  public:
    typedef specialized_SimpleVector_byte4_t TemplatedBase;
  public:
    DONT_OPTIMIZE_ALWAYS SimpleVector_byte4_t_O(size_t length, bit_array_word initialElement, bool initialElementSupplied,
                           size_t initialContentsSize = 0, const bit_array_word* initialContents = NULL)
      : TemplatedBase(length, initialElement, initialElementSupplied, initialContentsSize,initialContents) {};
    static smart_ptr_type make(size_t length,
                               value_type initialElement = 0, bool initialElementSupplied = false,
                               size_t initialContentsSize=0, const bit_array_word* initialContents = NULL,
                               bool static_vector_p = false) {
      bit_array_word init = initialFillValue(initialElement);
      return gctools::GC<my_type>::allocate_container(static_vector_p, length, init, initialElementSupplied,
                                                      initialContentsSize, initialContents);
    }
  };
};
