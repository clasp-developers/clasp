#pragma once
// ============================================================
// Arrays specialized for short_float_t
//

namespace core {

FORWARD(SimpleVector_short_float);
FORWARD(MDArray_short_float);
FORWARD(SimpleMDArray_short_float);
FORWARD(ComplexVector_short_float);

}; // namespace core

template <> struct gctools::GCInfo<core::SimpleVector_short_float_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
class SimpleVector_short_float_O;

typedef template_SimpleVector<SimpleVector_short_float_O, short_float_t, AbstractSimpleVector_O>
    specialized_SimpleVector_short_float;

class SimpleVector_short_float_O : public specialized_SimpleVector_short_float {
  LISP_CLASS(core, CorePkg, SimpleVector_short_float_O, "SimpleVector_short_float", AbstractSimpleVector_O);
  virtual ~SimpleVector_short_float_O() {};

public:
  typedef specialized_SimpleVector_short_float TemplatedBase;

  static value_type default_initial_element(void) { return short_float_t{0.0}; }
  static value_type from_object(T_sp obj) { return core::Number_O::as_short_float(obj); };
  static T_sp to_object(const value_type& v) { return core::ShortFloat_O::create(v); };

  SimpleVector_short_float_O(size_t length, value_type initialElement = value_type(), bool initialElementSupplied = false,
                             size_t initialContentsSize = 0, const value_type* initialContents = NULL)
      : TemplatedBase(length, initialElement, initialElementSupplied, initialContentsSize, initialContents) {};
  static smart_ptr_type make(size_t length, value_type initialElement = value_type(), bool initialElementSupplied = false,
                             size_t initialContentsSize = 0, const value_type* initialContents = NULL,
                             bool static_vector_p = false) {
    auto bs = gctools::GC<my_type>::allocate_container<gctools::RuntimeStage>(
        static_vector_p, length, initialElement, initialElementSupplied, initialContentsSize, initialContents);
    return bs;
  }

  virtual T_sp element_type() const override { return cl::_sym_short_float; };

  static SimpleVector_short_float_sp create(size_t sz) { return make(sz, short_float_t{0.0}, false, 0, NULL); }
  short_float_t& element(size_t i) { return this->operator[](i); };
  short_float_t& getElement(size_t i) { return this->operator[](i); };
  void setElement(size_t i, short_float_t v) { this->operator[](i) = v; };
  void addToElement(size_t i, short_float_t v) { this->operator[](i) += v; };
  void zero() {
    for (size_t i(0), iEnd(this->length()); i < iEnd; ++i)
      this->operator[](i) = short_float_t{0.0};
  };
  size_t size() const { return this->length(); };
};

class MDArray_short_float_O
    : public template_Array<MDArray_short_float_O, SimpleMDArray_short_float_O, SimpleVector_short_float_O, MDArray_O> {
  LISP_CLASS(core, CorePkg, MDArray_short_float_O, "MDArray_short_float", MDArray_O);
  virtual ~MDArray_short_float_O() {};

public:
  typedef template_Array<MDArray_short_float_O, SimpleMDArray_short_float_O, SimpleVector_short_float_O, MDArray_O> TemplatedBase;

  MDArray_short_float_O(size_t rank, List_sp dimensions, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset)
      : TemplatedBase(rank, dimensions, data, displacedToP, displacedIndexOffset) {};
};

class SimpleMDArray_short_float_O
    : public template_SimpleArray<SimpleMDArray_short_float_O, SimpleVector_short_float_O, SimpleMDArray_O> {
  LISP_CLASS(core, CorePkg, SimpleMDArray_short_float_O, "SimpleMDArray_short_float", SimpleMDArray_O);
  virtual ~SimpleMDArray_short_float_O() {};

public:
  typedef template_SimpleArray<SimpleMDArray_short_float_O, SimpleVector_short_float_O, SimpleMDArray_O> TemplatedBase;

  SimpleMDArray_short_float_O(size_t rank, List_sp dimensions, Array_sp data) : TemplatedBase(rank, dimensions, data) {};
};

class ComplexVector_short_float_O
    : public template_Vector<ComplexVector_short_float_O, SimpleVector_short_float_O, ComplexVector_O> {
  LISP_CLASS(core, CorePkg, ComplexVector_short_float_O, "ComplexVector_short_float", ComplexVector_O);
  virtual ~ComplexVector_short_float_O() {};

public:
  typedef template_Vector<ComplexVector_short_float_O, SimpleVector_short_float_O, ComplexVector_O> TemplatedBase;

  ComplexVector_short_float_O(size_t rank1, size_t dimension, T_sp fillPointer, Array_sp data, bool displacedToP,
                              Fixnum_sp displacedIndexOffset)
      : TemplatedBase(dimension, fillPointer, data, displacedToP, displacedIndexOffset) {};
  static smart_ptr_type make_vector(size_t dimension, simple_element_type initialElement /*=simple_element_type()*/,
                                    T_sp fillPointer /*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo /*=_Nil<T_O>()*/,
                                    bool displacedToP /*=false*/, Fixnum_sp displacedIndexOffset /*=clasp_make_fixnum(0)*/) {
    LIKELY_if(dataOrDisplacedTo.nilp()) dataOrDisplacedTo = simple_type::make(dimension, initialElement, true);
    return gctools::GC<my_type>::allocate_container<gctools::RuntimeStage>(
        false, 1 /*CRANK*/, dimension, fillPointer, gc::As_unsafe<Array_sp>(dataOrDisplacedTo), displacedToP, displacedIndexOffset);
  }
  static smart_ptr_type make_vector(size_t dimension) {
    return make_vector(dimension, 0, nil<T_O>(), nil<T_O>(), false, clasp_make_fixnum(0));
  }
  static smart_ptr_type make(size_t dimension, simple_element_type initialElement, bool initialElementSuppliedP, T_sp fillPointer,
                             T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
    (void)initialElementSuppliedP;
    return make_vector(dimension, initialElement, fillPointer, dataOrDisplacedTo, displacedToP, displacedIndexOffset);
  }
};
}; // namespace core
