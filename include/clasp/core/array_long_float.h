#pragma once
// ============================================================
// Arrays specialized for long_float_t
//

namespace core {

FORWARD(SimpleVector_long_float);
FORWARD(MDArray_long_float);
FORWARD(SimpleMDArray_long_float);
FORWARD(ComplexVector_long_float);

}; // namespace core

template <> struct gctools::GCInfo<core::SimpleVector_long_float_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
class SimpleVector_long_float_O;

typedef template_SimpleVector<SimpleVector_long_float_O, long_float_t, AbstractSimpleVector_O> specialized_SimpleVector_long_float;

class SimpleVector_long_float_O : public specialized_SimpleVector_long_float {
  LISP_CLASS(core, CorePkg, SimpleVector_long_float_O, "SimpleVector_long_float", AbstractSimpleVector_O);
  virtual ~SimpleVector_long_float_O() {};

public:
  typedef specialized_SimpleVector_long_float TemplatedBase;

  static value_type default_initial_element(void) { return long_float_t{0.0}; }
  static value_type from_object(T_sp obj) { return core::Number_O::as_long_float(obj.as<Number_O>()); };
  static T_sp to_object(const value_type& v) { return core::LongFloat_O::create(v); };

  SimpleVector_long_float_O(size_t length, value_type initialElement = value_type(), bool initialElementSupplied = false,
                            size_t initialContentsSize = 0, const value_type* initialContents = NULL)
      : TemplatedBase(length, initialElement, initialElementSupplied, initialContentsSize, initialContents) {};
  static smart_ptr_type make(size_t length, value_type initialElement = value_type(), bool initialElementSupplied = false,
                             size_t initialContentsSize = 0, const value_type* initialContents = NULL,
                             bool static_vector_p = false) {
    auto bs = gctools::GC<my_type>::allocate_container<gctools::RuntimeStage>(
        static_vector_p, length, initialElement, initialElementSupplied, initialContentsSize, initialContents);
    return bs;
  }

  virtual T_sp element_type() const override { return cl::_sym_long_float; };

  static SimpleVector_long_float_sp create(size_t sz) { return make(sz, long_float_t{0.0}, false, 0, NULL); }
  long_float_t& element(size_t i) { return this->operator[](i); };
  long_float_t& getElement(size_t i) { return this->operator[](i); };
  void setElement(size_t i, long_float_t v) { this->operator[](i) = v; };
  void addToElement(size_t i, long_float_t v) { this->operator[](i) += v; };
  void zero() { for (auto& e : *this) e = long_float_t{0.0}; };
  size_t size() const { return this->length(); };
};

class MDArray_long_float_O
    : public template_Array<MDArray_long_float_O, SimpleMDArray_long_float_O, SimpleVector_long_float_O, MDArray_O> {
  LISP_CLASS(core, CorePkg, MDArray_long_float_O, "MDArray_long_float", MDArray_O);
  virtual ~MDArray_long_float_O() {};

public:
  typedef template_Array<MDArray_long_float_O, SimpleMDArray_long_float_O, SimpleVector_long_float_O, MDArray_O> TemplatedBase;

  MDArray_long_float_O(size_t rank, List_sp dimensions, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset)
      : TemplatedBase(rank, dimensions, data, displacedToP, displacedIndexOffset) {};
};

class SimpleMDArray_long_float_O
    : public template_SimpleArray<SimpleMDArray_long_float_O, SimpleVector_long_float_O, SimpleMDArray_O> {
  LISP_CLASS(core, CorePkg, SimpleMDArray_long_float_O, "SimpleMDArray_long_float", SimpleMDArray_O);
  virtual ~SimpleMDArray_long_float_O() {};

public:
  typedef template_SimpleArray<SimpleMDArray_long_float_O, SimpleVector_long_float_O, SimpleMDArray_O> TemplatedBase;

  SimpleMDArray_long_float_O(size_t rank, List_sp dimensions, Array_sp data) : TemplatedBase(rank, dimensions, data) {};
};

class ComplexVector_long_float_O : public template_Vector<ComplexVector_long_float_O, SimpleVector_long_float_O, ComplexVector_O> {
  LISP_CLASS(core, CorePkg, ComplexVector_long_float_O, "ComplexVector_long_float", ComplexVector_O);
  virtual ~ComplexVector_long_float_O() {};

public:
  typedef template_Vector<ComplexVector_long_float_O, SimpleVector_long_float_O, ComplexVector_O> TemplatedBase;

  ComplexVector_long_float_O(size_t rank1, size_t dimension, T_sp fillPointer, Array_sp data, bool displacedToP,
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