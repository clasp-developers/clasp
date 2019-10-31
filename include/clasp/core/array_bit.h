// ============================================================
// Arrays holding bits
//

namespace core {
  FORWARD(SimpleBitVector_O);
  FORWARD(BitVectorNs_O);
  FORWARD(MDArrayBit_O);
  FORWARD(SimpleMDArrayBit_O);
}

namespace core {
  bool ranged_bit_vector_EQ_(const SimpleBitVector_O& x, const SimpleBitVector_O& y,
                             size_t startx, size_t endx, size_t starty, size_t endy );

}

namespace core { class SimpleBitVector_O; };
template <>
struct gctools::GCInfo<core::SimpleBitVector_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
  typedef template_SimpleBitUnitVector<SimpleBitVector_O, 1, false> specialized_SimpleBitVector;
  class SimpleBitVector_O : public specialized_SimpleBitVector {
    LISP_CLASS(core, ClPkg, SimpleBitVector_O, "simple-bit-vector",AbstractSimpleVector_O);
    virtual ~SimpleBitVector_O() {};
  public:
    typedef specialized_SimpleBitVector TemplatedBase;
  SimpleBitVector_O(size_t length,
                    bit_array_word initialElement,
                    bool initialElementSupplied,
                    size_t initialContentsSize=0,
                    const bit_array_word* initialContents=NULL)
    : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static smart_ptr_type make(size_t length,
                               bit_array_word initialElement=0,
                               bool initialElementSupplied=false,
                               size_t initialContentsSize=0,
                               const bit_array_word* initialContents=NULL,
                               bool static_vector_p = false) {
      bit_array_word init = initialFillValue(initialElement);
      return gctools::GC<my_type>::allocate_bitunit_container(static_vector_p,length,init,initialElementSupplied,initialContentsSize,initialContents);
    }
    smart_ptr_type copy(size_t length, bit_array_word initialElement, bool initialElementSupplied) {
      return make(length, initialElement, initialElementSupplied,
                  MIN(bitunit_array_type::nwords_for_length(length), byteslen()),
                  bytes());
    }
    smart_ptr_type copy() { return copy(this->length(), default_initial_element(), false); }
    static smart_ptr_type make(const string& bv);
  public:
    virtual T_sp type_of() const final { return Cons_O::createList(cl::_sym_simple_bit_vector,clasp_make_fixnum(this->length()));}
    static T_sp static_element_type() { return cl::_sym_bit; }
  public:
    // TEMPORARY. These are in place to hold over Cando, which should switch to operator[].
    simple_element_type testBit(size_t idx) const { return (*this)[idx]; }
    void setBit(size_t idx, simple_element_type v) { (*this)[idx] = v; }
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual void __write__(T_sp strm) const override final;
    virtual bool equal(T_sp other) const final;
    virtual void sxhash_(HashGenerator& hg) const final {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const final {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        for ( size_t i(start); i<end; ++i ) {
          simple_element_type c = (*this)[i];
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
    }
  };
};

namespace core {
  class BitVectorNs_O : public template_Vector<BitVectorNs_O, SimpleBitVector_O, ComplexVector_O> {
    LISP_CLASS(core, CorePkg, BitVectorNs_O, "BitVectorNs",ComplexVector_O);
    virtual ~BitVectorNs_O() {};
  public:
    typedef template_Vector<BitVectorNs_O, SimpleBitVector_O, ComplexVector_O> TemplatedBase;
  BitVectorNs_O(size_t dimension,
                T_sp fillPointer,
                Array_sp data,
                bool displacedToP,
                Fixnum_sp displacedIndexOffset)
    : TemplatedBase(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static BitVectorNs_sp make(size_t length, simple_element_type initialElement, bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset ) {
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(length,initialElement,initialElementSuppliedP);
      auto bv = gctools::GC<BitVectorNs_O>::allocate_container(false,length,fillPointer,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return bv;
    }
  public:
    virtual void __write__(T_sp strm) const override final;
  public:
    // TEMPORARY. These are in place to hold over Cando, which should switch to operator[].
    simple_element_type testBit(size_t idx) const { return (*this)[idx]; }
    void setBit(size_t idx, simple_element_type v) { (*this)[idx] = v; }
  public:
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final { return this->equal(other);};
  public:
    virtual void sxhash_(HashGenerator& hg) const final {
      if (hg.isFilling()) {
        AbstractSimpleVector_sp svec;
        size_t start,end;
        this->asAbstractSimpleVectorRange(svec,start,end);
        svec->ranged_sxhash(hg,start,end);
      }
    }
  };
};

namespace core {
  class MDArrayBit_O : public template_Array<MDArrayBit_O, SimpleMDArrayBit_O, SimpleBitVector_O, MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArrayBit_O, "MDArrayBit",MDArray_O);
    virtual ~MDArrayBit_O() {};
  public:
    typedef template_Array<MDArrayBit_O, SimpleMDArrayBit_O, SimpleBitVector_O, MDArray_O> TemplatedBase;
  public: // make array
  MDArrayBit_O(size_t rank,
               List_sp dimensions,
               Array_sp data,
               bool displacedToP,
               Fixnum_sp displacedIndexOffset)
    : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (dataOrDisplacedTo.nilp())
        dataOrDisplacedTo = simple_type::make(arrayTotalSize,initialElement,true);
      return gctools::GC<my_type>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
    }
  };
};

namespace core {
  class SimpleMDArrayBit_O : public template_SimpleArray<SimpleMDArrayBit_O, SimpleBitVector_O, SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayBit_O, "SimpleMDArrayBit",SimpleMDArray_O);
    virtual ~SimpleMDArrayBit_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayBit_O, SimpleBitVector_O, SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArrayBit_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data) : TemplatedBase(rank,dimensions,data) {};
    static smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp())
        data = simple_type::make(arrayTotalSize,initialElement,true);
      return gctools::GC<SimpleMDArrayBit_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
    }
  };
};

namespace core {
  void SimpleBitVector_inPlaceOr(SimpleBitVector_sp x, SimpleBitVector_sp y);
  void SimpleBitVector_inPlaceAnd(SimpleBitVector_sp x, SimpleBitVector_sp y);
  void SimpleBitVector_inPlaceXor(SimpleBitVector_sp x, SimpleBitVector_sp y);
  void SimpleBitVector_getOnIndices(SimpleBitVector_sp x, vector<size_t> &res);
  size_t SimpleBitVector_lowestIndex(SimpleBitVector_sp x);
  bool SimpleBitVector_isZero(SimpleBitVector_sp x);
  SimpleBitVector_sp SimpleBitVector_copy(SimpleBitVector_sp orig_sbv);
}
