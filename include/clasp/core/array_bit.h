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
  [[noreturn]] void bitVectorDoesntSupportError();
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
  class SimpleBitVector_O : public AbstractSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleBitVector_O, "simple-bit-vector",AbstractSimpleVector_O);
    virtual ~SimpleBitVector_O() {};
  public:
    typedef gctools::GCBitUnitArray_moveable<1> bitunit_array_type;
    typedef bit_array_word value_type;
    typedef uint bit_element_type;
    static const size_t BitWidth = bitunit_array_type::number_of_bit_units_in_word;
  public:
    bitunit_array_type _Data;
  SimpleBitVector_O(size_t length,
                    value_type initialElement,
                    bool initialElementSupplied,
                    size_t initialContentsSize=0,
                    value_type* initialContents=NULL)
    : Base(), _Data(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
  SimpleBitVector_O(size_t length, bit_array_word* data) : Base(), _Data(length, data) {};
    static SimpleBitVector_sp make( size_t length,
                                    value_type initialElement=0,
                                    bool initialElementSupplied=false,
                                    size_t initialContentsSize=0,
                                    value_type* initialContents=NULL) {
      return gctools::GC<SimpleBitVector_O>::allocate_bitunit_container(length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
    }
    SimpleBitVector_sp copy() {
      return gctools::GC<SimpleBitVector_O>::allocate_bitunit_container(this->length(), this->bytes());
    }
    static SimpleBitVector_sp make(const string& bv);
  public:
    static value_type default_initial_element(void) {return 0;}
    static value_type from_object(T_sp object) {
      if (object.fixnump()) {
        value_type i = object.unsafe_fixnum();
        if (i==0||i==1) return i;
      }
      TYPE_ERROR(object, cl::_sym_bit);
    }
    static T_sp to_object(const value_type& v) { return clasp_make_integer(v); };
  public:
    virtual T_sp type_of() const final { return Cons_O::createList(cl::_sym_simple_bit_vector,clasp_make_fixnum(this->length()));};
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return cl::_sym_bit; };
  public:
  public:
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const override {
      sv = this->asSmartPtr();
      start = 0;
      end = this->length();
    }
    void setBit(size_t idx, uint v) {this->_Data.unsignedSetBitUnit(idx,v);}
    uint testBit(size_t idx) const {return this->_Data.unsignedBitUnit(idx);};
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual void __write__(T_sp strm) const final;
    virtual size_t elementSizeInBytes() const override {bitVectorDoesntSupportError();};
    virtual void* rowMajorAddressOfElement_(size_t i) const override {bitVectorDoesntSupportError();};
    value_type* bytes() { return &this->_Data[0]; };
    virtual Array_sp reverse() const final;
    virtual Array_sp nreverse() final;
    virtual bool equal(T_sp other) const final;
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const final;
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) override;
    virtual vector<size_t> arrayDimensionsAsVector() const final {
      vector<size_t> dims;
      dims.push_back(this->length());
      return dims;
    }
    virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) final;
  public:
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return clasp_make_fixnum(this->testBit(idx)); };
    CL_METHOD_OVERLOAD virtual void vset(size_t idx, T_sp value) final {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp vref(size_t idx) const final {return clasp_make_fixnum(this->testBit(idx)); };
    virtual void sxhash_(HashGenerator& hg) const final {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const final {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        Fixnum c;
        for ( size_t i(start); i<end; ++i ) {
          uint c = this->testBit(i);
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
    }
  };
};

namespace core {
  // I can't use the template_Array here because of bitwise access
  class BitVectorNs_O : public ComplexVector_O {
    LISP_CLASS(core, CorePkg, BitVectorNs_O, "BitVectorNs",ComplexVector_O);
    virtual ~BitVectorNs_O() {};
  public:
    typedef SimpleBitVector_O simple_type;
  BitVectorNs_O(size_t dummy_rank_1,
                size_t dimension,
                T_sp fillPointer,
                Array_sp data,
                bool displacedToP,
                Fixnum_sp displacedIndexOffset)
    : Base(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static BitVectorNs_sp make(size_t length, SimpleBitVector_O::value_type initialElement, bool initialElementSuppliedP, T_sp fillPointer, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset ) {
//      GC_ALLOCATE_VARIADIC(BitVectorNs_O, bv, length, fillPointer, displacedTo, displacedIndexOffset );
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = SimpleBitVector_O::make(length,initialElement,initialElementSuppliedP);
      }
      auto bv = gctools::GC<BitVectorNs_O>::allocate_container(false,1,length,fillPointer,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return bv;
    }
  public:
    virtual void __write__(T_sp strm) const;
    uint testBit(size_t idx) const {
      AbstractSimpleVector_sp bme;
      size_t mstart, mend;
      this->asAbstractSimpleVectorRange(bme,mstart,mend);
      simple_type* me = reinterpret_cast<simple_type*>(&*bme);
      return me->testBit(idx+this->_DisplacedIndexOffset);
    }
    void setBit(size_t idx, uint v)  {
      AbstractSimpleVector_sp bme;
      size_t mstart, mend;
      this->asAbstractSimpleVectorRange(bme,mstart,mend);
      simple_type* me = reinterpret_cast<simple_type*>(&*bme);
      me->setBit(idx+this->_DisplacedIndexOffset,v);
    }
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      unlikely_if (!gc::IsA<SimpleBitVector_sp>(this->_Data)) {
        this->_Data->asAbstractSimpleVectorRange(sv,start,end);
        start += this->_DisplacedIndexOffset;
        end = this->length()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<SimpleBitVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->length()+this->_DisplacedIndexOffset;
    }
  public:
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final { return this->equal(other);};
    virtual void internalAdjustSize_(size_t size, T_sp init_element=_Nil<T_O>(), bool initElementSupplied=false ) override;
    virtual Array_sp reverse() const override;
    virtual Array_sp nreverse() override;
  public:
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) override {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const override {return clasp_make_fixnum(this->testBit(idx)); };
    virtual void sxhash_(HashGenerator& hg) const final {
      if (hg.isFilling()) {
        AbstractSimpleVector_sp svec;
        size_t start,end;
        this->asAbstractSimpleVectorRange(svec,start,end);
        svec->ranged_sxhash(hg,start,end);
      }
    }
    virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension = 0) override;
  };
};

namespace core {
  class MDArrayBit_O : public MDArray_O {
    LISP_CLASS(core, CorePkg, MDArrayBit_O, "MDArrayBit",MDArray_O);
    virtual ~MDArrayBit_O() {};
  public:
    typedef SimpleBitVector_O simple_type;
    typedef typename simple_type::bit_element_type bit_element_type;
  public: // make array
  MDArrayBit_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : Base(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
    static MDArrayBit_sp make_multi_dimensional(List_sp dim_desig, bit_element_type initialElement, T_sp dataOrDisplacedTo, bool displacedToP, Fixnum_sp displacedIndexOffset) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = simple_type::make(arrayTotalSize,initialElement,true);
      }
      MDArrayBit_sp array = gctools::GC<MDArrayBit_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return array;
    }
  public:
    uint testBit(size_t idx) const {
      AbstractSimpleVector_sp bme;
      size_t mstart, mend;
      this->asAbstractSimpleVectorRange(bme,mstart,mend);
      simple_type* me = reinterpret_cast<simple_type*>(&*bme);
      return me->testBit(idx+this->_DisplacedIndexOffset);
    }
    void setBit(size_t idx, uint v)  {
      AbstractSimpleVector_sp bme;
      size_t mstart, mend;
      this->asAbstractSimpleVectorRange(bme,mstart,mend);
      simple_type* me = reinterpret_cast<simple_type*>(&*bme);
      me->setBit(idx+this->_DisplacedIndexOffset,v);
    }
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      unlikely_if (!gc::IsA<SimpleBitVector_sp>(this->_Data)) {
        this->_Data->asAbstractSimpleVectorRange(sv,start,end);
        start += this->_DisplacedIndexOffset;
        //this->length() is a no-op here, returns the dummy value
        end = this->_Data->length()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<SimpleBitVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      //this->length() is a no-op here, returns the dummy value
      end = sv->length()+this->_DisplacedIndexOffset;
    }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) override {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const override {return clasp_make_fixnum(this->testBit(idx)); };
    virtual bool equal(T_sp other) const final {return this->eq(other); };
//    virtual bool equalp(T_sp other) const final;
    virtual Array_sp reverse() const final {notVectorError(this->asSmartPtr());};
    virtual Array_sp nreverse() override {notVectorError(this->asSmartPtr());};
    virtual void internalAdjustSize_(size_t size, T_sp init_element=_Nil<T_O>(), bool initElementSupplied=false ) {HARD_IMPLEMENT_ME();};
  };
};

namespace core {
  class SimpleMDArrayBit_O : public SimpleMDArray_O {
    LISP_CLASS(core, CorePkg, SimpleMDArrayBit_O, "SimpleMDArrayBit",SimpleMDArray_O);
    virtual ~SimpleMDArrayBit_O() {};
  public:
    typedef SimpleBitVector_O simple_type;
    typedef typename simple_type::bit_element_type bit_element_type;
  public: // make array
  SimpleMDArrayBit_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data) : Base(rank,dimensions,data) {};
    static SimpleMDArrayBit_sp make_multi_dimensional(List_sp dim_desig, bit_element_type initialElement, T_sp data) {
      ASSERT(dim_desig.consp()||dim_desig.nilp());
      size_t rank;
      size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig,rank);
      LIKELY_if (data.nilp()) {
        data = SimpleBitVector_O::make(arrayTotalSize,initialElement,true);
      }
      SimpleMDArrayBit_sp array = gctools::GC<SimpleMDArrayBit_O>::allocate_container(false,rank,dim_desig,gc::As<Array_sp>(data));
      return array;
    }
  public:
    uint testBit(size_t idx) const {
      AbstractSimpleVector_sp bme;
      size_t mstart, mend;
      this->asAbstractSimpleVectorRange(bme,mstart,mend);
      simple_type* me = reinterpret_cast<simple_type*>(&*bme);
      return me->testBit(idx+this->_DisplacedIndexOffset);
    }
    void setBit(size_t idx, uint v)  {
      AbstractSimpleVector_sp bme;
      size_t mstart, mend;
      this->asAbstractSimpleVectorRange(bme,mstart,mend);
      simple_type* me = reinterpret_cast<simple_type*>(&*bme);
      me->setBit(idx+this->_DisplacedIndexOffset,v);
    }
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      sv = gc::As<SimpleBitVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      // this->length() is the dummy value in this case, 0xDEADBEEF01234567
      // this->length() in this class shoud return an error
      // end = this->length()+this->_DisplacedIndexOffset;
      end = sv->length()+this->_DisplacedIndexOffset;
    }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) override {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const override {return clasp_make_fixnum(this->testBit(idx)); };
    virtual bool equal(T_sp other) const final {return this->eq(other); };
//    virtual bool equalp(T_sp other) const final;
    virtual Array_sp reverse() const final {notVectorError(this->asSmartPtr());};
    virtual Array_sp nreverse() override {notVectorError(this->asSmartPtr());};
    virtual void internalAdjustSize_(size_t size, T_sp init_element=_Nil<T_O>(), bool initElementSupplied=false ) {HARD_IMPLEMENT_ME();};
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
