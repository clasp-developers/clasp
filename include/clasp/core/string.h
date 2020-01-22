// Strings

namespace core {
  template <typename T1,typename T2>
    bool template_string_EQ_equal(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2)
  {
    const typename T1::simple_element_type* cp1((const typename T1::simple_element_type*)string1.rowMajorAddressOfElement_(start1));
    const typename T2::simple_element_type* cp2((const typename T2::simple_element_type*)string2.rowMajorAddressOfElement_(start2));
    size_t length = end1 - start1;
    if (length != (end2 - start2)) return false;
    for (size_t i = 0; i < length; ++i, ++cp1, ++cp2)
      if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2)))
        return false;
    return true;
  }
}; // namespace core

namespace core { class SimpleString_O; };
template <>
struct gctools::GCInfo<core::SimpleString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleString_O : public AbstractSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleString_O, "simple-string",AbstractSimpleVector_O);
    virtual ~SimpleString_O() {};
  };
};

namespace core { class SimpleBaseString_O; };
template <>
struct gctools::GCInfo<core::SimpleBaseString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleBaseString_O;
  typedef template_SimpleVector<SimpleBaseString_O,claspChar,SimpleString_O> specialized_SimpleBaseString;
  class SimpleBaseString_O : public specialized_SimpleBaseString {
    LISP_CLASS(core, ClPkg, SimpleBaseString_O, "simple-base-string",SimpleString_O);
    virtual ~SimpleBaseString_O() {};
  public:
    typedef specialized_SimpleBaseString TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type default_initial_element(void) {return '\0';}
    static value_type from_object(T_sp obj) {
      if (obj.characterp()) {
        return obj.unsafe_character();
      } else if (obj.nilp()) {
        return '\0';
      }
      TYPE_ERROR(obj,Cons_O::createList(cl::_sym_or,cl::_sym_character,cl::_sym_nil));
    }
    static T_sp to_object(const value_type& v) { return clasp_make_character(v); };
  public:
    // Always leave space for \0 at end
  SimpleBaseString_O(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleBaseString_sp make(size_t length, value_type initialElement='\0', bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL, bool static_vector_p = false) {
      // For C/C++ interop make SimpleBaseString 1 character longer and append a \0
      auto bs = gctools::GC<SimpleBaseString_O>::allocate_container_null_terminated_string(static_vector_p,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      bs->c_style_null_terminate(); // (*bs)[length] = '\0';
      return bs;
    }
    static SimpleBaseString_sp make(const std::string& str) {
      return SimpleBaseString_O::make(str.size(),'\0',true,str.size(),(const claspChar*)str.c_str());
    }
  //SimpleBaseString_O(size_t total_size) : Base(), _Data('\0',total_size+1) {};
  public:
    virtual T_sp type_of() const final { return Cons_O::createList(cl::_sym_simple_base_string,clasp_make_fixnum(this->length()));};
    virtual T_sp element_type() const final { return cl::_sym_base_char; };
  public:
    void c_style_null_terminate() { this->_Data[this->length()] = '\0'; };
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final;
    virtual void __write__(T_sp strm) const final; // implemented in write_array.cc
    virtual std::string get_std_string() const final { return this->length()==0 ? string("") : string((char*)&(*this)[0],this->length());};
    virtual std::string __repr__() const { return this->get_std_string(); };
    virtual void sxhash_(HashGenerator& hg) const final {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const final {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        Fixnum c;
        for ( size_t i(start); i<end; ++i ) {
          const value_type& c = (*this)[i];
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
    }
  };
};

namespace core { class SimpleCharacterString_O; };
template <>
struct gctools::GCInfo<core::SimpleCharacterString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleCharacterString_O;
  typedef template_SimpleVector<SimpleCharacterString_O,claspCharacter,SimpleString_O> specialized_SimpleCharacterString;
  class SimpleCharacterString_O : public specialized_SimpleCharacterString {
    LISP_CLASS(core, CorePkg, SimpleCharacterString_O, "SimpleCharacterString",SimpleString_O);
    virtual ~SimpleCharacterString_O() {};
  public:
    typedef specialized_SimpleCharacterString TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type default_initial_element(void) {return '\0';}
    static value_type from_object(T_sp obj) {
      if (obj.characterp()) {
        return obj.unsafe_character();
      } else if (obj.nilp()) {
        return 0;
      }
      TYPE_ERROR(obj,Cons_O::createList(cl::_sym_or,cl::_sym_character,cl::_sym_nil));
    }
    static T_sp to_object(const value_type& v) { return clasp_make_character(v); };
  public:
    // Always leave space for \0 at end
  SimpleCharacterString_O(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleCharacterString_sp make(size_t length, value_type initialElement='\0', bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL,
                                         bool static_vector_p = false) {
      auto bs = gctools::GC<SimpleCharacterString_O>::allocate_container(static_vector_p,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
    static SimpleCharacterString_sp make(const std::string& str) {
      auto bs = SimpleCharacterString_O::make(str.size(),'\0');
      for ( size_t i(0); i<str.size(); ++i ) {
        (*bs)[i] = str[i];
      }
      return bs;
    }
  public:
    virtual T_sp element_type() const final { return cl::_sym_character; };
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual void __write__(T_sp strm) const final;
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final;
    virtual std::string get_std_string() const final;
    virtual std::string __repr__() const final;
  public:
    virtual void sxhash_(HashGenerator& hg) const override {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const override {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        Fixnum c;
        for ( size_t i(start); i<end; ++i ) {
          const value_type& c = (*this)[i];
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
    }
  };
}; // namespace core

namespace core {
  class StrNs_O : public ComplexVector_O {
    LISP_CLASS(core, CorePkg, StrNs_O, "StrNs",ComplexVector_O);
    virtual ~StrNs_O() {};
  public:
  StrNs_O(size_t dimension,
          T_sp fillPointer,
          Array_sp data,
          bool displacedToP,
          Fixnum_sp displacedIndexOffset)
    : Base(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
  public:
    virtual void sxhash_(HashGenerator& hg) const final {
      AbstractSimpleVector_sp svec;
      size_t start,end;
      this->asAbstractSimpleVectorRange(svec,start,end);
      svec->ranged_sxhash(hg,start,end);
    }
    virtual SimpleString_sp asMinimalSimpleString() const = 0;
    void ensureSpaceAfterFillPointer(T_sp init_element, size_t size) {
      size_t min = this->fillPointer() + size;
      if (min > this->_ArrayTotalSize)
        this->internalAdjustSize_(min, init_element, true);
    }
  };
};

namespace core {
  class Str8Ns_O : public template_Vector<Str8Ns_O,SimpleBaseString_O,StrNs_O> {
    LISP_CLASS(core, CorePkg, Str8Ns_O, "Str8Ns",StrNs_O);
    virtual ~Str8Ns_O() {};
  public:
    // The types that define what this class does
    typedef template_Vector<Str8Ns_O,SimpleBaseString_O,StrNs_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
    typedef typename TemplatedBase::dimension_element_type value_type;
    typedef simple_element_type* iterator;
    typedef const simple_element_type* const_iterator;
  public:
      Str8Ns_O(size_t rank1,
               size_t dimension,
           T_sp fillPointer,
           Array_sp data,
           bool displacedToP,
           Fixnum_sp displacedIndexOffset)
    : TemplatedBase(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static Str8Ns_sp make(size_t dimension, claspChar initElement/*='\0'*/, bool initialElementSuppliedP/*=false*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_fixnum(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = SimpleBaseString_O::make(dimension,initElement,initialElementSuppliedP);
      }
      auto s = gctools::GC<Str8Ns_O>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return s;
    }
    static Str8Ns_sp make(size_t dimension, claspChar initElement/*='\0'*/, bool initialElementSuppliedP/*=false*/, T_sp fillPointer/*=_Nil<T_O>()*/) {
      return make(dimension,initElement,initialElementSuppliedP,fillPointer,_Nil<T_O>(),false,clasp_make_fixnum(0));
    }
    static Str8Ns_sp make(const string& nm) {
      auto ss = SimpleBaseString_O::make(nm);
      auto result = Str8Ns_O::make(nm.size(),'\0',false,_Nil<T_O>(),ss,false,clasp_make_fixnum(0));
      return result;
    }
  public:
    // move all the constructors into here
    static Str8Ns_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
      return Str8Ns_O::make(bufferSize, simple_element_type()/*' '*/, true, clasp_make_fixnum(0),
                            _Nil<T_O>(),false,clasp_make_fixnum(0));
    };
  public:
    static Str8Ns_sp create(const string &nm);
//  static Str8Ns_sp create(const boost::format &nm);
    static Str8Ns_sp create(const char *nm, size_t numChars);
    static Str8Ns_sp create(const char *nm);
    static Str8Ns_sp create(size_t numChars);
    static Str8Ns_sp create(Str8Ns_sp orig);
  public:
  public:
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final;
  public:
    iterator begin() { return &(*this)[0]; };
    iterator end() { return &(*this)[this->length()]; };
    const_iterator begin() const { return &(*this)[0]; };
    const_iterator end() const { return &(*this)[this->length()]; };
  public:
    virtual void __write__(T_sp strm) const final;
    virtual std::string get_std_string() const final { return std::string((const char*)this->begin(),this->length());};
    virtual std::string __repr__() const final { return this->get_std_string(); };
  public: // Str8Ns specific functions
    virtual SimpleString_sp asMinimalSimpleString() const final;
  };
};

namespace core {
  class StrWNs_O : public template_Vector<StrWNs_O,SimpleCharacterString_O,StrNs_O> {
    LISP_CLASS(core, CorePkg, StrWNs_O, "StrWNs",StrNs_O);
    virtual ~StrWNs_O() {};
  public:
    // The types that define what this class does
    typedef template_Vector<StrWNs_O,SimpleCharacterString_O,StrNs_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
    typedef simple_element_type* iterator;
    typedef const simple_element_type* const_iterator;
  public:
      StrWNs_O(size_t rank1,
               size_t dimension,
           T_sp fillPointer,
           Array_sp data,
           bool displacedToP,
           Fixnum_sp displacedIndexOffset)
    : TemplatedBase(dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    static StrWNs_sp make(size_t dimension, claspCharacter initElement/*='\0'*/, bool initialElementSuppliedP/*=false*/, T_sp fillPointer/*=_Nil<T_O>()*/, T_sp dataOrDisplacedTo/*=_Nil<T_O>()*/, bool displacedToP/*=false*/, Fixnum_sp displacedIndexOffset/*=clasp_make_fixnum(0)*/ ) {
      LIKELY_if (dataOrDisplacedTo.nilp()) {
        dataOrDisplacedTo = SimpleCharacterString_O::make(dimension,initElement,initialElementSuppliedP);
      }
      auto s = gctools::GC<StrWNs_O>::allocate_container(false,1/*CRANK*/,dimension,fillPointer,gc::As<Array_sp>(dataOrDisplacedTo),displacedToP,displacedIndexOffset);
      return s;
    }
    static StrWNs_sp make(size_t dimension, claspCharacter initElement/*='\0'*/, bool initialElementSuppliedP/*=false*/, T_sp fillPointer/*=_Nil<T_O>()*/) {
      return make(dimension,initElement,initialElementSuppliedP,fillPointer,_Nil<T_O>(),false,clasp_make_fixnum(0));
    }
    static StrWNs_sp make(const string& nm) {
      auto result = StrWNs_O::make(nm.size(),'\0',false,_Nil<T_O>(),_Nil<T_O>(),false,clasp_make_fixnum(0));
      return result;
    }
    static StrWNs_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
      return StrWNs_O::make(bufferSize, simple_element_type()/*' '*/, true, clasp_make_fixnum(0),
                            _Nil<T_O>(),false,clasp_make_fixnum(0));
    };
  public:
  public:
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final;
  public:
    iterator begin() { return &(*this)[0]; };
    iterator end() { return &(*this)[this->length()]; };
    const_iterator begin() const { return &(*this)[0]; };
    const_iterator end() const { return &(*this)[this->length()]; };
  public:
    virtual void __write__(T_sp strm) const final;
    virtual std::string get_std_string() const final;
    virtual std::string __repr__() const final;
  public: // StrWNs specific functions
    /*! Return true if all characters are base characters and the string
        can be downgraded to a base-char string */
    bool all_base_char_p() const;
    /*! Return the smallest character simple-string that can hold this */
    SimpleString_sp asMinimalSimpleString() const final;
  };
}; // namespace core
namespace core {
  FORWARD(MDArrayBaseChar);
};
namespace core {
  class MDArrayBaseChar_O : public template_Array<MDArrayBaseChar_O,SimpleMDArrayBaseChar_O,SimpleBaseString_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArrayBaseChar_O, "MDArrayBaseChar",MDArray_O);
    virtual ~MDArrayBaseChar_O() {};
  public:
    typedef template_Array<MDArrayBaseChar_O,SimpleMDArrayBaseChar_O,SimpleBaseString_O,MDArray_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public: // make array
  MDArrayBaseChar_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  public:
//    virtual bool equalp(T_sp o) const final;
  };
};

namespace core {
  class SimpleMDArrayBaseChar_O : public template_SimpleArray<SimpleMDArrayBaseChar_O,SimpleBaseString_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayBaseChar_O, "SimpleMDArrayBaseChar",SimpleMDArray_O);
    virtual ~SimpleMDArrayBaseChar_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayBaseChar_O,SimpleBaseString_O,SimpleMDArray_O> TemplatedBase;
    typedef typename TemplatedBase::simple_element_type simple_element_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public: // make array
  SimpleMDArrayBaseChar_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
}; // namespace core

namespace core {
  FORWARD(MDArrayCharacter);
};
namespace core {
  class MDArrayCharacter_O : public template_Array<MDArrayCharacter_O,SimpleMDArrayCharacter_O,SimpleCharacterString_O,MDArray_O> {
    LISP_CLASS(core, CorePkg, MDArrayCharacter_O, "MDArrayCharacter",MDArray_O);
    virtual ~MDArrayCharacter_O() {};
  public:
    typedef template_Array<MDArrayCharacter_O,SimpleMDArrayCharacter_O,SimpleCharacterString_O,MDArray_O> TemplatedBase;
  public: // make array
  MDArrayCharacter_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset) : TemplatedBase(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  public:
//    virtual bool equalp(T_sp o) const final;
  };
};

namespace core {
  class SimpleMDArrayCharacter_O : public template_SimpleArray<SimpleMDArrayCharacter_O,SimpleCharacterString_O,SimpleMDArray_O> {
    LISP_CLASS(core, CorePkg, SimpleMDArrayCharacter_O, "SimpleMDArrayCharacter",SimpleMDArray_O);
    virtual ~SimpleMDArrayCharacter_O() {};
  public:
    typedef template_SimpleArray<SimpleMDArrayCharacter_O,SimpleCharacterString_O,SimpleMDArray_O> TemplatedBase;
  public: // make array
  SimpleMDArrayCharacter_O(size_t rank,
                  List_sp dimensions,
                  Array_sp data) : TemplatedBase(rank,dimensions,data) {};
  };
}; // namespace core

namespace core {
  
  String_sp cl__string(T_sp str);
  SimpleString_sp cl__string_upcase(T_sp arg);
  SimpleString_sp cl__string_downcase(T_sp arg);
  String_sp cl__nstring_upcase(String_sp arg);
  String_sp cl__nstring_downcase(String_sp arg);
  Character_sp cl__char(String_sp str, size_t idx);

  bool clasp_memberChar(claspChar c, String_sp charBag);

  String_sp cl__string_trim(T_sp charbag, T_sp str);
  String_sp cl__string_left_trim(T_sp charbag, T_sp str);
  String_sp cl__string_right_trim(T_sp charbag, T_sp str);

  T_mv cl__parse_integer(String_sp str, Fixnum start = 0, T_sp end = _Nil<T_O>(), uint radix = 10, T_sp junkAllowed = _Nil<T_O>());

  T_sp cl__string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1 = clasp_make_fixnum(0), T_sp end1 = _Nil<T_O>(), Fixnum_sp start2 = clasp_make_fixnum(0), T_sp end2 = _Nil<T_O>());

  /*! Push a c-style string worth of characters into the buffer */
  void StringPushStringCharStar(String_sp buffer, const char* cp);
  void StringPushSubString(String_sp buffer, String_sp other, size_t start, size_t end);
  void StringPushString(String_sp buffer, String_sp other);

  T_sp cl__string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1=clasp_make_fixnum(0), T_sp end1=_Nil<T_O>(), Fixnum_sp start2=clasp_make_fixnum(0), T_sp end2=_Nil<T_O>());

  T_sp core__search_string(String_sp sub, size_t sub_start, size_t sub_end, String_sp outer, size_t outer_start, size_t outer_end );
  bool core__fits_in_base_string(T_sp str);
  T_sp core__copy_to_simple_base_string(T_sp buffer);
};

namespace core {

  extern bool clasp_isupper(claspCharacter cc);
  extern bool clasp_islower(claspCharacter cc);
  
template <class StringType>
int template_string_case(const StringType& s) {
  int upcase = 0;
  for (typename StringType::const_iterator it = s.begin(); it!=s.end(); ++it ) {
    claspCharacter cc = static_cast<claspCharacter>(*it);
    if (clasp_isupper(cc)) {
      if (upcase < 0) return 0;
      upcase = +1;
    } else if (clasp_islower(cc)) {
      if (upcase > 0) return 0;
      upcase = -1;
    }
  }
  return upcase;
}

inline int clasp_string_case(SimpleString_sp s) {
  if (SimpleBaseString_sp sb = s.asOrNull<SimpleBaseString_O>())
    return template_string_case(*sb);
  return template_string_case(*gc::As_unsafe<SimpleCharacterString_sp>(s));
}
inline int clasp_string_case(StrNs_sp s) {
  if (Str8Ns_sp sb = s.asOrNull<Str8Ns_O>())
    return template_string_case(*sb);
  return template_string_case(*gc::As_unsafe<StrWNs_sp>(s));
}
inline int clasp_string_case(String_sp s) {
  if (SimpleString_sp sb = s.asOrNull<SimpleString_O>())
    return clasp_string_case(sb);
  return clasp_string_case(gc::As_unsafe<StrNs_sp>(s));
}

}; // namespace core
