template <typename GetterPolicies, typename OT, typename VariablePtrType>
class WRAPPER_Getter : public core::SimpleFun_O {
public:
  typedef WRAPPER_Getter<GetterPolicies, OT, VariablePtrType> MyType;
  typedef core::SimpleFun_O TemplatedBase;
  typedef typename memberpointertraits<VariablePtrType>::member_type MemberType;

public:
  VariablePtrType mptr;

public:
  WRAPPER_Getter(VariablePtrType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : mptr(ptr), SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()) {
    trapGetterMethoid();
  };

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    //    printf("%s:%d:%s What do we do with mptr %p - I will try fixing\n", __FILE__, __LINE__, __FUNCTION__, *(void**)&this->mptr
    //    );
    this->fixupOneCodePointer(fixup, (void**)&this->mptr);
  };
private:
  inline LCC_RETURN go(core::T_O* a0) {
    core::T_sp arg0((gctools::Tagged)a0);
    OT* objPtr = gc::As<core::WrappedPointer_sp>(arg0)->cast<OT>();
    MemberType& orig = (*objPtr).*(this->mptr);
    return Values(translate::to_object<MemberType, translate::dont_adopt_pointer>::convert(orig));
  }
  
public:
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs == 1)
      return gctools::untag_general<MyType*>((MyType*)lcc_closure)->go(lcc_args[0]);
    else {
      cc_wrong_number_of_arguments(lcc_closure, lcc_nargs, 1, 1);
      UNREACHABLE();
    }
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    if constexpr(sizeof...(Ts) == 1)
      return gctools::untag_general<MyType*>((MyType*)lcc_closure)->go(args...);
    else {
      cc_wrong_number_of_arguments(lcc_closure, sizeof...(Ts), 1, 1);
      UNREACHABLE();
    }
  }
};
