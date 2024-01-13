template <typename SetterPolicies, typename OT, typename MemberType>
class WRAPPER_Setter<SetterPolicies, OT, MemberType* const(OT::*)> : public core::SimpleFun_O {
public:
  typedef WRAPPER_Setter<SetterPolicies, OT, MemberType* const(OT::*)> MyType;
  typedef core::SimpleFun_O TemplatedBase;
  typedef clbind::Wrapper<MemberType, MemberType*> WrapperType;
  typedef MemberType* const(OT::*VariablePtrType);

private:
  VariablePtrType mptr;

public:
  WRAPPER_Setter(VariablePtrType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : mptr(ptr), SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()){};

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    printf("%s:%d:%s What do we do with mptr %p\n", __FILE__, __LINE__, __FUNCTION__, *(void**)&this->mptr);
    // this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

private:
  inline LCC_RETURN go(core::T_O* a0, core::T_O* a1) {
    core::T_sp arg0((gctools::Tagged)a0);
    core::T_sp arg1((gctools::Tagged)a1);
    OT* objPtr = gc::As<core::WrappedPointer_sp>(arg1)->cast<OT>();
    translate::from_object<MemberType> fvalue(arg0);
    (*objPtr).*(this->mptr) = fvalue._v;
    typename gctools::return_type ret(arg0.raw_(), 1);
    return ret;
  }

public:
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs == 2)
      return gctools::untag_general<MyType*>((MyType*)lcc_closure)->go(lcc_args[0], lcc_args[1]);
    else {
      cc_wrong_number_of_arguments(lcc_closure, lcc_nargs, 2, 2);
      UNREACHABLE();
    }
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    DO_DRAG_CXX_CALLS();
    if constexpr(sizeof...(Ts) == 2) {
      return gctools::untag_general<MyType*>((MyType*)lcc_closure)->go(args...);
    } else {
      cc_wrong_number_of_arguments(lcc_closure, sizeof...(Ts), 2, 2);
      UNREACHABLE();
    }
  }
};
