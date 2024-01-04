template <typename GetterPolicies, typename OT, typename MemberType>
class WRAPPER_Getter<GetterPolicies, OT, MemberType* const(OT::*)> : public core::SimpleFun_O {
public:
  typedef WRAPPER_Getter<GetterPolicies, OT, MemberType* const(OT::*)> MyType;
  typedef core::SimpleFun_O TemplatedBase;
  typedef clbind::Wrapper<MemberType, MemberType*> WrapperType;
  typedef MemberType* const(OT::*VariablePtrType);

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
    printf("%s:%d:%s What do we do with mptr %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this->mptr);
    // this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    DO_DRAG_CXX_CALLS();
    core::T_sp arg0((gctools::Tagged)lcc_args[0]);
    OT* objPtr = gc::As<core::WrappedPointer_sp>(arg0)->cast<OT>();
    MemberType* ptr = (*objPtr).*(closure->mptr);
    return translate::to_object<MemberType*, translate::dont_adopt_pointer>::convert(ptr);
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    core::T_O* lcc_args[sizeof...(Ts)] = {args...};
    return entry_point_n(lcc_closure, sizeof...(Ts), lcc_args);
  }
};
