template <typename SetterPolicies, typename OT, typename VariablePtrType>
class WRAPPER_Setter
  : public core::GlobalEntryPointBase_O {
public:
  typedef WRAPPER_Setter<SetterPolicies,OT,VariablePtrType> MyType;
  typedef core::GlobalEntryPointBase_O TemplatedBase;
  typedef typename memberpointertraits<VariablePtrType>::member_type MemberType;
  typedef clbind::Wrapper<MemberType,MemberType*> WrapperType;

public:
  VariablePtrType mptr;

public:
  WRAPPER_Setter(VariablePtrType ptr, core::FunctionDescription_sp fdesc, core::T_sp code )
      : mptr(ptr), GlobalEntryPointBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code) {
  };

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    printf("%s:%d:%s What do we do with mptr %p\n", __FILE__, __LINE__, __FUNCTION__, *(void**)&this->mptr );
    //this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    ASSERT(lcc_nargs==2);
    core::T_sp arg0((gctools::Tagged)lcc_args[0]);
    core::T_sp arg1((gctools::Tagged)lcc_args[1]);
    OT *objPtr = gc::As<core::WrappedPointer_sp>(arg1)->cast<OT>();
    translate::from_object<MemberType> fvalue(arg0);
    (*objPtr).*(closure->mptr) = fvalue._v;
    gctools::return_type retv(arg0.raw_(),1);
    return retv;
  }
  static inline LISP_ENTRY_0() {
    return entry_point_n(lcc_closure,0,NULL);
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure,1,args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return entry_point_n(lcc_closure,2,args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return entry_point_n(lcc_closure,3,args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return entry_point_n(lcc_closure,4,args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return entry_point_n(lcc_closure,5,args);
  }

};
