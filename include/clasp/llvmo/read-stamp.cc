namespace llvmo {
template <typename TTT>
core::T_O* template_read_stamp(TTT* obj)
{
  uintptr_t tag = reinterpret_cast<uintptr_t>(obj)&gctools::ptag_mask;
  int64_t stamp;
  switch (tag) {
  case FIXNUM0_TAG:
  case FIXNUM1_TAG:
#if TAG_BITS==4
  case FIXNUM2_TAG:
  case FIXNUM3_TAG:
#endif
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMPWTAG_FIXNUM);
  case GENERAL_TAG: {
  // do more stuff to get the stamp
    core::General_O* client_ptr = reinterpret_cast<core::General_O*>(gctools::untag_general<TTT*>(obj));
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(gctools::GeneralPtrToHeaderPtr(client_ptr));
    uint64_t stamp = header.shifted_stamp();
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(stamp));
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Instance_O)));
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMPWTAG_core__FuncallableInstance_O)));
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMPWTAG_clbind__ClassRep_O)));
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMPWTAG_core__WrappedPointer_O)));
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMPWTAG_core__DerivableCxxObject_O)));
    if (stamp == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Instance_O) ||
        stamp == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__FuncallableInstance_O) ||
        stamp == DO_SHIFT_STAMP(gctools::STAMPWTAG_clbind__ClassRep_O)) {
      core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(client_ptr);
      core::Rack_O* rack = reinterpret_cast<core::Rack_O*>(gctools::untag_general<core::T_O*>(instance_ptr->rack().raw_()));
      return (core::T_O*)rack->_ShiftedStamp;
    } else if ( stamp == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__WrappedPointer_O) ) {
      core::WrappedPointer_O* wrapped_ptr = reinterpret_cast<core::WrappedPointer_O*>(client_ptr);
      return (core::T_O*)wrapped_ptr->ShiftedStamp_;
    } else if ( stamp == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__DerivableCxxObject_O) ) {
      core::DerivableCxxObject_O* derivable_cxx_object_ptr = reinterpret_cast<core::DerivableCxxObject_O*>(client_ptr);
      return (core::T_O*)derivable_cxx_object_ptr->get_stamp_();
    } else {
      return (core::T_O*)stamp;
    }
  }
  case CHARACTER_TAG:
      ASSERT(gctools::Header_s::StampWtagMtag::is_unshifted_stamp(gctools::STAMPWTAG_CHARACTER));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMPWTAG_CHARACTER);
  case CONS_TAG:
      ASSERT(gctools::Header_s::StampWtagMtag::is_unshifted_stamp(gctools::STAMPWTAG_CONS));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMPWTAG_CONS);
  case VASLIST0_TAG:
#if TAG_BITS==4
  case VASLIST1_TAG:
#endif
      ASSERT(gctools::Header_s::StampWtagMtag::is_unshifted_stamp(gctools::STAMPWTAG_VASLIST_S));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMPWTAG_VASLIST_S);
  case SINGLE_FLOAT_TAG:
      ASSERT(gctools::Header_s::StampWtagMtag::is_unshifted_stamp(gctools::STAMPWTAG_SINGLE_FLOAT));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMPWTAG_SINGLE_FLOAT);
  }
  UNREACHABLE();
}



  // do more stuff to get the stamp
inline core::T_O* template_read_general_stamp(core::General_O* client_ptr) {
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(gctools::GeneralPtrToHeaderPtr(client_ptr));
  uint64_t stamp = header.shifted_stamp();
  return (core::T_O*)stamp;
}

inline core::T_O* template_read_rack_stamp(core::General_O* client_ptr) {
  core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(client_ptr);
  core::Rack_O* rack = reinterpret_cast<core::Rack_O*>(gctools::untag_general<core::T_O*>(instance_ptr->rack().raw_()));
  return (core::T_O*)rack->_ShiftedStamp;
}

inline core::T_O* template_read_wrapped_stamp(core::General_O* client_ptr) {
  core::WrappedPointer_O* wrapped_ptr = reinterpret_cast<core::WrappedPointer_O*>(client_ptr);
  return (core::T_O*)wrapped_ptr->ShiftedStamp_;
}

inline core::T_O* template_read_derived_stamp(core::General_O* client_ptr) {
  core::DerivableCxxObject_O* derivable_cxx_object_ptr = reinterpret_cast<core::DerivableCxxObject_O*>(client_ptr);
  return (core::T_O*)derivable_cxx_object_ptr->get_stamp_();
}

};
