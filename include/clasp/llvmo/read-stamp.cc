namespace llvmo {
template <typename TTT>
core::T_O* template_read_stamp(TTT* obj)
{
  uintptr_t tag = reinterpret_cast<uintptr_t>(obj)&gctools::tag_mask;
  int64_t stamp;
  switch (tag) {
  case FIXNUM0_TAG:
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMP_FIXNUM);
  case GENERAL_TAG: {
  // do more stuff to get the stamp
    core::General_O* client_ptr = reinterpret_cast<core::General_O*>(gctools::untag_general<TTT*>(obj));
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client_ptr));
    uint64_t stamp = header.shifted_stamp();
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(stamp));
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMP_core__Instance_O)));
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMP_core__FuncallableInstance_O)));
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMP_clbind__ClassRep_O)));
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMP_core__WrappedPointer_O)));
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(DO_SHIFT_STAMP(gctools::STAMP_core__DerivableCxxObject_O)));
    if (stamp == DO_SHIFT_STAMP(gctools::STAMP_core__Instance_O) ||
        stamp == DO_SHIFT_STAMP(gctools::STAMP_core__FuncallableInstance_O) ||
        stamp == DO_SHIFT_STAMP(gctools::STAMP_clbind__ClassRep_O)) {
      core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(client_ptr);
      core::Rack_O* rack = reinterpret_cast<core::Rack_O*>(gctools::untag_general<core::T_O*>(instance_ptr->_Rack.raw_()));
      return (core::T_O*)rack->_ShiftedStamp;
    } else if ( stamp == DO_SHIFT_STAMP(gctools::STAMP_core__WrappedPointer_O) ) {
      core::WrappedPointer_O* wrapped_ptr = reinterpret_cast<core::WrappedPointer_O*>(client_ptr);
      return (core::T_O*)wrapped_ptr->ShiftedStamp_;
    } else if ( stamp == DO_SHIFT_STAMP(gctools::STAMP_core__DerivableCxxObject_O) ) {
      core::DerivableCxxObject_O* derivable_cxx_object_ptr = reinterpret_cast<core::DerivableCxxObject_O*>(client_ptr);
      return (core::T_O*)derivable_cxx_object_ptr->get_stamp_();
    } else {
      return (core::T_O*)stamp;
    }
  }
  case CHARACTER_TAG:
      ASSERT(gctools::Header_s::Value::is_unshifted_stamp(gctools::STAMP_CHARACTER));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMP_CHARACTER);
  case CONS_TAG:
      ASSERT(gctools::Header_s::Value::is_unshifted_stamp(gctools::STAMP_CONS));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMP_CONS);
  case FIXNUM1_TAG:
      ASSERT(gctools::Header_s::Value::is_unshifted_stamp(gctools::STAMP_FIXNUM));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMP_FIXNUM);
  case VASLIST_TAG:
      ASSERT(gctools::Header_s::Value::is_unshifted_stamp(gctools::STAMP_VA_LIST_S));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMP_VA_LIST_S);
  case SINGLE_FLOAT_TAG:
      ASSERT(gctools::Header_s::Value::is_unshifted_stamp(gctools::STAMP_SINGLE_FLOAT));
      return (core::T_O*)DO_SHIFT_STAMP(gctools::STAMP_SINGLE_FLOAT);
  }
  unreachableError();
  return (core::T_O*)0;
}



  // do more stuff to get the stamp
inline core::T_O* template_read_general_stamp(core::General_O* client_ptr) {
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client_ptr));
  uint64_t stamp = header.shifted_stamp();
  return (core::T_O*)stamp;
}

inline core::T_O* template_read_rack_stamp(core::General_O* client_ptr) {
  core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(client_ptr);
  core::Rack_O* rack = reinterpret_cast<core::Rack_O*>(gctools::untag_general<core::T_O*>(instance_ptr->_Rack.raw_()));
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
