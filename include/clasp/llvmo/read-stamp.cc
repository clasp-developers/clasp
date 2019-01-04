namespace llvmo {
template <typename TTT>
core::T_O* template_read_stamp(TTT* obj)
{
  uintptr_t tag = reinterpret_cast<uintptr_t>(obj)&gctools::tag_mask;
  int64_t stamp;
  switch (tag) {
  case FIXNUM0_TAG:
      return core::make_fixnum(gctools::STAMP_FIXNUM).raw_();
  case GENERAL_TAG: {
  // do more stuff to get the stamp
    core::General_O* client_ptr = reinterpret_cast<core::General_O*>(gctools::untag_general<TTT*>(obj));
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client_ptr));
    uint64_t stamp = header.stamp();
    if (stamp == gctools::STAMP_core__Instance_O ||
        stamp == gctools::STAMP_core__FuncallableInstance_O ||
        stamp == global_TheClassRep_stamp ) {
      core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(client_ptr);
      core::SimpleVector_O* rack = reinterpret_cast<core::SimpleVector_O*>(gctools::untag_general<core::T_O*>(instance_ptr->_Rack.raw_()));
      return core::make_fixnum((*rack)[0].unsafe_fixnum()).raw_();
    } else if ( stamp == gctools::STAMP_core__WrappedPointer_O ) {
      core::WrappedPointer_O* wrapped_ptr = reinterpret_cast<core::WrappedPointer_O*>(client_ptr);
      return core::make_fixnum(wrapped_ptr->Stamp_).raw_();
    } else if ( stamp == gctools::STAMP_core__DerivableCxxObject_O ) {
      core::DerivableCxxObject_O* derivable_cxx_object_ptr = reinterpret_cast<core::DerivableCxxObject_O*>(client_ptr);
      return core::make_fixnum(derivable_cxx_object_ptr->get_stamp_()).raw_();
    } else {
      return core::make_fixnum(stamp).raw_();
    }
  }
  case CHARACTER_TAG:
      return core::make_fixnum(gctools::STAMP_CHARACTER).raw_();
  case CONS_TAG:
      return core::make_fixnum(gctools::STAMP_CONS).raw_();
  case FIXNUM1_TAG:
      return core::make_fixnum(gctools::STAMP_FIXNUM).raw_();
  case VASLIST_TAG:
      return core::make_fixnum(gctools::STAMP_VA_LIST_S).raw_();
  case SINGLE_FLOAT_TAG:
      return core::make_fixnum(gctools::STAMP_SINGLE_FLOAT).raw_();
  }
  return core::make_fixnum(123456).raw_();
}
};
