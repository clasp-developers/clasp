
// Nothing for now

#include <clasp/core/core.h>
#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/llvmo/intrinsics.h>

#if 0 // DEBUGGING
#define NO_UNWIND_BEGIN_BUILTINS() NO_UNWIND_BEGIN()
#define NO_UNWIND_END_BUILTINS() NO_UNWIND_END()
#define ENSURE_VALID_OBJECT_BUILTINS(x) ENSURE_VALID_OBJECT(x)
#else
#define NO_UNWIND_BEGIN_BUILTINS()
#define NO_UNWIND_END_BUILTINS()
#define ENSURE_VALID_OBJECT_BUILTINS(x) x
#endif

#define LINKAGE __attribute__ ((visibility ("default")))

#define BUILTIN_ATTRIBUTES __attribute__((always_inline))


extern "C" {

#if !( (FIXNUM0_TAG<CPTR_TAG) && (CPTR_TAG<CHARACTER_TAG) && (CHARACTER_TAG<CONS_TAG) && (CONS_TAG<FIXNUM1_TAG) && (FIXNUM1_TAG<VASLIST_TAG) && (VASLIST_TAG<SINGLE_FLOAT_TAG) && (SINGLE_FLOAT_TAG<GENERAL_TAG))
#error "The tag values do not match the order needed by cc_read_stamp"
#endif
#if !( (FLAGS_STAMP_IN_HEADER < FLAGS_STAMP_IN_RACK) && (FLAGS_STAMP_IN_RACK < FLAGS_STAMP_IN_WRAPPER) && (FLAGS_STAMP_IN_WRAPPER < FLAGS_STAMP_IN_CALLBACK))
#error "The FLAGS_STAMP_IN_xxx values do not match the order needed by cc_read_stamp"
#endif
 
BUILTIN_ATTRIBUTES int64_t cc_read_stamp(void* tagged_pointer)
{
  uintptr_t tag = reinterpret_cast<uintptr_t>(tagged_pointer)&gctools::tag_mask;
  int64_t stamp;
  switch (tag) {
  case FIXNUM0_TAG:
      return gctools::STAMP_FIXNUM;
  case CPTR_TAG:
      return gctools::STAMP_CPOINTER;
  case CHARACTER_TAG:
      return gctools::STAMP_CHARACTER;
  case CONS_TAG:
      return gctools::STAMP_CONS;
  case FIXNUM1_TAG:
      return gctools::STAMP_FIXNUM;
  case VASLIST_TAG:
      return gctools::STAMP_VA_LIST_S;
  case SINGLE_FLOAT_TAG:
      return gctools::STAMP_SINGLE_FLOAT;
  case GENERAL_TAG: {
  // do more stuff to get the stamp
    core::General_O* client_ptr = reinterpret_cast<core::General_O*>(gctools::untag_general<core::T_O*>(reinterpret_cast<core::T_O*>(tagged_pointer)));
    uint64_t* header_ptr = reinterpret_cast<uint64_t*>(ClientPtrToBasePtr(client_ptr));
    uint64_t header = *header_ptr;
    uint64_t header_flags = (header&gctools::Header_s::flags_mask)>>gctools::Header_s::flags_shift;
    switch (header_flags) {
    case FLAGS_STAMP_IN_HEADER: 
        return header>>gctools::Header_s::stamp_shift;
    case FLAGS_STAMP_IN_RACK: {
      core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(gctools::untag_general<core::T_O*>(reinterpret_cast<core::T_O*>(tagged_pointer)));
      core::SimpleVector_O* rack = reinterpret_cast<core::SimpleVector_O*>(gctools::untag_general<core::T_O*>(instance_ptr->_Rack.raw_()));
      return (*rack)[0].unsafe_fixnum();
    }
    case FLAGS_STAMP_IN_WRAPPER: {
      core::WrappedPointer_O* wrapped_ptr = reinterpret_cast<core::WrappedPointer_O*>(client_ptr);
      return wrapped_ptr->Stamp_;
    }
    case FLAGS_STAMP_IN_CALLBACK:
        return client_ptr->get_stamp_();
    }
  }
  }
  return 123456;
}

};





