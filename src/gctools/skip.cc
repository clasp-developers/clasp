#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/memoryManagement.h>

namespace gctools {

size_t cons_skip(core::Cons_O* client) {
  (void)client;
  return sizeof(core::Cons_O);
}

size_t general_skip(core::General_O* client) {
  const BaseHeader_s& header = *base_header_ptr(client);
  const BaseHeader_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;

  GCStampEnum stamp_wtag = header_value.stamp_wtag();
  size_t stamp_index = header_value.stamp_();
  const Stamp_layout& stamp_layout = global_stamp_layout[stamp_index];
  const Container_layout* container_layout = stamp_layout.container_layout;
  if (container_layout) {
    // abs is there because for bignums, we use a negative length to
    // indicate that the bignum is negative.
    size_t capacity = std::abs(*(int64_t*)((const char*)client + container_layout->capacity_offset));
    if (container_layout->bits_per_bitunit) { // sub-byte array
      return AlignUp(bitunit_sizeof(container_layout->bits_per_bitunit, capacity) + container_layout->data_offset);
    } else if (stamp_wtag == STAMPWTAG_core__SimpleBaseString_O) [[unlikely]] { // Account for the SimpleBaseString additional byte for \0
      return AlignUp(container_layout->element_size * (capacity + 1) + container_layout->data_offset);
    } else if (stamp_wtag == STAMPWTAG_llvmo__ObjectFile_O) [[unlikely]] {
      llvmo::ObjectFile_O* code = (llvmo::ObjectFile_O*)client;
      return AlignUp(llvmo::ObjectFile_O::sizeofInState(code, code->_State));
    } else {
      return AlignUp(container_layout->element_size * capacity + container_layout->data_offset);
    }
  } else if (stamp_layout.layout_op == templated_op) {
    return AlignUp(client->templatedSizeof());
  } else { // normal object, not a container or template or anything
    return AlignUp(stamp_layout.size);
  }
}

}; // namespace gctools
