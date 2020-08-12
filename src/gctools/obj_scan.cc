

/*
 * Object scanner - include this and modify the following macros 
 *  to customize this code for different purposes.
 *
 *
#define SCAN_STRUCT_T int          // Type of scanning struct
#define ADDR_T mps_addr_t          // Type of addresses
#define OBJECT_SCAN fixup_objects  // Name of function
#define SCAN_BEGIN(x)              // Macro for starting scanning block
#define SCAN_END(x)                // Macro for end of scanning block
#define POINTER_FIX(field)         // Macro to fix pointer at field
#define GC_OBJECT_SCAN             // Macro to turn on #ifdef inclusion of code
 */

#ifdef GC_OBJECT_SCAN
GC_RESULT OBJECT_SCAN(SCAN_STRUCT_T ss, ADDR_T client, ADDR_T limit) {
  LOG(BF("obj_scan START client=%p limit=%p\n") % (void*)client % (void*)limit );
  ADDR_T oldClient;
  size_t stamp_index;
  size_t size;
  SCAN_BEGIN(GC_SCAN_STATE) {
    while (client < limit) {
      oldClient = (ADDR_T)client;
      // The client must have a valid header
      const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(gctools::ClientPtrToBasePtr(client));
      const gctools::Header_s::StampWtagMtag& header_value = header._stamp_wtag_mtag;
      stamp_index = header.stamp_();
      LOG(BF("obj_scan client=%p stamp=%lu\n") % (void*)client % stamp_index );
      gctools::tagged_stamp_t mtag = header_value.mtag();
      switch (mtag) {
      case gctools::Header_s::stamp_tag: {
#ifdef DEBUG_VALIDATE_GUARD
        header->validate();
#endif
        gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
        const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
        if ( stamp_wtag == gctools::STAMP_core__DerivableCxxObject_O ) {
        // If this is true then I think we need to call virtual functions on the client
        // to determine the Instance_O offset and the total size of the object.
          printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__ );
        }
        if (stamp_layout.layout_op == gctools::templated_op ) {
          size = ((core::General_O*)client)->templatedSizeof();
        } else {
          size = stamp_layout.size;
        }
        if ( stamp_layout.field_layout_start ) {
          int num_fields = stamp_layout.number_of_fields;
          const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
          for ( int i=0; i<num_fields; ++i ) {
            core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
            POINTER_FIX(field);
            ++field_layout_cur;
          }
        }
        if ( stamp_layout.container_layout ) {
          const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
          size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
          size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
          size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
          for ( int i=0; i<end; ++i ) {
            gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
            ASSERT(field_layout_cur);
            const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
            for ( int j=0; j<container_layout.number_of_fields; ++j ) {
              core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
              POINTER_FIX(field);
              ++field_layout_cur;
            }
          }
        }
        client = (ADDR_T)((char*)client + gctools::AlignUp(size + sizeof(gctools::Header_s)) + header.tail_size());
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object with stamp %u will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, header.stamp_(), scan_size, skip_size);
            obj_skip(oldClient);
          }
        }
#endif
        break;
      }
      case gctools::Header_s::fwd_tag: {
        client = (char *)(client) + header.fwdSize();
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object with fwd_tag will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, scan_size, skip_size);
          }
        }
#endif
        break;
      }
      case gctools::Header_s::pad_tag: {
        if (header_value.pad1P()) {
          client = (char *)(client) + header.pad1Size();
        } else if (header.padP()) {
          client = (char *)(client) + header.padSize();
        }
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object with pad_tag will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, scan_size, skip_size);
          }
        }
#endif
        break;
      }
      case gctools::Header_s::invalid_tag: {
        throw_hard_error_bad_client((void*)client);
      }
      }
    }
  } SCAN_END(GC_SCAN_STATE);
  LOG(BF("obj_scan ENDING client=%p\n") % (void*)client );
  return MPS_RES_OK;
}
#endif // GC_SCAN
