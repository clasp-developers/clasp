

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
#define GC_RESULT_TYPE             // Result type 
#define RETURN_OK                  // value to return on OK - MPS_RES_OK
 */

//#define DEBUG_OBJECT_SCAN 1
//#define DEBUG_POINTER_BITMAPS 1




#ifdef GC_OBJECT_SCAN
GC_RESULT_TYPE OBJECT_SCAN(SCAN_STRUCT_T ss, ADDR_T client, ADDR_T limit EXTRA_ARGUMENTS) {
#ifdef DEBUG_OBJECT_SCAN
  printf("%s:%d obj_scan client = %p  limit = %p\n", __FILE__, __LINE__, client, limit );
#endif
  
  ADDR_T oldClient;
  size_t stamp_index;
  size_t size;
  SCAN_BEGIN(ss) {
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
          // Handle Lisp_O object specially because it's bitmask will be too large
          if ( stamp_wtag == gctools::STAMP_core__Lisp_O ) {
            int num_fields = stamp_layout.number_of_fields;
            const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
            for ( int i=0; i<num_fields; ++i ) {
              core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
              POINTER_FIX(field);
              ++field_layout_cur;
            }
          } else {
#if 1
            // Use pointer bitmaps
            int idx = 0;
            uintptr_t pointer_bitmap = stamp_layout.class_field_pointer_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
            const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
            for (uintptr_t* addr = (uintptr_t*)client; pointer_bitmap; addr++, pointer_bitmap<<=1) {
              if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
            
                core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
                if (addr != (uintptr_t*)field) {
                  printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! field_offset=%lu\n", __FILE__, __LINE__, stamp_index, client, addr, field, field_layout_cur->field_offset);
                }
                ++field_layout_cur;
#endif
                POINTER_FIX((core::T_O**)addr);
              }
            }
#else          
            int num_fields = stamp_layout.number_of_fields;
            const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
            for ( int i=0; i<num_fields; ++i ) {
              core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
              POINTER_FIX(field);
              ++field_layout_cur;
            }
#endif
          }
        }
        if ( stamp_layout.container_layout ) {
          const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
          size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
          size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
          size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
#if 1
          // Use new way with pointer bitmaps
          uintptr_t start_pointer_bitmap = stamp_layout.container_layout->container_field_pointer_bitmap;
          if (start_pointer_bitmap) {
            if (!(start_pointer_bitmap<<1)) {
              // Trivial case - there is a single pointer to fix in every element and its the only element
              const char* element = ((const char*)client + stamp_layout.data_offset);
              for ( int i=0; i<end; ++i, element += (stamp_layout.element_size)) {
                uintptr_t* addr = (uintptr_t*)element;
#ifdef DEBUG_POINTER_BITMAPS
                gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
                const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
                core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
                if (addr != (uintptr_t*)field) {
                  printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address mismatch!!!! field_layout_cur->field_offset=%lu\n", __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field, field_layout_cur->field_offset);
                }
                ++field_layout_cur;
#endif
                POINTER_FIX((core::T_O**)addr);
              }
            } else {
              // The contents of the container are more complicated - so use the bitmap to scan pointers within them
              const char* element = ((const char*)client + stamp_layout.data_offset);
              for ( int i=0; i<end; ++i, element += (stamp_layout.element_size)) {
#ifdef DEBUG_POINTER_BITMAPS
                gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
#endif
                uintptr_t pointer_bitmap = start_pointer_bitmap;
                for (uintptr_t* addr = (uintptr_t*)element; pointer_bitmap; addr++, pointer_bitmap<<=1) {
                  if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
                    const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
                    core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
                    if (addr != (uintptr_t*)field) {
                      printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address mismatch!!!! field_layout_cur->field_offset=%lu\n", __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field, field_layout_cur->field_offset);
                    }
                    ++field_layout_cur;
#endif
                    POINTER_FIX((core::T_O**)addr);
                  }
                }
              }                
            }
          }
#else
          // Use old way with field offsets
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
#endif
        }
        client = (ADDR_T)((char*)client + gctools::AlignUp(size + sizeof(gctools::Header_s)) + header.tail_size());
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object at client %p with stamp %u will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, (void*)oldClient, header.stamp_(), scan_size, skip_size);
            obj_skip(oldClient);
          }
        }
#endif
        break;
      }
#ifdef USE_MPS          
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
#endif // USE_MPS
      case gctools::Header_s::invalid_tag: {
        throw_hard_error_bad_client((void*)client);
      }
      }
    }
  } SCAN_END(ss);
  LOG(BF("obj_scan ENDING client=%p\n") % (void*)client );
  return RETURN_OK;
}
#endif // GC_OBJECT_SCAN


#ifdef GC_OBJECT_SKIP

ADDR_T OBJECT_SKIP(ADDR_T client,bool dbg) {
  ADDR_T oldClient = client;
  size_t size = 0;
  const gctools::Header_s* header_ptr = reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client));
  const gctools::Header_s& header = *header_ptr;
  const Header_s::StampWtagMtag& header_value = header._stamp_wtag_mtag;
  tagged_stamp_t mtag = header_value.mtag();
#ifdef DEBUG_ON
  if (dbg) {
    LOG(BF("obj_scan_debug mtag = %d  AlignUp(size + sizeof(Header_s)) -> %lu + header.tail_size())-> %lu\n")
        % mtag % (AlignUp(size + sizeof(Header_s))) % header.tail_size() );
  }
#endif
  switch (mtag) {
  case gctools::Header_s::stamp_tag: {
#ifdef DEBUG_VALIDATE_GUARD
    header->validate();
#endif
    gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
    size_t stamp_index = header.stamp_();
#ifdef DEBUG_ON
    if (dbg) {
      LOG(BF("stamp_wtag = %lu stamp_index=%lu\n") % (size_t)stamp_wtag % stamp_index);
    }
#endif
    if ( stamp_wtag == STAMP_core__DerivableCxxObject_O ) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG(BF("DerivableCxxObject\n"));
      }
#endif
        // If this is true then I think we need to call virtual functions on the client
        // to determine the Instance_O offset and the total size of the object.
      printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__ );
    }
    const Stamp_layout& stamp_layout = global_stamp_layout[stamp_index];
    unlikely_if ( stamp_wtag == STAMP_core__SimpleBitVector_O ) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("SimpleBitVector\n"));}
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = core::SimpleBitVector_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
        // Do other bitunit vectors here
    }
    unlikely_if ( stamp_wtag == STAMP_core__SimpleVector_byte2_t_O ) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("STAMP_core__SimpleVector_byte2_t_O"));}
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = core::SimpleVector_byte2_t_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
    }
    unlikely_if ( stamp_wtag == STAMP_core__SimpleVector_int2_t_O ) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("STAMP_core__SimpleVector_int2_t_O"));}
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = core::SimpleVector_int2_t_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
    }
    unlikely_if ( stamp_wtag == STAMP_core__SimpleVector_byte4_t_O ) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("STAMP_core__SimpleVector_byte4_t_O"));}
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = core::SimpleVector_byte4_t_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
    }
    unlikely_if ( stamp_wtag == STAMP_core__SimpleVector_int4_t_O ) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("STAMP_core__SimpleVector_int4_t_O"));}
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = core::SimpleVector_int4_t_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
    }
    unlikely_if (stamp_wtag == gctools::STAMP_core__SimpleBaseString_O) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("SimpleBaseString\n"));}
#endif
        // Account for the SimpleBaseString additional byte for \0
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset) + 1;
      size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
    }
    if ( stamp_layout.container_layout ) {
#ifdef DEBUG_ON
      if (dbg) {LOG(BF("container_layout\n"));}
#endif
        // special cases
      Container_layout& container_layout = *stamp_layout.container_layout;
        // For bignums we allow the _MaybeSignedLength(capacity) to be a negative value to represent negative bignums
        // because GMP only stores positive bignums.  So the value at stamp_layout.capacity_offset is a signed int64_t
        // Because of this we need to take the absolute value to get the number of entries.
      size_t capacity = (size_t)std::llabs(*(int64_t*)((const char*)client + stamp_layout.capacity_offset));
      size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
    } else {
      if (stamp_layout.layout_op == templated_op) {
#ifdef DEBUG_ON
        if (dbg) {LOG(BF("templatedSizeof\n"));}
#endif
        size = ((core::General_O*)client)->templatedSizeof();
      } else {
#ifdef DEBUG_ON
        if (dbg) {LOG(BF("stamp_layout.size = %lu\n") % stamp_layout.size);}
#endif
        size = stamp_layout.size;
      }
    }
    STAMP_CONTINUE:
    client = (ADDR_T)((char*)client + AlignUp(size + sizeof(Header_s)) + header.tail_size());
    break;
  }
  case gctools::Header_s::fwd_tag: {
    client = (char *)(client) + header.fwdSize();
    break;
  }
  case gctools::Header_s::pad_tag: {
    if (header_value.pad1P()) {
      client = (char *)(client) + header.pad1Size();
    } else {
      client = (char *)(client) + header.padSize();
    }
    break;
  }
  case gctools::Header_s::invalid_tag: {
    throw_hard_error_bad_client((void*)client);
  }
  }
  return client;
}


#endif // GC_OBJECT_SKIP






#ifdef GC_LISP_OBJECT_MARK

#define MAYBE_MARK(addr,WORK)                           \
  gctools::Tagged* taggedP = (gctools::Tagged*)addr;                    \
  gctools::Tagged tagged_obj = *taggedP;                                \
  if (gctools::tagged_objectp(tagged_obj)) {                            \
    gctools::Tagged obj = gctools::untag_object<gctools::Tagged>(tagged_obj); \
    msp = ((GC_word)(obj) >= (GC_word)GC_least_plausible_heap_addr &&   \
           (GC_word)(obj) <= (GC_word)GC_greatest_plausible_heap_addr ? \
           GC_mark_and_push((void*)obj, msp, msl, (void**)addr) : (msp)); \
    WORK; \
  }


extern "C" {
  struct GC_ms_entry* Lisp_O_object_mark(GC_word addr,
                                         struct GC_ms_entry* msp,
                                         struct GC_ms_entry* msl,
                                         GC_word env)
  {
#ifdef DEBUG_OBJECT_SCAN
    printf("%s:%d:%s addr = %p\n", __FILE__, __LINE__,__FUNCTION__, addr );
#endif

    // The client must have a valid header
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(addr);
    void* client = (char*)addr + sizeof(gctools::Header_s);
    const gctools::Header_s::StampWtagMtag& header_value = header._stamp_wtag_mtag;
    size_t stamp_index = header.stamp_();
    gctools::tagged_stamp_t mtag = header_value.mtag();
    gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    int num_fields = stamp_layout.number_of_fields;
    const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
    for ( int i=0; i<num_fields; ++i ) {
      gctools::Tagged* addr = (gctools::Tagged*)((const char*)client + field_layout_cur->field_offset);
      MAYBE_MARK(addr,++field_layout_cur);
    }
    return msp;
  }


  struct GC_ms_entry* cons_mark(GC_word addr,
                                struct GC_ms_entry* msp,
                                struct GC_ms_entry* msl,
                                GC_word env)
  {
#ifdef DEBUG_OBJECT_SCAN
    printf("%s:%d:%s addr = %p\n", __FILE__, __LINE__,__FUNCTION__, addr );
#endif
    // The client must have a valid header
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(addr);
    void* client = (char*)addr;
    MAYBE_MARK((char*)addr+offset(core::Cons_O,_Car));
    MAYBE_MARK((char*)addr+offset(core::Cons_O,_Cdr));
    return msp;
  }

  struct GC_ms_entry* class_mark(GC_word addr,
                                 struct GC_ms_entry* msp,
                                 struct GC_ms_entry* msl,
                                 GC_word env)
  {
#ifdef DEBUG_OBJECT_SCAN
    printf("%s:%d:%s addr = %p\n", __FILE__, __LINE__,__FUNCTION__, addr );
#endif
    // The client must have a valid header
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(addr);
    void* client = (char*)addr + sizeof(gctools::Header_s);
    const gctools::Header_s::StampWtagMtag& header_value = header._stamp_wtag_mtag;
    size_t stamp_index = header.stamp_();
    gctools::tagged_stamp_t mtag = header_value.mtag();
    gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    int idx = 0;
    uintptr_t pointer_bitmap = stamp_layout.boehm._class_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
    const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
    // Only mark the class fields if env == 0
    if (!env) {
      for (uintptr_t* addr = (uintptr_t*)client; pointer_bitmap; addr++, pointer_bitmap<<=1) {
        if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
          core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
          if (addr != (uintptr_t*)field) {
            printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! field_offset=%lu\n", __FILE__, __LINE__, stamp_index, client, addr, field, field_layout_cur->field_offset);
          }
          ++field_layout_cur;
#endif
          MAYBE_MARK(addr,);
        }
      }
    }
    return msp;
  }


  struct GC_ms_entry* pointer_containing_container_mark(GC_word addr,
                                                        struct GC_ms_entry* msp,
                                                        struct GC_ms_entry* msl,
                                                        GC_word env)
  {
#ifdef DEBUG_OBJECT_SCAN
    printf("%s:%d:%s addr = %p\n", __FILE__, __LINE__,__FUNCTION__, addr );
#endif
    // The client must have a valid header
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(addr);
    void* client = (char*)addr + sizeof(gctools::Header_s);
    const gctools::Header_s::StampWtagMtag& header_value = header._stamp_wtag_mtag;
    size_t stamp_index = header.stamp_();
    gctools::tagged_stamp_t mtag = header_value.mtag();
    gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    int idx = 0;
    uintptr_t pointer_bitmap = stamp_layout.boehm._class_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
    const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
    // Only mark the class fields if env == 0
    if (!env) {
      for (uintptr_t* addr = (uintptr_t*)client; pointer_bitmap; addr++, pointer_bitmap<<=1) {
        if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
          core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
          if (addr != (uintptr_t*)field) {
            printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! field_offset=%lu\n", __FILE__, __LINE__, stamp_index, client, addr, field, field_layout_cur->field_offset);
          }
          ++field_layout_cur;
#endif
          MAYBE_MARK(addr,);
        }
      }
    }
    // Now mark the container pointers
    const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
    size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
    size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
    // Use new way with pointer bitmaps
    uintptr_t start_pointer_bitmap = stamp_layout.boehm._container_bitmap;
    if (start_pointer_bitmap) {
      if (!(start_pointer_bitmap<<1)) {
        // Trivial case - there is a single pointer to fix in every element and its the only element
        const char* element = ((const char*)client + stamp_layout.data_offset);
        for ( int i=0; i<end; ++i, element += (stamp_layout.element_size)) {
          uintptr_t* addr = (uintptr_t*)element;
#ifdef DEBUG_POINTER_BITMAPS
          gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
          const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
          core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
          if (addr != (uintptr_t*)field) {
            printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address mismatch!!!! field_layout_cur->field_offset=%lu\n", __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field, field_layout_cur->field_offset);
          }
          ++field_layout_cur;
#endif
          MAYBE_MARK(addr,);
        }
      } else {
        // The contents of the container are more complicated - so use the bitmap to scan pointers within them
        const char* element_start = ((const char*)client + stamp_layout.data_offset);
        for ( int i=0; i<end; ++i, element_start += (stamp_layout.element_size)) {
          // THIS IS WHERE WE BREAK THE WORK INTO CHUNKS AND UPDATE ENV
          printf("%s:%d:%s This is where I need to break work into chunks of %d\n", __FILE__, __LINE__, __FUNCTION__, stamp_layout.boehm._container_element_work );
#ifdef DEBUG_POINTER_BITMAPS
          gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
#endif
          uintptr_t pointer_bitmap = start_pointer_bitmap;
          for (uintptr_t* addr = (uintptr_t*)element_start; pointer_bitmap; addr++, pointer_bitmap<<=1) {
            if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
              const char* elementdbg = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
              core::T_O** field = (core::T_O**)((const char*)elementdbg + field_layout_cur->field_offset);
              if (addr != (uintptr_t*)field) {
                printf("%s:%d stamp: %lu elementdbg@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address mismatch!!!! field_layout_cur->field_offset=%lu\n",
                       __FILE__, __LINE__, stamp_index, elementdbg, i, start_pointer_bitmap, addr, field, field_layout_cur->field_offset);
              }
              ++field_layout_cur;
#endif
              MAYBE_MARK(addr,);
            }
          }
        }                
      }
    }
    return msp;
  }





}
#endif // #ifdef GC_LISP_OBJECT_MARK

