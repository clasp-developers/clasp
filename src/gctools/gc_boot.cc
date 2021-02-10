#include <clasp/core/foundation.h>
#include <clasp/gctools/gctoolsPackage.fwd.h>
#include <clasp/core/array.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/core/lisp.h>
#include <clasp/core/package.h>
#include <clasp/core/numbers.h>

#if defined(USE_BOEHM) && defined(USE_PRECISE_GC)
#include <gc/gc.h>
#include <gc/gc_mark.h>
#endif

#define DUMP_GC_BOOT 1
//#define DUMP_PRECISE_CALC 1



#if defined(USE_BOEHM) && defined(USE_PRECISE_GC)
extern "C" {
void* obj_skip(void*);
};
#define GC_LISP_OBJECT_MARK
#include "obj_scan.cc"
#undef GC_LISP_OBJECT_MARK
#endif


namespace gctools {

int              global_cons_kind;
int              global_code_kind;
int              global_container_proc_index;
int              global_container_kind;
size_t           global_stamp_max;
Stamp_info*       global_stamp_info;
Stamp_layout*     global_stamp_layout;
Field_info*      global_field_info;
Field_layout*    global_field_layout;
Container_layout* global_container_layout;
Container_info*  global_container_info;


inline size_t bitmap_field_index(size_t start,size_t offset) {
  size_t bitindex = start-(offset/8);
  if (bitindex > 63) {
    printf("%s:%d The bit position for offset %lu will not fit in a 64-bit word - the class has pointers beyond the reach of a 62-bit bitmap\n", __FILE__, __LINE__, offset );
  }
  return bitindex;
}

inline uintptr_t bitmap_field_bitmap(size_t bitindex) {
  return (uintptr_t)1 << bitindex;
}

void walk_stamp_field_layout_tables(WalkKind walk, FILE* fout)
{
  std::string indent = "";
  if (walk==lldb_info) {
    if (!fout) {
      printf("%s:%d You must provide a file to do a lldb_walk\n", __FILE__, __LINE__ );
      abort();
    }
    //    fprintf(fout,"import clasp\n");
    //    fprintf(fout,"from clasp import inspect\n");
    dump_data_types(fout,indent);
  }
  // First pass through the global_stamp_layout_codes_table
  // to count the number of stamps and the number of fields
  Layout_code* codes = get_stamp_layout_codes();
  int idx = 0;
  size_t number_of_fixable_fields = 0;
  size_t number_of_containers = 0;
  size_t local_stamp_max = 0;
  size_t num_codes = 0;
  while (1) {
    if ( codes[idx].cmd == layout_end ) break;
    ++num_codes;
    if ( codes[idx].cmd < 0 || codes[idx].cmd > layout_end ) {
      printf("%s:%d the layout code table is damaged\n", __FILE__, __LINE__ );
      abort();
    }
    if ( codes[idx].cmd == class_kind ||
         codes[idx].cmd == container_kind ||
         codes[idx].cmd == bitunit_container_kind ||
         codes[idx].cmd == templated_kind ) {
      size_t stamp_code = STAMP_UNSHIFT_MTAG(codes[idx].data0);
      if ( local_stamp_max < stamp_code ) {
        local_stamp_max = stamp_code+1;
      } else {
      }
    } else if ( (codes[idx].cmd == fixed_field
                 || codes[idx].cmd == variable_field )
                && (codes[idx].data0 == SMART_PTR_OFFSET
                    || codes[idx].data0 == TAGGED_POINTER_OFFSET
                    || codes[idx].data0 == POINTER_OFFSET )) {
      ++number_of_fixable_fields;
    } else if ((codes[idx].cmd == fixed_field)
               && (codes[idx].data0 == CONSTANT_ARRAY_OFFSET)) {
      // Ignore the Array_O size_t _Length[0] array
      if (strcmp(codes[idx].description,"_Length")!=0) {
        //        printf("%s:%d There is an unknown CONSTANT_ARRAY named %s that the static analyzer identified - deal with it\n", __FILE__, __LINE__, codes[idx].description );
      }
    } else if ( codes[idx].cmd == variable_array0
                || codes[idx].cmd == variable_bit_array0 ) {
      ++number_of_containers;
    }
    ++idx;
  }
  // Now allocate memory for the tables
  // now that we know the size of everything
  Stamp_info* local_stamp_info = new Stamp_info[local_stamp_max+1]; // (Stamp_info*)malloc(sizeof(Stamp_info)*(local_stamp_max+1));
  memset(local_stamp_info,0,sizeof(Stamp_info)*(local_stamp_max+1));
  Stamp_layout* local_stamp_layout = new Stamp_layout[local_stamp_max+1]; // (Stamp_layout*)malloc(sizeof(Stamp_layout)*(local_stamp_max+1));
  Field_layout* local_field_layout = new Field_layout[number_of_fixable_fields]; // (Field_layout*)malloc(sizeof(Field_layout)*number_of_fixable_fields);
  Field_layout* cur_field_layout= local_field_layout;
  Field_layout* max_field_layout = (Field_layout*)((char*)local_field_layout + sizeof(Field_layout)*number_of_fixable_fields);
  Field_info* local_field_info = new Field_info[number_of_fixable_fields]; // (Field_info*)malloc(sizeof(Field_info)*(number_of_fixable_fields));
  Field_info* cur_field_info = local_field_info;
  Field_info* max_field_info = (Field_info*)((char*)local_field_info + sizeof(Field_info)*number_of_fixable_fields);
  Container_layout* local_container_layout = new Container_layout[number_of_containers+1]; // (Container_layout*)malloc(sizeof(Container_layout)*(number_of_containers+1));
  Container_info* local_container_info = new Container_info[number_of_containers+1]; // (Container_info*)malloc(sizeof(Container_info)*(number_of_containers+1));
  // Fill in the immediate stamps
  // Traverse the local_stamp_layout_codes again and fill the tables
  codes = get_stamp_layout_codes();
  size_t cur_container_layout_idx = 0;
  size_t cur_container_info_idx = 0;
  int cur_stamp=0;
  idx = 0;
#define STAMP(_stamp_wtag_) (_stamp_wtag_>>(Header_s::wtag_width))
  for ( idx=0; idx<num_codes; ++idx ) {
#ifdef DUMP_GC_BOOT
    printf("%s:%d idx = %d\n", __FILE__, __LINE__, idx);
#endif
    switch (codes[idx].cmd) {
    case class_kind:
      cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
      printf("%s:%d  class_kind  cur_stamp = %d name = %s\n", __FILE__, __LINE__, cur_stamp, codes[idx].description);
#endif
      local_stamp_layout[cur_stamp].layout_op = class_container_op;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      if (walk == lldb_info) fprintf(fout, "%sInit_class_kind( stamp=%d, name=\"%s\", size=%d )\n",
                                     indent.c_str(),
                                     cur_stamp,
                                     codes[idx].description,
                                     local_stamp_layout[cur_stamp].size);
      break;
    case fixed_field:
      {
        size_t data_type = codes[idx].data0;
        size_t index = local_stamp_layout[cur_stamp].number_of_fields;
        const char* field_name = codes[idx].description;
        size_t field_offset = codes[idx].data2;
        if (walk == lldb_info) {
          fprintf(fout,"%sInit__fixed_field( stamp=%d, index=%lu, data_type=%lu, field_name=\"%s\", field_offset=%lu)\n",
                  indent.c_str(),
                  cur_stamp,
                  index,
                  data_type,
                  field_name,
                  field_offset
                  );
        }
        if ( !(data_type == SMART_PTR_OFFSET
               || data_type == TAGGED_POINTER_OFFSET
               || data_type == POINTER_OFFSET )) continue;
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
        // Handle Lisp_O object specially
        // There is a corresponding change to obj_scan.cc
        size_t field_bit;
        uintptr_t field_bitmap;
        if (cur_stamp != STAMP_UNSHIFT_MTAG(gctools::STAMP_core__Lisp_O)) {
          field_bit = bitmap_field_index(63,field_offset);
          field_bitmap = bitmap_field_bitmap(field_bit);
          local_stamp_layout[cur_stamp].class_field_pointer_bitmap |= field_bitmap;
        } else {
          // Do the same for STAMP_core__Lisp_O - but if it doesn't fit in a 64bit word then we will skip this.
          field_bit = bitmap_field_index(63,field_offset);
          field_bitmap = bitmap_field_bitmap(field_bit);
          local_stamp_layout[cur_stamp].class_field_pointer_bitmap |= field_bitmap;
        }
        if ( local_stamp_layout[cur_stamp].field_layout_start == NULL )
          local_stamp_layout[cur_stamp].field_layout_start = cur_field_layout;
#ifdef DUMP_GC_BOOT
        printf("%s:%d   fixed_field %s : cur_stamp = %d  field_offset = %lu bit_index =%lu bitmap = 0x%lX\n", __FILE__, __LINE__, field_name, cur_stamp, field_offset, field_bit, field_bitmap);
        printf("       class_field_pointer_bitmap = 0x%lX\n", local_stamp_layout[cur_stamp].class_field_pointer_bitmap);
#endif
        cur_field_layout->field_offset = field_offset;
        GCTOOLS_ASSERT(cur_field_info<max_field_info);
        if ( local_stamp_info[cur_stamp].field_info_ptr == NULL )
          local_stamp_info[cur_stamp].field_info_ptr = cur_field_info;
        cur_field_info->field_name = field_name;
        cur_field_info->data_type = data_type;
        ++local_stamp_layout[cur_stamp].number_of_fields;
        ++cur_field_layout;
        ++cur_field_info;
      }
      break;
    case container_kind:
      cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
      printf("%s:%d   container_kind  cur_stamp = %d name = %s\n", __FILE__, __LINE__, cur_stamp, codes[idx].description);
#endif
      local_stamp_layout[cur_stamp].layout_op = class_container_op;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      if (walk == lldb_info) fprintf(fout, "%sInit_container_kind( stamp=%d, name=\"%s\", size=%d )\n",
                                     indent.c_str(),
                                     cur_stamp,
                                     local_stamp_info[cur_stamp].name,
                                     local_stamp_layout[cur_stamp].size);
      break;
    case bitunit_container_kind:
      cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
      printf("%s:%d   bitunit_container_kind  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
      local_stamp_layout[cur_stamp].layout_op = bitunit_container_op;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_layout[cur_stamp].bits_per_bitunit = codes[idx].data2;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      if (walk == lldb_info) fprintf(fout, "%sInit_bitunit_container_kind( stamp=%d, name=\"%s\", size=%d, bits_per_bitunit=%d )\n",
                                     indent.c_str(),
                                     cur_stamp,
                                     local_stamp_info[cur_stamp].name,
                                     local_stamp_layout[cur_stamp].size,
                                     local_stamp_layout[cur_stamp].bits_per_bitunit);
      break;
    case variable_array0:
#ifdef DUMP_GC_BOOT
      printf("%s:%d   variable_array0 cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
      local_stamp_layout[cur_stamp].container_layout = &local_container_layout[cur_container_layout_idx++];
      GCTOOLS_ASSERT(cur_container_layout_idx<=number_of_containers);
      local_stamp_layout[cur_stamp].data_offset = codes[idx].data2;
      if (walk == lldb_info) fprintf(fout, "%sInit__variable_array0( stamp=%d, name=\"%s\", offset=%d )\n",
                                     indent.c_str(),
                                     cur_stamp,
                                     codes[idx].description,
                                     local_stamp_layout[cur_stamp].data_offset);
      break;
    case variable_bit_array0:
#ifdef DUMP_GC_BOOT
      printf("%s:%d   variable_bit_array0 cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
      local_stamp_layout[cur_stamp].container_layout = &local_container_layout[cur_container_layout_idx++];
      GCTOOLS_ASSERT(cur_container_layout_idx<=number_of_containers);
      local_stamp_layout[cur_stamp].data_offset = codes[idx].data2;
      local_stamp_layout[cur_stamp].bits_per_bitunit = codes[idx].data0;
      break;
    case variable_capacity:
#ifdef DUMP_GC_BOOT
      printf("%s:%d   variable_capacity cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
      local_stamp_layout[cur_stamp].container_layout->field_layout_start = cur_field_layout;
      local_stamp_layout[cur_stamp].element_size = codes[idx].data0;
      local_stamp_layout[cur_stamp].container_layout->number_of_fields = 0;
      local_stamp_layout[cur_stamp].end_offset = codes[idx].data1;
      local_stamp_layout[cur_stamp].capacity_offset = codes[idx].data2;
      if (walk == lldb_info) fprintf(fout, "%sInit__variable_capacity( stamp=%d, element_size=%d, end_offset=%d, capacity_offset=%d )\n",
                                     indent.c_str(),
                                     cur_stamp,
                                     local_stamp_layout[cur_stamp].element_size,
                                     local_stamp_layout[cur_stamp].end_offset,
                                     local_stamp_layout[cur_stamp].capacity_offset);
      break;
    case variable_field:
      {
        size_t data_type = codes[idx].data0;
        size_t index = local_stamp_layout[cur_stamp].container_layout->number_of_fields;
        size_t field_offset = codes[idx].data2;
        const char* field_name = codes[idx].description;
        if (walk == lldb_info) fprintf(fout, "%sInit__variable_field( stamp=%d, index=%lu, data_type=%lu, field_name=\"%s\", field_offset=%lu )\n",
                                       indent.c_str(),
                                       cur_stamp,
                                       index,
                                       data_type,
                                       field_name,
                                       field_offset
                                       );
        if ( !((data_type) == SMART_PTR_OFFSET
               || (data_type) == TAGGED_POINTER_OFFSET
               || (data_type) == POINTER_OFFSET )) continue;
        size_t field_bit = bitmap_field_index(63,field_offset);
        uintptr_t field_bitmap = bitmap_field_bitmap(field_bit);
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
        local_stamp_layout[cur_stamp].container_layout->container_field_pointer_bitmap |= field_bitmap;
        local_stamp_layout[cur_stamp].container_layout->container_field_pointer_count++;
#ifdef DUMP_GC_BOOT
        printf("%s:%d   variable_field  %s cur_stamp = %d field_offset = %lu field_bit_index = %lu field_bit = 0x%lX\n", __FILE__, __LINE__, field_name, cur_stamp, field_offset, field_bit, field_bitmap );
        printf("        .container_layout->container_field_pointer_bitmap = 0x%lX\n", local_stamp_layout[cur_stamp].container_layout->container_field_pointer_bitmap);
#endif
        cur_field_layout->field_offset = field_offset;
        if ( local_stamp_info[cur_stamp].container_info_ptr == NULL )
          local_stamp_info[cur_stamp].container_info_ptr = &local_container_info[cur_container_info_idx++];
        GCTOOLS_ASSERT(cur_container_info_idx<=number_of_containers);
        local_stamp_info[cur_stamp].container_info_ptr->field_name = field_name;
        local_stamp_info[cur_stamp].container_info_ptr->data_type = data_type;
        ++cur_field_layout;
        ++local_stamp_layout[cur_stamp].container_layout->number_of_fields;
      }
      break;
    case templated_kind:
      {
        cur_stamp = STAMP(codes[idx].data0);
        size_t size = codes[idx].data1;
        const char* name = codes[idx].description;
        if (walk == lldb_info) fprintf(fout, "%sInit_templated_kind( stamp=%d, name=\"%s\", size=%lu )\n",
                                       indent.c_str(),
                                       cur_stamp,
                                       name,
                                       size);
#ifdef DUMP_GC_BOOT
        printf("%s:%d   templated_kind cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        local_stamp_layout[cur_stamp].layout_op = templated_op;
        local_stamp_layout[cur_stamp].field_layout_start = NULL;
        local_stamp_layout[cur_stamp].container_layout = NULL;
        local_stamp_layout[cur_stamp].number_of_fields = 0;
        local_stamp_layout[cur_stamp].size = codes[idx].data1;
        local_stamp_info[cur_stamp].name = codes[idx].description;
        local_stamp_info[cur_stamp].field_info_ptr = NULL;
        local_stamp_info[cur_stamp].container_info_ptr = NULL;
      }
      break;
    case templated_class_jump_table_index:
#ifdef DUMP_GC_BOOT
      printf("%s:%d   templated_class_jump_table_index\n", __FILE__, __LINE__);
#endif
      break;
    case container_jump_table_index:
#ifdef DUMP_GC_BOOT
      printf("%s:%d   container_jump_table_index\n", __FILE__, __LINE__);
#endif
      break;
    default:
      printf("%s:%d Illegal Layout_code table command: %d\n", __FILE__, __LINE__, codes[idx].cmd);
      throw_hard_error_bad_layout_command(codes[idx].cmd);
    }
  }

  // Calculate boehm header
#if defined(USE_BOEHM) && defined(USE_PRECISE_GC)

  // Setup the CONS cell bitmap
  uintptr_t cons_stamp = STAMP_UNSHIFT_MTAG(STAMP_core__Cons_O);
  uintptr_t cons_bitmap = ((uintptr_t)1<<(63-(offsetof(core::Cons_O,_Car)/8)))|((uintptr_t)1<<(63-(offsetof(core::Cons_O,_Cdr)/8)));
  if (cons_bitmap != local_stamp_layout[cons_stamp].class_field_pointer_bitmap) {
    printf("%s:%d The cons stamp %lu cons_bitmap = 0x%lX  and it doesn't match   local_stamp_layout[cons_stamp].class_field_pointer_bitmap = 0x%lX\n", __FILE__, __LINE__, cons_stamp, cons_bitmap, local_stamp_layout[cons_stamp].class_field_pointer_bitmap);
    abort();
  }

  // Use boehm in the precise GC mode
  global_container_proc_index = GC_new_proc_inner((GC_mark_proc)dumb_class_container_mark);
#if 0
  uintptr_t lisp_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
  uintptr_t cons_kind = GC_new_kind(GC_new_free_list(), GC_DS_BITMAP | cons_bitmap, 0, 1 ); // GC_DS_LENGTH, 1, 1);
  uintptr_t class_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
  uintptr_t container_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
#else
  uintptr_t lisp_kind = GC_I_NORMAL; // GC_new_kind(GC_new_free_list(), GC_MAKE_PROC(GC_new_proc((GC_mark_proc)Lisp_O_object_mark),0), 0, 1); // GC_DS_LENGTH, 1, 1);
  uintptr_t cons_kind = GC_new_kind(GC_new_free_list(), GC_DS_BITMAP | cons_bitmap, 0, 1 ); // GC_DS_LENGTH, 1, 1);
  uintptr_t class_kind = GC_new_kind(GC_new_free_list(), GC_MAKE_PROC(GC_new_proc((GC_mark_proc)Lisp_O_object_mark),0), 0, 1); // GC_DS_LENGTH, 1, 1);
  uintptr_t container_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1); // */  GC_new_kind(GC_new_free_list(), GC_MAKE_PROC(global_container_proc_index,0),0,1); // GC_DS_LENGTH, 1, 1);
#endif
  uintptr_t code_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
  uintptr_t atomic_kind = GC_I_PTRFREE; // GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 0, 1);
  global_container_kind = container_kind;
  for ( cur_stamp=0; cur_stamp<local_stamp_max; ++cur_stamp ) {
    if (local_stamp_layout[cur_stamp].layout_op != undefined_op) {
#ifdef DUMP_PRECISE_CALC
      printf("%s:%d --------------------------------------\n", __FILE__, __LINE__ );
      printf("%s:%d calculate boehm header cur_stamp = %d  layout_op %d  %s  \n", __FILE__, __LINE__, cur_stamp, local_stamp_layout[cur_stamp].layout_op, local_stamp_info[cur_stamp].name);
#endif
      if (cur_stamp == STAMP_UNSHIFT_MTAG(STAMP_core__Lisp_O) ) {
        local_stamp_layout[cur_stamp].boehm._kind = lisp_kind;
        local_stamp_layout[cur_stamp].boehm._kind_defined = true;
      } else if (cur_stamp == STAMP_UNSHIFT_MTAG(STAMP_llvmo__Code_O) ) {
        local_stamp_layout[cur_stamp].boehm._kind = code_kind;
        global_code_kind = code_kind;
        local_stamp_layout[cur_stamp].boehm._kind_defined = true;
      } else if (cur_stamp == STAMP_UNSHIFT_MTAG(STAMP_core__Cons_O) ) {
        local_stamp_layout[cur_stamp].boehm._kind = cons_kind;
        global_cons_kind = cons_kind;
#ifdef DUMP_PRECISE_CALC
        printf("%s:%d   cons_bitmap = 0x%lx global_cons_kind = %d  address = %p\n", __FILE__, __LINE__, cons_bitmap, global_cons_kind, &global_cons_kind );
#endif
        local_stamp_layout[cur_stamp].boehm._kind_defined = true;
      } else {
        uintptr_t class_bitmap = (local_stamp_layout[cur_stamp].class_field_pointer_bitmap);
        local_stamp_layout[cur_stamp].boehm._class_bitmap = class_bitmap;
#ifdef DUMP_PRECISE_CALC
        printf("%s:%d stamp = %d  class_bitmap = 0x%lX\n", __FILE__, __LINE__, cur_stamp, class_bitmap );
        const gctools::Stamp_layout& stamp_layout = local_stamp_layout[cur_stamp];
        const gctools::Stamp_info& stamp_info = local_stamp_info[cur_stamp];
        const gctools::Field_info* field_info_cur = stamp_info.field_info_ptr;
        const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
        int num_fields = stamp_layout.number_of_fields;
        for ( int i=0; i<num_fields; ++i ) {
          printf("%s:%d [%d]   offset %zu   %s\n", __FILE__, __LINE__, i, field_layout_cur->field_offset, field_info_cur->field_name);
          field_layout_cur++;
          field_info_cur++;
        }
#endif
        if (!local_stamp_layout[cur_stamp].container_layout) {
          if (class_bitmap) {
            // There are class fields to mark but not a container
            local_stamp_layout[cur_stamp].boehm._kind = class_kind;
          } else {
            // There are no class fields to mark and not a container - zero rank - mark nothing
            local_stamp_layout[cur_stamp].boehm._kind = atomic_kind;
          }
          local_stamp_layout[cur_stamp].boehm._kind_defined = true;
        } else {
          // Start from the client pointer
          local_stamp_layout[cur_stamp].boehm._class_bitmap = (local_stamp_layout[cur_stamp].class_field_pointer_bitmap);
          uintptr_t container_bitmap = local_stamp_layout[cur_stamp].container_layout->container_field_pointer_bitmap;
          local_stamp_layout[cur_stamp].boehm._container_bitmap = container_bitmap;
          int pointer_count = local_stamp_layout[cur_stamp].container_layout->container_field_pointer_count;
          if ( pointer_count*8 > GC_PROC_BYTES ) {
            printf("%s:%d WARNING There are too many pointers (%d) in each element of a container to break up the work for boehm\n", __FILE__, __LINE__, pointer_count );
          }
          local_stamp_layout[cur_stamp].boehm._container_pointer_count = pointer_count;
          // Calculate the number of elements worth of pointers are processed with each
          // call to the marking procedure
          int container_element_work = pointer_count ? (GC_PROC_BYTES/8/2)/pointer_count : 0;
          local_stamp_layout[cur_stamp].boehm._container_element_work = container_element_work;
          if ( class_bitmap && !container_bitmap) {
            // There are no pointers in the container part
            // - so we can use the bitmap_skip_header
            local_stamp_layout[cur_stamp].boehm._kind = class_kind;
          } else if (container_bitmap) {
            // The container part contains pointers - so we need to use a callback
            local_stamp_layout[cur_stamp].boehm._kind = container_kind;
          } else {
            // Container with no fixable pointers at all
            // SimpleCharacterString_O is one of these
            local_stamp_layout[cur_stamp].boehm._kind = atomic_kind;
          }
          local_stamp_layout[cur_stamp].boehm._kind_defined = true;
        }
      }
#ifdef DUMP_PRECISE_CALC
      if (local_stamp_layout[cur_stamp].boehm._kind_defined) {
        printf("%s:%d      boehm_kind = %lu address = %p\n", __FILE__, __LINE__, local_stamp_layout[cur_stamp].boehm._kind, &local_stamp_layout[cur_stamp].boehm._kind);
      }
#endif
      if (!local_stamp_layout[cur_stamp].boehm._kind_defined) {
        printf("%s:%d calculate boehm header cur_stamp = %d  layout_op %d  %s  UNDEFINED kind\n", __FILE__, __LINE__, cur_stamp, local_stamp_layout[cur_stamp].layout_op, local_stamp_info[cur_stamp].name );
      }
    }
  }
#ifdef DUMP_PRECISE_CALC
      printf("%s:%d    GC_I_NORMAL = %d\n", __FILE__, __LINE__, GC_I_NORMAL );
      printf("%s:%d   GC_I_PTRFREE = %d\n", __FILE__, __LINE__, GC_I_PTRFREE );
      printf("%s:%d      lisp_kind = %lu\n", __FILE__, __LINE__, lisp_kind );
      printf("%s:%d      cons_kind = %lu\n", __FILE__, __LINE__, cons_kind );
      printf("%s:%d    atomic_kind = %lu\n", __FILE__, __LINE__, atomic_kind );
      printf("%s:%d     class_kind = %lu\n", __FILE__, __LINE__, class_kind );
      printf("%s:%d container_kind = %lu\n", __FILE__, __LINE__, container_kind );
#endif
#endif // #if defined(USE_BOEHM) && defined(USE_PRECISE_GC)

  
  if (walk == lldb_info) {
    free(local_stamp_info);
    free(local_stamp_layout);
    free(local_field_layout);
    free(local_field_info);
    free(local_container_layout);
    free(local_container_info);
  } else if (walk == mps_info) {
    global_stamp_max = local_stamp_max;
    global_stamp_info = local_stamp_info;
    global_stamp_layout = local_stamp_layout;
    global_field_info = local_field_info;
    global_field_layout = local_field_layout;
    global_container_layout = local_container_layout;
    global_container_info = local_container_info;
  } else {
    printf("%s:%d Illegal walk %d\n", __FILE__, __LINE__, walk);
    abort();
  }
}


CL_DEFUN Fixnum gctools__size_of_stamp_field_layout_table()
{
  // First pass through the global_stamp_layout_codes_table
  // to count the number of stamps and the number of fields
  Layout_code* codes = get_stamp_layout_codes();
  int idx = 0;
  while (1) {
    if ( codes[idx].cmd == layout_end ) return idx;
    ++idx;
  }
}

};
