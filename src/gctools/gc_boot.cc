#include <clasp/core/foundation.h>
#include <clasp/gctools/gctoolsPackage.fwd.h>
#include <clasp/core/array.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/core/lisp.h>
#include <clasp/core/package.h>
#include <clasp/core/numbers.h>
#include <clasp/core/funcallableInstance.h>
#if defined(USE_BOEHM) && defined(USE_PRECISE_GC)
#include "src/bdwgc/include/gc.h"
#include "src/bdwgc/include/gc_mark.h"
#endif

// #define DUMP_GC_BOOT 1
// #define DUMP_PRECISE_CALC 1

#ifdef DUMP_GC_BOOT
#define DGC_PRINT(...) printf(__VA_ARGS__);
#else
#define DGC_PRINT(...)
#endif

#if defined(USE_BOEHM) && defined(USE_PRECISE_GC)
extern "C" {
void* obj_skip(void*);
};
#define GC_LISP_OBJECT_MARK
#include "obj_scan.cc"
#undef GC_LISP_OBJECT_MARK
#endif

namespace gctools {

uintptr_t global_lisp_kind;
uintptr_t global_cons_kind;
uintptr_t global_class_kind;
uintptr_t global_container_kind;
uintptr_t global_code_kind;
uintptr_t global_atomic_kind;
// int              global_container_proc_index;
size_t global_stamp_max;
Stamp_info* global_stamp_info;
Stamp_layout* global_stamp_layout;
Field_info* global_field_info;
Field_layout* global_field_layout;
Container_layout* global_container_layout;
Container_info* global_container_info;

void dump_data_types(std::ostream& fout, const std::string& indent) {
#define DTNAME(_type_, _name_, _sz_)                                                                                               \
  fmt::print(fout, "{}Init_data_type( data_type={}, name=\"{}\",sizeof={})\n", indent.c_str(), (int)_type_, _name_, _sz_)
  DTNAME(SMART_PTR_OFFSET, "smart_ptr", sizeof(void*));
  DTNAME(ATOMIC_SMART_PTR_OFFSET, "atomic_smart_ptr", sizeof(void*));
  DTNAME(TAGGED_POINTER_OFFSET, "tagged_ptr", sizeof(void*));
  DTNAME(RAW_POINTER_OFFSET, "void*", sizeof(void*));
  DTNAME(ARRAY_OFFSET, "array", sizeof(void*));
  DTNAME(POINTER_OFFSET, "pointer", sizeof(void*));
  DTNAME(CONSTANT_ARRAY_OFFSET, "constant_array", sizeof(void*));
  DTNAME(ctype_double, "double", sizeof(double));
  DTNAME(ctype_float, "float", sizeof(float));
  DTNAME(ctype_long_double, "long double", sizeof(long double));
  DTNAME(ctype_int, "int", sizeof(int));
  DTNAME(ctype_short, "short", sizeof(short));
  DTNAME(ctype_unsigned_char, "unsigned_char", sizeof(unsigned char));
  DTNAME(ctype_signed_char, "signed_char", sizeof(signed char));
  DTNAME(ctype_unsigned_short, "unsigned_short", sizeof(unsigned short));
  DTNAME(ctype_signed_short, "signed_short", sizeof(signed short));
  DTNAME(ctype_unsigned_long, "unsigned_long", sizeof(unsigned long));
  DTNAME(ctype_unsigned_int, "unsigned_int", sizeof(unsigned int));
  DTNAME(ctype_long, "long", sizeof(long));
  DTNAME(ctype_long_long, "long_long", sizeof(long long));
  DTNAME(ctype_char, "char", sizeof(char));
  DTNAME(ctype_char32_t, "char32_t", sizeof(char32_t));
  DTNAME(ctype__Bool, "_Bool", sizeof(bool));
  DTNAME(ctype_enum_core__StreamMode, "enum_core__StreamMode", sizeof(int));
  DTNAME(ctype_const_char_ptr, "const_char_ptr", sizeof(const char*));
  DTNAME(ctype_size_t, "size_t", sizeof(size_t));
  DTNAME(ctype_opaque_ptr, "opaque_ptr", sizeof(void*));

  DTNAME(CXX_FIXUP_OFFSET, "CXX_FIXUP_OFFSET", sizeof(unsigned long));
  DTNAME(ATOMIC_POD_OFFSET_unsigned_short, "ATOMIC_POD_OFFSET_unsigned_short", sizeof(unsigned short));
  DTNAME(ATOMIC_POD_OFFSET_unsigned_long, "ATOMIC_POD_OFFSET_unsigned_long", sizeof(unsigned long));
  DTNAME(ATOMIC_POD_OFFSET_mp__ProcessPhase, "ATOMIC_POD_OFFSET_mp__ProcessPhase", sizeof(mp::ProcessPhase));
  DTNAME(ATOMIC_POD_OFFSET_unsigned_int, "ATOMIC_POD_OFFSET_unsigned_int", sizeof(unsigned int));
  DTNAME(ATOMIC_POD_OFFSET_long_long, "ATOMIC_POD_OFFSET_long_long", sizeof(long long));
  DTNAME(ATOMIC_POD_OFFSET__Bool, "ATOMIC_POD_OFFSET__Bool", sizeof(bool));
  DTNAME(ATOMIC_POD_OFFSET_long, "ATOMIC_POD_OFFSET_long", sizeof(long));
  DTNAME(CXX_SHARED_MUTEX_OFFSET, "CXX_SHARED_MUTEX_OFFSET", sizeof(mp::Mutex));

#define Init_global_ints(_name_, _value_)                                                                                          \
  fmt::print(fout, "{}Init_global_ints(name=\"{}\",value={})\n", indent.c_str(), _name_, _value_);
#define Init_global_size_t(_name_, _value_)                                                                                        \
  fmt::print(fout, "{}Init_global_ints(name=\"{}\",value={})\n", indent.c_str(), _name_, _value_);
  Init_global_ints("TAG_BITS", TAG_BITS);
  Init_global_ints("IMMEDIATE_MASK", IMMEDIATE_MASK);
  Init_global_ints("FIXNUM_MASK", FIXNUM_MASK);
  Init_global_ints("GENERAL_TAG", GENERAL_TAG);
  Init_global_ints("CONS_TAG", CONS_TAG);
  Init_global_ints("SINGLE_FLOAT_TAG", SINGLE_FLOAT_TAG);
  Init_global_ints("CHARACTER_TAG", CHARACTER_TAG);
  Init_global_ints("VASLIST0_TAG", VASLIST0_TAG);
  Init_global_ints("FIXNUM_SHIFT", FIXNUM_SHIFT);
  Init_global_ints("GENERAL_MTAG_MASK", (size_t)(gctools::Header_s::general_mtag_mask));
  Init_global_ints("GENERAL_STAMP_SHIFT", (int)Header_s::general_stamp_shift);
  Init_global_ints("MTAG_SHIFT", (int)Header_s::mtag_shift);
  Init_global_ints("MTAG_MASK", (int)Header_s::mtag_mask);
  Init_global_ints("GENERAL_MTAG", (int)Header_s::general_mtag);
  Init_global_ints("CONS_MTAG", (int)Header_s::cons_mtag);
  Init_global_ints("REF_CLASS_CLASS_NAME", (int)core::Instance_O::REF_CLASS_CLASS_NAME);

  Init_global_size_t("VASLIST-ARGS-OFFSET", core::Vaslist::args_offset());
  Init_global_size_t("VASLIST-NARGS-OFFSET", core::Vaslist::nargs_offset());
  Init_global_size_t("VASLIST-NARGS-DECREMENT", core::Vaslist::NargsDecrement);
  Init_global_size_t("VASLIST-NARGS-MASK", core::Vaslist::NargsMask);
  Init_global_size_t("VASLIST-NARGS-SHIFT", core::Vaslist::NargsShift);
}

inline int bitmap_field_index(size_t start, size_t offset) {
  int bitindex = start - (offset / 8);
  if (bitindex > 63 || bitindex < 2) return -1;
  else return bitindex;
}

inline uintptr_t bitmap_field_bitmap(size_t bitindex) { return (uintptr_t)1 << bitindex; }

void walk_stamp_field_layout_tables(WalkKind walk, std::ostream& fout) {
  std::string indent = "";
  if (walk == lldb_info) {
    //    fprintf(fout,"import clasp\n");
    //    fprintf(fout,"from clasp import inspect\n");
    dump_data_types(fout, indent);
    //    core::registerOrDumpDtreeInfo(fout);
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
    DGC_PRINT("%s:%d Initial scan idx = %d\n", __FILE__, __LINE__, idx);
    if (codes[idx].cmd == layout_end)
      break;
    ++num_codes;
    if (codes[idx].cmd < 0 || codes[idx].cmd > layout_end) {
      printf("%s:%d the layout code table is damaged\n", __FILE__, __LINE__);
      abort();
    }
    if (codes[idx].cmd == class_kind || codes[idx].cmd == container_kind || codes[idx].cmd == bitunit_container_kind ||
        codes[idx].cmd == templated_kind) {
#define STAMP(_stamp_wtag_) (_stamp_wtag_ >> (Header_s::wtag_width))
      size_t stamp_code = STAMP(codes[idx].data0);
      DGC_PRINT("%s:%d    idx: %d codes[idx].data0: %lu  stamp_code: %lu\n", __FILE__, __LINE__, idx, codes[idx].data0, stamp_code);
      if (local_stamp_max < stamp_code) {
        local_stamp_max = stamp_code + 1;
        DGC_PRINT("%s:%d:%s local_stamp_max set to %lu\n", __FILE__, __LINE__, __FUNCTION__, local_stamp_max);
      }
    } else if ((codes[idx].cmd == fixed_field || codes[idx].cmd == variable_field) &&
               (codes[idx].data0 == SMART_PTR_OFFSET || codes[idx].data0 == ATOMIC_SMART_PTR_OFFSET ||
                codes[idx].data0 == TAGGED_POINTER_OFFSET || codes[idx].data0 == POINTER_OFFSET)) {
      ++number_of_fixable_fields;
    } else if ((codes[idx].cmd == fixed_field) && (codes[idx].data0 == CONSTANT_ARRAY_OFFSET)) {
      // Ignore the Array_O size_t _Length[0] array
      if (strcmp(codes[idx].description, "_Length") != 0) {
        //        printf("%s:%d There is an unknown CONSTANT_ARRAY named %s that the static analyzer identified - deal with it\n",
        //        __FILE__, __LINE__, codes[idx].description );
      }
    } else if (codes[idx].cmd == variable_array0 || codes[idx].cmd == variable_bit_array0) {
      ++number_of_containers;
    }
    ++idx;
  }
  DGC_PRINT("%s:%d Load scan ================\n", __FILE__, __LINE__);
  // Now allocate memory for the tables
  // now that we know the size of everything
  Stamp_info* local_stamp_info = (Stamp_info*)malloc(sizeof(Stamp_info) * (local_stamp_max + 1));
  DGC_PRINT("%s:%d:%s local_stamp_info = %p num: %lu\n", __FILE__, __LINE__, __FUNCTION__, local_stamp_info, local_stamp_max + 1);
  DGC_PRINT("%s:%d:%s &local_stamp_info[local_stamp_max+1] = %p\n", __FILE__, __LINE__, __FUNCTION__,
            &local_stamp_info[local_stamp_max + 1]);
  memset(local_stamp_info, 0, sizeof(Stamp_info) * (local_stamp_max + 1));
  Stamp_layout* local_stamp_layout =
      new Stamp_layout[local_stamp_max + 1]; // (Stamp_layout*)malloc(sizeof(Stamp_layout)*(local_stamp_max+1));
  Field_layout* local_field_layout =
      new Field_layout[number_of_fixable_fields]; // (Field_layout*)malloc(sizeof(Field_layout)*number_of_fixable_fields);
  Field_layout* cur_field_layout = local_field_layout;
  Field_layout* max_field_layout = (Field_layout*)((char*)local_field_layout + sizeof(Field_layout) * number_of_fixable_fields);
  Field_info* local_field_info =
      new Field_info[number_of_fixable_fields]; // (Field_info*)malloc(sizeof(Field_info)*(number_of_fixable_fields));
  Field_info* cur_field_info = local_field_info;
  Field_info* max_field_info = (Field_info*)((char*)local_field_info + sizeof(Field_info) * number_of_fixable_fields);
  Container_layout* local_container_layout =
      new Container_layout[number_of_containers +
                           1]; // (Container_layout*)malloc(sizeof(Container_layout)*(number_of_containers+1));
  Container_info* local_container_info =
      new Container_info[number_of_containers + 1]; // (Container_info*)malloc(sizeof(Container_info)*(number_of_containers+1));
  // Fill in the immediate stamps
  // Traverse the local_stamp_layout_codes again and fill the tables
  codes = get_stamp_layout_codes();
  size_t cur_container_layout_idx = 0;
  size_t cur_container_info_idx = 0;
  int cur_stamp = 0;
  idx = 0;
  size_t fixed_index = 0;
  size_t container_variable_index = 0;
  size_t prev_field_offset = ~0;
  for (idx = 0; idx < num_codes; ++idx) {
    DGC_PRINT("%s:%d Loading scan   idx = %d\n", __FILE__, __LINE__, idx);
    switch (codes[idx].cmd) {
    case class_kind:
      prev_field_offset = ~0;
      cur_stamp = STAMP(codes[idx].data0);
      if (cur_stamp > local_stamp_max) {
        printf("%s:%d  class_kind  cur_stamp = %d local_stamp_max = %lu\n", __FILE__, __LINE__, cur_stamp, local_stamp_max);
      }
      DGC_PRINT("%s:%d  idx: %d class_kind  cur_stamp = %d name = %s\n", __FILE__, __LINE__, idx, cur_stamp,
                codes[idx].description);
      local_stamp_layout[cur_stamp].layout_op = class_container_op;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_layout[cur_stamp].flags = codes[idx].data3;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      fixed_index = 0;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init_class_kind( stamp={}, name=\"{}\", size={} )\n", indent.c_str(), cur_stamp, codes[idx].description,
                   local_stamp_layout[cur_stamp].size);
      break;
    case fixed_field: {
      size_t data_type = codes[idx].data0;
      const char* field_name = codes[idx].description;
      size_t field_offset = codes[idx].data2;
      if (walk == lldb_info) {
        fmt::print(fout, "{}Init__fixed_field( stamp={}, index={}, data_type={}, field_name=\"{}\", field_offset={})\n",
                   indent.c_str(), cur_stamp,
                   fixed_index++, // index,
                   data_type, field_name, field_offset);
      }
      //
      // Certain types can't be saved/loaded so we count them here
      //
      if (data_type == CXX_SHARED_MUTEX_OFFSET || data_type == CXX_FIXUP_OFFSET || data_type == ctype_opaque_ptr) {
        local_stamp_layout[cur_stamp].snapshot_save_load_poison++;
      }
      if ((data_type == SMART_PTR_OFFSET || data_type == ATOMIC_SMART_PTR_OFFSET
           || data_type == TAGGED_POINTER_OFFSET
           || data_type == POINTER_OFFSET
           || data_type == WEAK_PTR_OFFSET)) {
        GCTOOLS_ASSERT(cur_field_layout < max_field_layout);
#ifdef USE_PRECISE_GC
        int bit_index;
        uintptr_t field_bitmap;
        bit_index = bitmap_field_index(63, field_offset);
        if (data_type == WEAK_PTR_OFFSET || bit_index == -1) {
          // We have a field we need to fix that is beyond the range of a bitmap.
          // Flag this class to tell the scanner to use the field layouts instead.
          // Alternately we have a weak reference that needs to be
          // scanned specially.
          local_stamp_layout[cur_stamp].flags |= COMPLEX_SCAN;
        } else {
          // Otherwise (normal case) just put it in the bitmap.
          field_bitmap = bitmap_field_bitmap(bit_index);
          local_stamp_layout[cur_stamp].class_field_pointer_bitmap |= field_bitmap;
        }
        
        if (local_stamp_layout[cur_stamp].field_layout_start == NULL)
          local_stamp_layout[cur_stamp].field_layout_start = cur_field_layout;
        DGC_PRINT("%s:%d   fixed_field %s : cur_stamp = %d  field_offset = %lu bit_index =%d bitmap = 0x%lX\n", __FILE__, __LINE__,
                  field_name, cur_stamp, field_offset, bit_index, field_bitmap);
        DGC_PRINT("       class_field_pointer_bitmap = 0x%lX\n", local_stamp_layout[cur_stamp].class_field_pointer_bitmap);
#endif
#ifdef DEBUG_ASSERT
        if (field_offset == prev_field_offset) {
          printf("%s:%d:%s The same field_offset %lu has been declared twice while parsing the static analyzer generated tables.\n",
                 __FILE__, __LINE__, __FUNCTION__, field_offset);
          printf("cur_stamp  = %d\n", cur_stamp);
          printf("Class name = \"%s\"\n", local_stamp_info[cur_stamp].name);
          printf("field_name = \"%s\"\n", field_name);
          printf("     This indicates a problem with those generated tables - check the static analyzer output.  ABORTING...\n");
          abort();
        }
#endif
        cur_field_layout->field_offset = field_offset;
        prev_field_offset = field_offset;
        GCTOOLS_ASSERT(cur_field_info < max_field_info);
        if (local_stamp_info[cur_stamp].field_info_ptr == NULL)
          local_stamp_info[cur_stamp].field_info_ptr = cur_field_info;
        cur_field_info->field_name = field_name;
        cur_field_info->data_type = data_type;
        ++local_stamp_layout[cur_stamp].number_of_fields;
        ++cur_field_layout;
        ++cur_field_info;
      }
    } break;
    case container_kind:
      prev_field_offset = ~0;
      cur_stamp = STAMP(codes[idx].data0);
      DGC_PRINT("%s:%d   container_kind  cur_stamp = %d name = %s\n", __FILE__, __LINE__, cur_stamp, codes[idx].description);
      local_stamp_layout[cur_stamp].layout_op = class_container_op;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_layout[cur_stamp].flags = codes[idx].data3;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      fixed_index = 0;
      container_variable_index = 0;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init_container_kind( stamp={}, name=\"{}\", size={} )\n", indent.c_str(), cur_stamp,
                   local_stamp_info[cur_stamp].name, local_stamp_layout[cur_stamp].size);
      break;
    case bitunit_container_kind:
      prev_field_offset = ~0;
      cur_stamp = STAMP(codes[idx].data0);
      DGC_PRINT("%s:%d   bitunit_container_kind  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
      local_stamp_layout[cur_stamp].layout_op = bitunit_container_op;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_layout[cur_stamp].flags = codes[idx].data3;
      local_stamp_layout[cur_stamp].bits_per_bitunit = codes[idx].data2;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      fixed_index = 0;
      container_variable_index = 0;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init_bitunit_container_kind( stamp={}, name=\"{}\", size={}, bits_per_bitunit={} )\n", indent.c_str(),
                   cur_stamp, local_stamp_info[cur_stamp].name, local_stamp_layout[cur_stamp].size,
                   local_stamp_layout[cur_stamp].bits_per_bitunit);
      break;
    case variable_array0:
      DGC_PRINT("%s:%d   variable_array0 cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
      local_stamp_layout[cur_stamp].container_layout = &local_container_layout[cur_container_layout_idx++];
      GCTOOLS_ASSERT(cur_container_layout_idx <= number_of_containers);
      local_stamp_layout[cur_stamp].data_offset = codes[idx].data2;
      container_variable_index = 0;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init__variable_array0( stamp={}, name=\"{}\", offset={} )\n", indent.c_str(), cur_stamp,
                   codes[idx].description, local_stamp_layout[cur_stamp].data_offset);
      break;
    case variable_bit_array0:
      DGC_PRINT("%s:%d   variable_bit_array0 cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
      local_stamp_layout[cur_stamp].container_layout = &local_container_layout[cur_container_layout_idx++];
      GCTOOLS_ASSERT(cur_container_layout_idx <= number_of_containers);
      local_stamp_layout[cur_stamp].data_offset = codes[idx].data2;
      local_stamp_layout[cur_stamp].bits_per_bitunit = codes[idx].data0;
      break;
    case variable_capacity:
      DGC_PRINT("%s:%d   variable_capacity cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
      local_stamp_layout[cur_stamp].container_layout->field_layout_start = cur_field_layout;
      local_stamp_layout[cur_stamp].element_size = codes[idx].data0;
      local_stamp_layout[cur_stamp].container_layout->number_of_fields = 0;
      local_stamp_layout[cur_stamp].end_offset = codes[idx].data1;
      local_stamp_layout[cur_stamp].capacity_offset = codes[idx].data2;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init__variable_capacity( stamp={}, element_size={}, end_offset={}, capacity_offset={} )\n",
                   indent.c_str(), cur_stamp, local_stamp_layout[cur_stamp].element_size, local_stamp_layout[cur_stamp].end_offset,
                   local_stamp_layout[cur_stamp].capacity_offset);
      break;
    case variable_field: {
      size_t data_type = codes[idx].data0;
      size_t field_offset = codes[idx].data2;
      const char* field_name = codes[idx].description;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init__variable_field( stamp={}, index={}, data_type={}, field_name=\"{}\", field_offset={} )\n",
                   indent.c_str(), cur_stamp,
                   container_variable_index++, // index,
                   data_type, field_name, field_offset);
      //
      // Certain types can't be saved/loaded so we count them here
      //
      if (data_type == CXX_SHARED_MUTEX_OFFSET || data_type == CXX_FIXUP_OFFSET || data_type == ctype_opaque_ptr) {
        local_stamp_layout[cur_stamp].snapshot_save_load_poison++;
      }
      if (((data_type) == SMART_PTR_OFFSET || (data_type) == ATOMIC_SMART_PTR_OFFSET || (data_type) == TAGGED_POINTER_OFFSET ||
           (data_type) == POINTER_OFFSET)) {
        int bit_index = bitmap_field_index(63, field_offset);
        uintptr_t field_bitmap = bitmap_field_bitmap(bit_index);
        GCTOOLS_ASSERT(cur_field_layout < max_field_layout);
        local_stamp_layout[cur_stamp].container_layout->container_field_pointer_bitmap |= field_bitmap;
        local_stamp_layout[cur_stamp].container_layout->container_field_pointer_count++;
        DGC_PRINT("%s:%d   variable_field  %s cur_stamp = %d field_offset = %lu field_bit_index = %d field_bitmap = 0x%lX\n",
                  __FILE__, __LINE__, field_name, cur_stamp, field_offset, bit_index, field_bitmap);
        DGC_PRINT("        .container_layout->container_field_pointer_bitmap = 0x%lX\n",
                  local_stamp_layout[cur_stamp].container_layout->container_field_pointer_bitmap);
        cur_field_layout->field_offset = field_offset;
        if (local_stamp_info[cur_stamp].container_info_ptr == NULL)
          local_stamp_info[cur_stamp].container_info_ptr = &local_container_info[cur_container_info_idx++];
        GCTOOLS_ASSERT(cur_container_info_idx <= number_of_containers);
        local_stamp_info[cur_stamp].container_info_ptr->field_name = field_name;
        local_stamp_info[cur_stamp].container_info_ptr->data_type = data_type;
        ++cur_field_layout;
        ++local_stamp_layout[cur_stamp].container_layout->number_of_fields;
      }
    } break;
    case templated_kind: {
      prev_field_offset = ~0;
      cur_stamp = STAMP(codes[idx].data0);
      size_t size = codes[idx].data1;
      const char* name = codes[idx].description;
      if (walk == lldb_info)
        fmt::print(fout, "{}Init_templated_kind( stamp={}, name=\"{}\", size={} )\n", indent.c_str(), cur_stamp, name, size);
      DGC_PRINT("%s:%d   templated_kind cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
      local_stamp_layout[cur_stamp].layout_op = templated_op;
      local_stamp_layout[cur_stamp].field_layout_start = NULL;
      local_stamp_layout[cur_stamp].container_layout = NULL;
      local_stamp_layout[cur_stamp].number_of_fields = 0;
      local_stamp_layout[cur_stamp].size = codes[idx].data1;
      local_stamp_layout[cur_stamp].flags = codes[idx].data3;
      local_stamp_info[cur_stamp].name = codes[idx].description;
      local_stamp_info[cur_stamp].field_info_ptr = NULL;
      local_stamp_info[cur_stamp].container_info_ptr = NULL;
      fixed_index = 0;
      container_variable_index = 0;
    } break;
    case templated_class_jump_table_index:
      DGC_PRINT("%s:%d   templated_class_jump_table_index\n", __FILE__, __LINE__);
      break;
    case container_jump_table_index:
      DGC_PRINT("%s:%d   container_jump_table_index\n", __FILE__, __LINE__);
      break;
    default:
      printf("%s:%d Illegal Layout_code table command: %d\n", __FILE__, __LINE__, codes[idx].cmd);
      throw_hard_error_bad_layout_command(codes[idx].cmd);
    }
  }

  // Calculate boehm header
#if defined(USE_BOEHM) && defined(USE_PRECISE_GC)
  if (walk == precise_info) {
    // Setup the CONS cell bitmap
    uintptr_t cons_stamp = STAMP_UNSHIFT_WTAG(STAMPWTAG_core__Cons_O); // wasMTAG
    // The cons_bitmap doesn't take into account the header!!!!! You need to do this if you are going to use bitmaps
    uintptr_t cons_bitmap =
        ((uintptr_t)1 << (63 - (offsetof(core::Cons_O, _Car) / 8))) | ((uintptr_t)1 << (63 - (offsetof(core::Cons_O, _Cdr) / 8)));
    if (cons_bitmap != local_stamp_layout[cons_stamp].class_field_pointer_bitmap) {
      printf("%s:%d The cons stamp %lu cons_bitmap = 0x%lX  and it doesn't match   "
             "local_stamp_layout[cons_stamp].class_field_pointer_bitmap = 0x%lX\n",
             __FILE__, __LINE__, cons_stamp, cons_bitmap, local_stamp_layout[cons_stamp].class_field_pointer_bitmap);
      abort();
    }

    // Use boehm in the precise GC mode
    //  global_container_proc_index = GC_new_proc_inner((GC_mark_proc)class_container_mark);
    global_lisp_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
    global_cons_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
    global_class_kind =
        GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1,
                    1); //  GC_MAKE_PROC(GC_new_proc((GC_mark_proc)Lisp_object_mark),0), 0, 1); // GC_DS_LENGTH, 1, 1);
    global_container_kind = GC_new_kind(
        GC_new_free_list(), GC_DS_LENGTH, 1,
        1); // */  GC_new_kind(GC_new_free_list(), GC_MAKE_PROC(global_container_proc_index,0),0,1); // GC_DS_LENGTH, 1, 1);
    global_code_kind = GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 1, 1);
    global_atomic_kind = GC_I_PTRFREE; // GC_new_kind(GC_new_free_list(), GC_DS_LENGTH, 0, 1);
    for (cur_stamp = 0; cur_stamp <= local_stamp_max; ++cur_stamp) {
      if (local_stamp_layout[cur_stamp].layout_op != undefined_op) {
#ifdef DUMP_PRECISE_CALC
        printf("%s:%d --------------------------------------\n", __FILE__, __LINE__);
        printf("%s:%d calculate boehm header cur_stamp = %d  layout_op %d  %s  \n", __FILE__, __LINE__, cur_stamp,
               local_stamp_layout[cur_stamp].layout_op, local_stamp_info[cur_stamp].name);
#endif
        if (cur_stamp == STAMP_UNSHIFT_WTAG(STAMPWTAG_core__Lisp)) { // wasMTAG
          local_stamp_layout[cur_stamp].boehm._kind = global_lisp_kind;
          local_stamp_layout[cur_stamp].boehm._kind_defined = true;
        } else if (cur_stamp == STAMP_UNSHIFT_WTAG(STAMPWTAG_llvmo__CodeBlock_O)) { // wasMTAG
          local_stamp_layout[cur_stamp].boehm._kind = global_code_kind;
          local_stamp_layout[cur_stamp].boehm._kind_defined = true;
        } else if (cur_stamp == STAMP_UNSHIFT_WTAG(STAMPWTAG_core__Cons_O)) { // wasMTAG
          local_stamp_layout[cur_stamp].boehm._kind = global_cons_kind;
#ifdef DUMP_PRECISE_CALC
          printf("%s:%d   cons_bitmap = 0x%lx global_cons_kind = %lu  address = %p\n", __FILE__, __LINE__, cons_bitmap,
                 global_cons_kind, &global_cons_kind);
#endif
          local_stamp_layout[cur_stamp].boehm._kind_defined = true;
        } else {
          uintptr_t class_bitmap = (local_stamp_layout[cur_stamp].class_field_pointer_bitmap);
          local_stamp_layout[cur_stamp].boehm._class_bitmap = class_bitmap;
#ifdef DUMP_PRECISE_CALC
          printf("%s:%d stamp = %d  class_bitmap = 0x%lX\n", __FILE__, __LINE__, cur_stamp, class_bitmap);
          const gctools::Stamp_layout& stamp_layout = local_stamp_layout[cur_stamp];
          const gctools::Stamp_info& stamp_info = local_stamp_info[cur_stamp];
          const gctools::Field_info* field_info_cur = stamp_info.field_info_ptr;
          const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
          int num_fields = stamp_layout.number_of_fields;
          for (int i = 0; i < num_fields; ++i) {
            printf("%s:%d [%d]   offset %zu   %s\n", __FILE__, __LINE__, i, field_layout_cur->field_offset,
                   field_info_cur->field_name);
            field_layout_cur++;
            field_info_cur++;
          }
#endif
          if (!local_stamp_layout[cur_stamp].container_layout) {
            if (class_bitmap) {
              // There are class fields to mark but not a container
              local_stamp_layout[cur_stamp].boehm._kind = global_class_kind;
            } else {
              // There are no class fields to mark and not a container - zero rank - mark nothing
              local_stamp_layout[cur_stamp].boehm._kind = global_atomic_kind;
            }
            local_stamp_layout[cur_stamp].boehm._kind_defined = true;
          } else {
            // Start from the client pointer
            local_stamp_layout[cur_stamp].boehm._class_bitmap = (local_stamp_layout[cur_stamp].class_field_pointer_bitmap);
            uintptr_t container_bitmap = local_stamp_layout[cur_stamp].container_layout->container_field_pointer_bitmap;
            local_stamp_layout[cur_stamp].boehm._container_bitmap = container_bitmap;
            int pointer_count = local_stamp_layout[cur_stamp].container_layout->container_field_pointer_count;
            if (pointer_count * 8 > GC_PROC_BYTES) {
              printf(
                  "%s:%d WARNING There are too many pointers (%d) in each element of a container to break up the work for boehm\n",
                  __FILE__, __LINE__, pointer_count);
            }
            local_stamp_layout[cur_stamp].boehm._container_pointer_count = pointer_count;
            // Calculate the number of elements worth of pointers are processed with each
            // call to the marking procedure
            int container_element_work = pointer_count ? (GC_PROC_BYTES / 8 / 2) / pointer_count : 0;
            local_stamp_layout[cur_stamp].boehm._container_element_work = container_element_work;
            if (class_bitmap && !container_bitmap) {
              // There are no pointers in the container part
              // - so we can use the bitmap_skip_header
              local_stamp_layout[cur_stamp].boehm._kind = global_class_kind;
            } else if (container_bitmap) {
              // The container part contains pointers - so we need to use a callback
              local_stamp_layout[cur_stamp].boehm._kind = global_container_kind;
            } else {
              // Container with no fixable pointers at all
              // SimpleCharacterString_O is one of these
              local_stamp_layout[cur_stamp].boehm._kind = global_atomic_kind;
            }
            local_stamp_layout[cur_stamp].boehm._kind_defined = true;
          }
        }
#ifdef DUMP_PRECISE_CALC
        if (local_stamp_layout[cur_stamp].boehm._kind_defined) {
          printf("%s:%d      boehm_kind = %lu address = %p\n", __FILE__, __LINE__, local_stamp_layout[cur_stamp].boehm._kind,
                 &local_stamp_layout[cur_stamp].boehm._kind);
        }
#endif
        if (!local_stamp_layout[cur_stamp].boehm._kind_defined) {
          printf("%s:%d calculate boehm header cur_stamp = %d  layout_op %d  %s  UNDEFINED kind\n", __FILE__, __LINE__, cur_stamp,
                 local_stamp_layout[cur_stamp].layout_op, local_stamp_info[cur_stamp].name);
        }
      }
    }
#ifdef DUMP_PRECISE_CALC
    printf("%s:%d    GC_I_NORMAL = %d\n", __FILE__, __LINE__, GC_I_NORMAL);
    printf("%s:%d   GC_I_PTRFREE = %d\n", __FILE__, __LINE__, GC_I_PTRFREE);
    printf("%s:%d      lisp_kind = %lu\n", __FILE__, __LINE__, global_lisp_kind);
    printf("%s:%d      cons_kind = %lu\n", __FILE__, __LINE__, global_cons_kind);
    printf("%s:%d    atomic_kind = %lu\n", __FILE__, __LINE__, global_atomic_kind);
    printf("%s:%d     class_kind = %lu\n", __FILE__, __LINE__, global_class_kind);
    printf("%s:%d container_kind = %lu\n", __FILE__, __LINE__, global_container_kind);
    printf("%s:%d     code_kind = %lu\n", __FILE__, __LINE__, global_code_kind);
#endif
  }
#endif // #if defined(USE_BOEHM) && defined(USE_PRECISE_GC)

  if (walk == lldb_info) {
    DGC_PRINT("%s:%d:%s local_stamp_info = %p\n", __FILE__, __LINE__, __FUNCTION__, local_stamp_info);
    free(local_stamp_info);
    free(local_stamp_layout);
    free(local_field_layout);
    free(local_field_info);
    free(local_container_layout);
    free(local_container_info);
  } else if (walk == precise_info) {
    // Check that everything is ok
    if (getenv("CLASP_DEBUG_STAMP_INFO")) {
      for (size_t stamp = 0; stamp <= local_stamp_max; stamp++) {
        printf("%s:%d:%s stamp: %3lu  boehm._kind_defined %2d  boehm._kind %5lu  name: %s\n", __FILE__, __LINE__, __FUNCTION__,
               stamp, local_stamp_layout[stamp].boehm._kind_defined, local_stamp_layout[stamp].boehm._kind,
               local_stamp_info[stamp].name);
      }
      printf("%s:%d:%s local_stamp_max: %lu\n", __FILE__, __LINE__, __FUNCTION__, local_stamp_max);
    }
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

DOCGROUP(clasp);
CL_DEFUN Fixnum gctools__size_of_stamp_field_layout_table() {
  // First pass through the global_stamp_layout_codes_table
  // to count the number of stamps and the number of fields
  Layout_code* codes = get_stamp_layout_codes();
  int idx = 0;
  while (1) {
    if (codes[idx].cmd == layout_end)
      return idx;
    ++idx;
  }
}

}; // namespace gctools
