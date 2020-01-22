#include <clasp/core/foundation.h>
#include <clasp/gctools/gctoolsPackage.fwd.h>
#include <clasp/core/array.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/core/lisp.h>
#include <clasp/core/package.h>
#include <clasp/core/numbers.h>

namespace gctools {



size_t           global_stamp_max;
Stamp_info*       global_stamp_info;
Stamp_layout*     global_stamp_layout;
Field_info*      global_field_info;
Field_layout*    global_field_layout;
Container_layout* global_container_layout;
Container_info*  global_container_info;



/*! This will build the tables that MPS needs to fix pointers in simple classes
It uses global_class_layout_codes to malloc space for and
build the global_class_info_table and the global_field_layout_table.
*/
void build_stamp_field_layout_tables()
{
  // First pass through the global_stamp_layout_codes_table
  // to count the number of stamps and the number of fields
  Layout_code* codes = get_stamp_layout_codes();
  int idx = 0;
  size_t number_of_fixable_fields = 0;
  size_t number_of_containers = 0;
  global_stamp_max = 0;
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
      if ( global_stamp_max < codes[idx].data0 ) {
        global_stamp_max = codes[idx].data0;
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
  // Now malloc memory for the tables
  // now that we know the size of everything
  global_stamp_info = (Stamp_info*)malloc(sizeof(Stamp_info)*(global_stamp_max+1));
  global_stamp_layout = (Stamp_layout*)malloc(sizeof(Stamp_layout)*(global_stamp_max+1));
  global_field_layout = (Field_layout*)malloc(sizeof(Field_layout)*number_of_fixable_fields);
  Field_layout* cur_field_layout= global_field_layout;
  Field_layout* max_field_layout = (Field_layout*)((char*)global_field_layout + sizeof(Field_layout)*number_of_fixable_fields);
  global_field_info = (Field_info*)malloc(sizeof(Field_info)*(number_of_fixable_fields));
  Field_info* cur_field_info = global_field_info;
  Field_info* max_field_info = (Field_info*)((char*)global_field_info + sizeof(Field_info)*number_of_fixable_fields);
  global_container_layout = (Container_layout*)malloc(sizeof(Container_layout)*(number_of_containers+1));
  global_container_info = (Container_info*)malloc(sizeof(Container_info)*(number_of_containers+1));
  // Fill in the immediate stamps
  // Traverse the global_stamp_layout_codes again and fill the tables
  codes = get_stamp_layout_codes();
  size_t cur_container_layout_idx = 0;
  size_t cur_container_info_idx = 0;
  int cur_stamp=0;
  idx = 0;
#define DUMP_GC_BOOT 1
#define STAMP(_stamp_wtag_mtag_) (_stamp_wtag_mtag_>>(Header_s::wtag_shift))
  for ( idx=0; idx<num_codes; ++idx ) {
    printf("%s:%d idx = %d\n", __FILE__, __LINE__, idx);
    switch (codes[idx].cmd) {
    case class_kind:
        cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
        printf("%s:%d  class_kind cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].layout_op = class_container_op;
        global_stamp_layout[cur_stamp].field_layout_start = NULL;
        global_stamp_layout[cur_stamp].container_layout = NULL;
        global_stamp_layout[cur_stamp].number_of_fields = 0;
        global_stamp_layout[cur_stamp].size = codes[idx].data1;
        global_stamp_info[cur_stamp].name = codes[idx].description;
        global_stamp_info[cur_stamp].field_info_ptr = NULL;
        global_stamp_info[cur_stamp].container_info_ptr = NULL;
        break;
    case fixed_field:
        if ( !((codes[idx].data0) == SMART_PTR_OFFSET
               || (codes[idx].data0) == TAGGED_POINTER_OFFSET
               || (codes[idx].data0) == POINTER_OFFSET )) continue;
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
#ifdef DUMP_GC_BOOT
        printf("%s:%d   fixed_field  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        if ( global_stamp_layout[cur_stamp].field_layout_start == NULL )
          global_stamp_layout[cur_stamp].field_layout_start = cur_field_layout;
        ++global_stamp_layout[cur_stamp].number_of_fields;
        cur_field_layout->field_offset = codes[idx].data2;
        ++cur_field_layout;
        GCTOOLS_ASSERT(cur_field_info<max_field_info);
        if ( global_stamp_info[cur_stamp].field_info_ptr == NULL )
          global_stamp_info[cur_stamp].field_info_ptr = cur_field_info;
        cur_field_info->field_name = codes[idx].description;
        ++cur_field_info;
        break;
    case container_kind:
        cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
        printf("%s:%d   container_kind  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].layout_op = class_container_op;
        global_stamp_layout[cur_stamp].number_of_fields = 0;
        global_stamp_layout[cur_stamp].size = codes[idx].data1;
        global_stamp_layout[cur_stamp].field_layout_start = NULL;
        global_stamp_layout[cur_stamp].container_layout = NULL;
        global_stamp_info[cur_stamp].name = codes[idx].description;
        global_stamp_info[cur_stamp].field_info_ptr = NULL;
        global_stamp_info[cur_stamp].container_info_ptr = NULL;
        break;
    case bitunit_container_kind:
        cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
        printf("%s:%d   bitunit_container_kind  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].layout_op = bitunit_container_op;
        global_stamp_layout[cur_stamp].number_of_fields = 0;
        global_stamp_layout[cur_stamp].size = codes[idx].data1;
        global_stamp_layout[cur_stamp].bits_per_bitunit = codes[idx].data2;
        global_stamp_layout[cur_stamp].field_layout_start = NULL;
        global_stamp_layout[cur_stamp].container_layout = NULL;
        global_stamp_info[cur_stamp].name = codes[idx].description;
        global_stamp_info[cur_stamp].field_info_ptr = NULL;
        global_stamp_info[cur_stamp].container_info_ptr = NULL;
        break;
    case variable_array0:
#ifdef DUMP_GC_BOOT
        printf("%s:%d   variable_array0 cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].container_layout = &global_container_layout[cur_container_layout_idx++];
        GCTOOLS_ASSERT(cur_container_layout_idx<=number_of_containers);
        global_stamp_layout[cur_stamp].data_offset = codes[idx].data2;
        break;
    case variable_bit_array0:
#ifdef DUMP_GC_BOOT
        printf("%s:%d   variable_bit_array0 cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].container_layout = &global_container_layout[cur_container_layout_idx++];
        GCTOOLS_ASSERT(cur_container_layout_idx<=number_of_containers);
        global_stamp_layout[cur_stamp].data_offset = codes[idx].data2;
        global_stamp_layout[cur_stamp].bits_per_bitunit = codes[idx].data0;
        break;
    case variable_capacity:
#ifdef DUMP_GC_BOOT
        printf("%s:%d   variable_capacity cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].container_layout->field_layout_start = cur_field_layout;
        global_stamp_layout[cur_stamp].element_size = codes[idx].data0;
        global_stamp_layout[cur_stamp].container_layout->number_of_fields = 0;
        global_stamp_layout[cur_stamp].end_offset = codes[idx].data1;
        global_stamp_layout[cur_stamp].capacity_offset = codes[idx].data2;
        break;
    case variable_field:
        if ( !((codes[idx].data0) == SMART_PTR_OFFSET
               || (codes[idx].data0) == TAGGED_POINTER_OFFSET
               || (codes[idx].data0) == POINTER_OFFSET )) continue;
#ifdef DUMP_GC_BOOT
        printf("%s:%d   variable_field cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
        cur_field_layout->field_offset = codes[idx].data2;
        ++cur_field_layout;
        ++global_stamp_layout[cur_stamp].container_layout->number_of_fields;
        if ( global_stamp_info[cur_stamp].container_info_ptr == NULL )
          global_stamp_info[cur_stamp].container_info_ptr = &global_container_info[cur_container_info_idx++];
        GCTOOLS_ASSERT(cur_container_info_idx<=number_of_containers);
        global_stamp_info[cur_stamp].container_info_ptr->field_name = codes[idx].description;
        break;
    case templated_kind:
        cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
        printf("%s:%d   templated_kind cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        global_stamp_layout[cur_stamp].layout_op = templated_op;
        global_stamp_layout[cur_stamp].field_layout_start = NULL;
        global_stamp_layout[cur_stamp].container_layout = NULL;
        global_stamp_layout[cur_stamp].number_of_fields = 0;
        global_stamp_layout[cur_stamp].size = codes[idx].data1;
        global_stamp_info[cur_stamp].name = codes[idx].description;
        global_stamp_info[cur_stamp].field_info_ptr = NULL;
        global_stamp_info[cur_stamp].container_info_ptr = NULL;
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

CL_DEFUN core::T_mv gctools__stamp_field_layout_entry(size_t idx)
{
  core::SymbolToEnumConverter_sp conv = gctools::As<core::SymbolToEnumConverter_sp>(_sym_STARstamp_field_layout_table_cmdsSTAR->symbolValue());
  // First pass through the global_stamp_layout_codes_table
  // to count the number of stamps and the number of fields
  Layout_code* codes = get_stamp_layout_codes();
  Layout_code& code = codes[idx];
  core::Symbol_sp cmd = conv->symbolForEnumIndex(code.cmd);
  core::Fixnum_sp data0 = core::clasp_make_fixnum(code.data0);
  core::Fixnum_sp data1 = core::clasp_make_fixnum(code.data1);
  core::Fixnum_sp data2 = core::clasp_make_fixnum(code.data2);
  core::Symbol_sp description = _Nil<core::T_O>();
  if ( code.description ) {
    core::SimpleBaseString_sp desc = core::SimpleBaseString_O::make(code.description);
    core::Package_sp pkg = gctools::As<core::Package_sp>(core::_sym_STARclasp_packageSTAR->symbolValue());
    description = pkg->intern(desc);
  }
  return Values(cmd,data0,data1,data2,description);
}


};
