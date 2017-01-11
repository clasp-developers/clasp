#include <clasp/core/foundation.h>
#include <clasp/gctools/gctoolsPackage.fwd.h>
#include <clasp/core/str.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/core/lisp.h>
#include <clasp/core/package.h>
#include <clasp/core/numbers.h>

namespace gctools {



size_t           global_kind_max;
Kind_info*       global_kind_info;
Kind_layout*     global_kind_layout;
Field_info*      global_field_info;
Field_layout*    global_field_layout;
Container_layout* global_container_layout;
Container_info*  global_container_info;



/*! This will build the tables that MPS needs to fix pointers in simple classes
It uses global_class_layout_codes to malloc space for and
build the global_class_info_table and the global_field_layout_table.
*/
void build_kind_field_layout_tables()
{
  // First pass through the global_kind_layout_codes_table
  // to count the number of kinds and the number of fields
  Layout_code* codes = get_kind_layout_codes();
  int idx = 0;
  size_t number_of_fixable_fields = 0;
  size_t number_of_containers = 0;
  global_kind_max = 0;
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
      if ( global_kind_max < codes[idx].data0 ) {
        global_kind_max = codes[idx].data0;
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
    } else if ( codes[idx].cmd == variable_array0 ) {
      ++number_of_containers;
    }
    ++idx;
  }
  // Now malloc memory for the tables
  // now that we know the size of everything
  global_kind_info = (Kind_info*)malloc(sizeof(Kind_info)*(global_kind_max+1));
  global_kind_layout = (Kind_layout*)malloc(sizeof(Kind_layout)*(global_kind_max+1));
  global_field_layout = (Field_layout*)malloc(sizeof(Field_layout)*number_of_fixable_fields);
  Field_layout* cur_field_layout= global_field_layout;
  Field_layout* max_field_layout = (Field_layout*)((char*)global_field_layout + sizeof(Field_layout)*number_of_fixable_fields);
  global_field_info = (Field_info*)malloc(sizeof(Field_info)*(number_of_fixable_fields));
  Field_info* cur_field_info = global_field_info;
  Field_info* max_field_info = (Field_info*)((char*)global_field_info + sizeof(Field_info)*number_of_fixable_fields);
  global_container_layout = (Container_layout*)malloc(sizeof(Container_layout)*(number_of_containers+1));
  global_container_info = (Container_info*)malloc(sizeof(Container_info)*(number_of_containers+1));
  // Fill in the immediate kinds
  // Traverse the global_kind_layout_codes again and fill the tables
  codes = get_kind_layout_codes();
  size_t cur_container_layout_idx = 0;
  size_t cur_container_info_idx = 0;
  int cur_kind=0;
  idx = 0;
  for ( idx=0; idx<num_codes; ++idx ) {
//    printf("%s:%d idx = %d\n", __FILE__, __LINE__, idx);
    switch (codes[idx].cmd) {
    case class_kind: 
        cur_kind = codes[idx].data0;
//        printf("%s:%d  cur_kind = %d\n", __FILE__, __LINE__, cur_kind);
        global_kind_layout[cur_kind].layout_op = class_container_op;
        global_kind_layout[cur_kind].field_layout_start = NULL;
        global_kind_layout[cur_kind].container_layout = NULL;
        global_kind_layout[cur_kind].number_of_fields = 0;
        global_kind_layout[cur_kind].bits_per_bitunit = 0;
        global_kind_layout[cur_kind].size = codes[idx].data1;
        global_kind_info[cur_kind].name = codes[idx].description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        global_kind_info[cur_kind].container_info_ptr = NULL;
        break;
    case fixed_field:
        if ( !(codes[idx].data0 == SMART_PTR_OFFSET
               || codes[idx].data0 == TAGGED_POINTER_OFFSET
               || codes[idx].data0 == POINTER_OFFSET )) continue;
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
        if ( global_kind_layout[cur_kind].field_layout_start == NULL )
          global_kind_layout[cur_kind].field_layout_start = cur_field_layout;
        ++global_kind_layout[cur_kind].number_of_fields;
        cur_field_layout->field_offset = codes[idx].data2;
        ++cur_field_layout;
        GCTOOLS_ASSERT(cur_field_info<max_field_info);
        if ( global_kind_info[cur_kind].field_info_ptr == NULL )
          global_kind_info[cur_kind].field_info_ptr = cur_field_info;
        cur_field_info->field_name = codes[idx].description;
        ++cur_field_info;
        break;
    case container_kind:
        cur_kind = codes[idx].data0;
        global_kind_layout[cur_kind].layout_op = class_container_op;
        global_kind_layout[cur_kind].number_of_fields = 0;
        global_kind_layout[cur_kind].size = codes[idx].data1;
        global_kind_layout[cur_kind].bits_per_bitunit = 0;
        global_kind_layout[cur_kind].field_layout_start = NULL;
        global_kind_layout[cur_kind].container_layout = NULL;
        global_kind_info[cur_kind].name = codes[idx].description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        global_kind_info[cur_kind].container_info_ptr = NULL;
        break;
    case bitunit_container_kind:
        cur_kind = codes[idx].data0;
        global_kind_layout[cur_kind].layout_op = bitunit_container_op;
        global_kind_layout[cur_kind].number_of_fields = 0;
        global_kind_layout[cur_kind].size = codes[idx].data1;
        global_kind_layout[cur_kind].bits_per_bitunit = codes[idx].data2;
        global_kind_layout[cur_kind].field_layout_start = NULL;
        global_kind_layout[cur_kind].container_layout = NULL;
        global_kind_info[cur_kind].name = codes[idx].description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        global_kind_info[cur_kind].container_info_ptr = NULL;
        break;
    case variable_array0:
        global_kind_layout[cur_kind].container_layout = &global_container_layout[cur_container_layout_idx++];
        GCTOOLS_ASSERT(cur_container_layout_idx<=number_of_containers);
        global_kind_layout[cur_kind].container_layout->data_offset = codes[idx].data2;
        break;
    case variable_capacity:
        global_kind_layout[cur_kind].container_layout->field_layout_start = cur_field_layout;
        global_kind_layout[cur_kind].container_layout->element_size = codes[idx].data0;
        global_kind_layout[cur_kind].container_layout->number_of_fields = 0;
        global_kind_layout[cur_kind].container_layout->end_offset = codes[idx].data1;
        global_kind_layout[cur_kind].container_layout->capacity_offset = codes[idx].data2;
        break;
    case variable_field:
        if ( !(codes[idx].data0 == SMART_PTR_OFFSET
               || codes[idx].data0 == TAGGED_POINTER_OFFSET
               || codes[idx].data0 == POINTER_OFFSET )) continue;
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
        cur_field_layout->field_offset = codes[idx].data2;
        ++cur_field_layout;
        ++global_kind_layout[cur_kind].container_layout->number_of_fields;
        if ( global_kind_info[cur_kind].container_info_ptr == NULL ) 
          global_kind_info[cur_kind].container_info_ptr = &global_container_info[cur_container_info_idx++];
        GCTOOLS_ASSERT(cur_container_info_idx<=number_of_containers);
        global_kind_info[cur_kind].container_info_ptr->field_name = codes[idx].description;
        break;
    case templated_kind:
        cur_kind = codes[idx].data0;
        global_kind_layout[cur_kind].layout_op = templated_op;
        global_kind_layout[cur_kind].field_layout_start = NULL;
        global_kind_layout[cur_kind].container_layout = NULL;
        global_kind_layout[cur_kind].number_of_fields = 0;
        global_kind_layout[cur_kind].size = codes[idx].data1;
        global_kind_info[cur_kind].name = codes[idx].description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        global_kind_info[cur_kind].container_info_ptr = NULL;
        break;
    case templated_class_jump_table_index:
        break;
    case container_jump_table_index:
        break;
    default:
        printf("%s:%d Illegal Layout_code table command: %d\n", __FILE__, __LINE__, codes[idx].cmd);
        THROW_HARD_ERROR(BF("The Layout_code table contained an illegal command: %d\n") % codes[idx].cmd);
    }
  }
}


CL_DEFUN Fixnum gctools__size_of_kind_field_layout_table()
{
  // First pass through the global_kind_layout_codes_table
  // to count the number of kinds and the number of fields
  Layout_code* codes = get_kind_layout_codes();
  int idx = 0;
  while (1) {
    if ( codes[idx].cmd == layout_end ) return idx;
    ++idx;
  }
}

CL_DEFUN core::T_mv gctools__kind_field_layout_entry(size_t idx)
{
  core::SymbolToEnumConverter_sp conv = gctools::As<core::SymbolToEnumConverter_sp>(_sym_STARkind_field_layout_table_cmdsSTAR->symbolValue());
  // First pass through the global_kind_layout_codes_table
  // to count the number of kinds and the number of fields
  Layout_code* codes = get_kind_layout_codes();
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

