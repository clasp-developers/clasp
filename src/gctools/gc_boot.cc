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
      if ( local_stamp_max < codes[idx].data0 ) {
        local_stamp_max = codes[idx].data0;
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
  Stamp_info* local_stamp_info = (Stamp_info*)malloc(sizeof(Stamp_info)*(local_stamp_max+1));
  memset(local_stamp_info,0,sizeof(Stamp_info)*(local_stamp_max+1));
  Stamp_layout* local_stamp_layout = (Stamp_layout*)malloc(sizeof(Stamp_layout)*(local_stamp_max+1));
  Field_layout* local_field_layout = (Field_layout*)malloc(sizeof(Field_layout)*number_of_fixable_fields);
  Field_layout* cur_field_layout= local_field_layout;
  Field_layout* max_field_layout = (Field_layout*)((char*)local_field_layout + sizeof(Field_layout)*number_of_fixable_fields);
  Field_info* local_field_info = (Field_info*)malloc(sizeof(Field_info)*(number_of_fixable_fields));
  Field_info* cur_field_info = local_field_info;
  Field_info* max_field_info = (Field_info*)((char*)local_field_info + sizeof(Field_info)*number_of_fixable_fields);
  Container_layout* local_container_layout = (Container_layout*)malloc(sizeof(Container_layout)*(number_of_containers+1));
  Container_info* local_container_info = (Container_info*)malloc(sizeof(Container_info)*(number_of_containers+1));
  // Fill in the immediate stamps
  // Traverse the local_stamp_layout_codes again and fill the tables
  codes = get_stamp_layout_codes();
  size_t cur_container_layout_idx = 0;
  size_t cur_container_info_idx = 0;
  int cur_stamp=0;
  idx = 0;
//#define DUMP_GC_BOOT 1
#define STAMP(_stamp_wtag_) (_stamp_wtag_>>(Header_s::wtag_width))
  for ( idx=0; idx<num_codes; ++idx ) {
#ifdef DUMP_GC_BOOT
    printf("%s:%d idx = %d\n", __FILE__, __LINE__, idx);
#endif
    switch (codes[idx].cmd) {
    case class_kind:
        cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
        printf("%s:%d  class_kind cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
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
#ifdef DUMP_GC_BOOT
        printf("%s:%d   fixed_field  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        if ( local_stamp_layout[cur_stamp].field_layout_start == NULL )
          local_stamp_layout[cur_stamp].field_layout_start = cur_field_layout;
        cur_field_layout->field_offset = field_offset;
        GCTOOLS_ASSERT(cur_field_info<max_field_info);
        if ( local_stamp_info[cur_stamp].field_info_ptr == NULL )
          local_stamp_info[cur_stamp].field_info_ptr = cur_field_info;
        cur_field_info->field_name = field_name;
        ++local_stamp_layout[cur_stamp].number_of_fields;
        ++cur_field_layout;
        ++cur_field_info;
      }
        break;
    case container_kind:
        cur_stamp = STAMP(codes[idx].data0);
#ifdef DUMP_GC_BOOT
        printf("%s:%d   container_kind  cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
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
#ifdef DUMP_GC_BOOT
        printf("%s:%d   variable_field cur_stamp = %d\n", __FILE__, __LINE__, cur_stamp);
#endif
        GCTOOLS_ASSERT(cur_field_layout<max_field_layout);
        cur_field_layout->field_offset = field_offset;
        if ( local_stamp_info[cur_stamp].container_info_ptr == NULL )
          local_stamp_info[cur_stamp].container_info_ptr = &local_container_info[cur_container_info_idx++];
        GCTOOLS_ASSERT(cur_container_info_idx<=number_of_containers);
        local_stamp_info[cur_stamp].container_info_ptr->field_name = field_name;
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
