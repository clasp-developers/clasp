#include <clasp/gctools/gc_boot.h>


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
      printf("%s:%d the layout code table is damaged\n");
      abort();
    }
    if ( codes[idx].cmd == class_kind ||
         codes[idx].cmd == container_kind ||
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
    } else if ( codes[idx].cmd == variable_array0 ) {
      ++number_of_containers;
    }
    ++idx;
  }
  // Now malloc memory for the tables
  // now that we know the size of everything
  global_kind_info = (Kind_info*)malloc(sizeof(Kind_info)*(global_kind_max+1));
  global_kind_layout = (Kind_layout*)malloc(sizeof(Kind_layout)*(global_kind_max+1));
  global_field_info = (Field_info*)malloc(sizeof(Field_info)*(number_of_fixable_fields+1));
  global_field_layout = (Field_layout*)malloc(sizeof(Field_layout)*(number_of_fixable_fields+1));
  global_container_layout = (Container_layout*)malloc(sizeof(Container_layout)*(number_of_containers+1));
  global_container_info = (Container_info*)malloc(sizeof(Container_info)*(number_of_containers+1));
  // Fill in the immediate kinds
  // Traverse the global_kind_layout_codes again and fill the tables
  codes = get_kind_layout_codes();
  Field_layout* cur_field_layout= global_field_layout;
  Field_info* cur_field_info= global_field_info;
  Container_layout* cur_container_layout = global_container_layout;
  Container_info* cur_container_info = global_container_info;
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
        global_kind_layout[cur_kind].size = codes[idx].data1;
        global_kind_info[cur_kind].name = codes[idx].description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        global_kind_info[cur_kind].container_info_ptr = NULL;
        break;
    case fixed_field:
        if ( !(codes[idx].data0 == SMART_PTR_OFFSET
               || codes[idx].data0 == TAGGED_POINTER_OFFSET
               || codes[idx].data0 == POINTER_OFFSET )) continue;
        if ( global_kind_layout[cur_kind].field_layout_start == NULL )
          global_kind_layout[cur_kind].field_layout_start = cur_field_layout;
        ++global_kind_layout[cur_kind].number_of_fields;
        cur_field_layout->field_offset = codes[idx].data2;
        ++cur_field_layout;
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
        global_kind_layout[cur_kind].field_layout_start = NULL;
        global_kind_layout[cur_kind].container_layout = NULL;
        global_kind_info[cur_kind].name = codes[idx].description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        global_kind_info[cur_kind].container_info_ptr = NULL;
        break;
    case variable_array0:
        global_kind_layout[cur_kind].container_layout = cur_container_layout;
        ++cur_container_layout;
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
        cur_field_layout->field_offset = codes[idx].data2;
        ++cur_field_layout;
        ++global_kind_layout[cur_kind].container_layout->number_of_fields;
        if ( global_kind_info[cur_kind].container_info_ptr == NULL )
          global_kind_info[cur_kind].container_info_ptr = cur_container_info;
        cur_container_info->field_name = codes[idx].description;
        ++cur_container_info;
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
  printf("%s:%d  Done building kind_layout tables\n", __FILE__, __LINE__ );
}
};

