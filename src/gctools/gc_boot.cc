#include <clasp/gctools/gc_boot.h>


namespace gctools {



 size_t           global_kind_max;
 Kind_info*       global_kind_info;
 Kind_layout*     global_kind_layout;
 Field_info*      global_field_info;
 Field_layout*    global_field_layout;



/*! This will build the tables that MPS needs to fix pointers in simple classes
It uses global_class_layout_codes to malloc space for and
build the global_class_info_table and the global_field_layout_table.
*/
void build_kind_field_layout_tables()
{
  // First pass through the global_kind_layout_codes_table
  // to count the number of kinds and the number of fields
  Layout_code* cur = get_kind_layout_codes();
  size_t number_of_fields = 0;
  global_kind_max = 0;
  while (cur->cmd != layout_end) {
    if ( cur->cmd == class_kind ||
         cur->cmd == container_kind ||
         cur->cmd == templated_class_kind ) {
      if ( global_kind_max < cur->data ) {
        global_kind_max = cur->data;
      }
    } else if ( cur->cmd == field_fix ) {
      ++number_of_fields;
    }
    ++cur;
  }
  // Now malloc memory for the tables
  // now that we know the size of everything
  global_kind_info = (Kind_info*)malloc(sizeof(Kind_info)*(global_kind_max+1));
  global_kind_layout = (Kind_layout*)malloc(sizeof(Kind_layout)*(global_kind_max+1));
  global_field_info = (Field_info*)malloc(sizeof(Field_info)*number_of_fields);
  global_field_layout = (Field_layout*)malloc(sizeof(Field_layout)*number_of_fields);
  // Fill in the immediate kinds
  
  // Traverse the global_kind_layout_codes again and fill the tables
  cur = get_kind_layout_codes();
  Field_layout* cur_field_layout= global_field_layout;
  Field_info* cur_field_info= global_field_info;
  int cur_kind=0;
  while (cur->cmd != layout_end) {
    switch (cur->cmd) {
    case class_kind: 
        cur_kind = cur->data;
        global_kind_layout[cur_kind].layout_operation = class_operation;
        global_kind_layout[cur_kind].class_.field_layout_start = cur_field_layout;
        global_kind_layout[cur_kind].class_.number_of_fields = 0;
        global_kind_info[cur_kind].name = cur->description;
        global_kind_info[cur_kind].field_info_ptr = cur_field_info;
        break;
    case class_size: 
        global_kind_layout[cur_kind].class_.size = cur->data;
        break;
    case field_fix:
        ++global_kind_layout[cur_kind].class_.number_of_fields;
        ASSERT(cur->data%8 == 0);
        cur_field_layout->field_offset = cur->data/8;
        cur_field_info->field_name = cur->description;
        ++cur_field_layout;
        ++cur_field_info;
        break;
    case container_kind:
        cur_kind = cur->data;
        global_kind_layout[cur_kind].layout_operation = jump_table_operation;
        global_kind_info[cur_kind].name = cur->description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        break;
    case container_jump_table_index:
        global_kind_layout[cur_kind].jump.jump_table_index = cur->data;
        break;
    case templated_class_kind:
        cur_kind = cur->data;
        global_kind_layout[cur_kind].layout_operation = jump_table_operation;
        global_kind_info[cur_kind].name = cur->description;
        global_kind_info[cur_kind].field_info_ptr = NULL;
        break;
    case templated_class_jump_table_index:
        global_kind_layout[cur_kind].jump.jump_table_index = cur->data;
        break;
    default:
        THROW_HARD_ERROR(BF("The Layout_code table contained an illegal command: %d\n") % cur->cmd);
    }
    ++cur;
  }
}
};

