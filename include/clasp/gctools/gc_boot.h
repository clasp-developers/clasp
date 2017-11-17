/*
    File: gc_boot.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#ifndef GC_BOOT_H
#define GC_BOOT_H
namespace gctools {

  enum Data_types {
      SMART_PTR_OFFSET,
      TAGGED_POINTER_OFFSET,
      ARRAY_OFFSET,
      POINTER_OFFSET,
      CONSTANT_ARRAY_OFFSET,
      ctype_double,
      ctype_float,
      ctype_int,
      ctype_short,
      ctype_unsigned_char,
      ctype_signed_char,
      ctype_unsigned_short,
      ctype_signed_short,
      ctype_unsigned_long,
      ctype_unsigned_int,
      ctype_long,
      ctype_long_long,
      ctype_char,
      ctype__Bool,
      ctype_enum_core__StreamMode,
      ctype_core__FrameStamp,
      last_data_type };
  
  enum Layout_cmd {
      class_kind=0, container_kind=1, templated_kind=2,
      fixed_field=3,
      variable_array0=4,
      variable_capacity=5, variable_field=6,
      templated_class_jump_table_index=7,
      container_jump_table_index=8,
      bitunit_container_kind=9,
      variable_bit_array0=10,
      layout_end
  };

  struct Layout_code {
    Layout_cmd    cmd;
    uintptr_clasp_t     data0;
    uintptr_clasp_t     data1;
    uintptr_clasp_t     data2;
    const char*   description;
  };

  struct Field_layout {
    size_t            field_offset;
  };

  struct Field_info {
    const char*    field_name;
  };

  struct Container_info {
    const char*    field_name;
  };

  struct Container_layout {
    Field_layout*     field_layout_start; // Points into global_field_layout_table
    uint              number_of_fields;
//    uint              bits_per_bitunit;
//    size_t            data_offset;
//    size_t            end_offset;
//    size_t            capacity_offset;
  };

  enum Layout_operation { class_container_op, bitunit_container_op, templated_op };
  struct Stamp_info {
    Layout_operation    layout_op;
    const char*   name;
    Field_info*   field_info_ptr; // Only applies to classes
    Container_info* container_info_ptr; // 
  };

  struct Stamp_layout {
    Layout_operation  layout_op; // One of class_container_op, bitunit_container_op, templated_op
    uint              number_of_fields;
    uint              bits_per_bitunit;
    uint              size;
    uint              element_size;
    uint              data_offset;
    uint              end_offset;
    uint              capacity_offset;
    Field_layout*     field_layout_start; // Points into global_field_layout_table
    Container_layout* container_layout;
  };

  extern Layout_code* get_stamp_layout_codes();
  extern size_t           global_stamp_max;
  extern Stamp_info*       global_stamp_info;
  extern Stamp_layout*     global_stamp_layout;
  extern Field_info*      global_field_info;
  extern Field_layout*    global_field_layout;


  void build_stamp_field_layout_tables();


#define FRIEND_GC_INTERFACE() friend gctools::Layout_code* gctools::get_stamp_layout_codes()

};

#endif
