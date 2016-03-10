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
#include <clasp/core/foundation.h>
namespace gctools {

  enum Layout_cmd {
      class_kind, class_size, field_fix,
      container_kind, container_jump_table_index, container_content_size, container_field_fix,
      templated_class_kind, templated_class_jump_table_index, templated_class_field_fix,
      layout_end
  };

  struct Layout_code {
    Layout_cmd    cmd;
    uintptr_t     data;
    const char*   description;
  };

  struct Field_layout {
    int            field_offset;
  };

  struct Field_info {
    const char*    field_name;
  };

  struct Class_layout {
    Field_layout*  field_layout_start; // Points into global_field_layout_table
    uint            number_of_fields;
    uint            size;
  };

  struct Jump_table_layout {
    uint        jump_table_index;
  };

  enum Layout_operation { class_operation, jump_table_operation };
  struct Kind_info {
    Layout_operation    layout_op;
    const char*   name;
    Field_info*   field_info_ptr; // Only applies to classes
  };

  struct Kind_layout {
    Layout_operation    layout_operation; // One of class_kind, templated_class_kind, container_kind
  // Union of the layout info for each layout_cmd
    union {
      Class_layout class_;
      Jump_table_layout jump;
    };
  };

  extern Layout_code* get_kind_layout_codes();
  extern size_t           global_kind_max;
  extern Kind_info*       global_kind_info;
  extern Kind_layout*     global_kind_layout;
  extern Field_info*      global_field_info;
  extern Field_layout*    global_field_layout;


  void build_kind_field_layout_tables();

};

#endif
