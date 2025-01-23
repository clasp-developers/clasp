#pragma once

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

namespace gctools {

enum Data_types {
  DONT_EXPOSE_OFFSET,
  SMART_PTR_OFFSET,
  ATOMIC_SMART_PTR_OFFSET,
  TAGGED_POINTER_OFFSET,
  WEAK_PTR_OFFSET,
  EPHEMERON_OFFSET,
  RAW_POINTER_OFFSET,
  ARRAY_OFFSET,
  POINTER_OFFSET,
  CONSTANT_ARRAY_OFFSET,
  ctype_double,
  ctype_float,
  ctype_long_double,
  ctype_int,
  ctype_short,
  ctype_unsigned_char,
  ctype_signed_char,
  ctype_unsigned_short,
  ctype_signed_short,
  ctype_unsigned_long,
  ctype_unsigned_long_long,
  ctype_unsigned_int,
  ctype_long,
  ctype_long_long,
  ctype_char,
  ctype__Bool,
  ctype_enum_core__StreamMode,
  ctype_core__FrameStamp,
  ctype_const_char_ptr,
  ctype_size_t,
  ctype_char32_t,
  ctype_opaque_ptr,
  CXX_FIXUP_OFFSET,
  ATOMIC_POD_OFFSET_unsigned_short,
  ATOMIC_POD_OFFSET_unsigned_long,
  ATOMIC_POD_OFFSET_mp__ProcessPhase,
  ATOMIC_POD_OFFSET_unsigned_int,
  ATOMIC_POD_OFFSET_long_long,
  ATOMIC_POD_OFFSET__Bool,
  ATOMIC_POD_OFFSET_long,
  CXX_SHARED_MUTEX_OFFSET,
  last_data_type
};

//
// Use powers of two for the flags
//
enum ClassFlags { IS_POLYMORPHIC = 1, COMPLEX_SCAN = 2 };

extern uintptr_t global_lisp_kind;
extern uintptr_t global_cons_kind;
extern uintptr_t global_class_kind;
extern uintptr_t global_container_kind;
extern uintptr_t global_code_kind;
extern uintptr_t global_atomic_kind;

extern int global_container_proc_index;

extern void dump_data_types(std::ostream& fout, const std::string& indent);

enum Layout_cmd {
  class_kind = 0,
  container_kind = 1,
  templated_kind = 2,
  fixed_field = 3,
  variable_array0 = 4,
  variable_capacity = 5,
  variable_field = 6,
  templated_class_jump_table_index = 7,
  container_jump_table_index = 8,
  bitunit_container_kind = 9,
  variable_bit_array0 = 10,
  layout_end
};

//
// Layout_code represents the entries in GC_OBJ_SCAN_HELPERS section in clasp_gcXXX.cc
// It is a Layout_cmd enum followed by three uintptr_t arguments whose meaning depends
// on the value of 'cmd'.
// For example:
// When cmd = class_kind
// Then data0 = The header STAMP_xxx of the class
//      data1 = sizeof(class)
//      data2 = 0
//      data3 = bitwise OR of flags
//      description = C-string name of the class
// When cmd = fixed_field
// Then data0 = enum value that represents the type of the field
//      data1 = sizeof(field)
//      data2 = offset of field from start of class
//      data3 = 0
//      description = C-string name of the field

struct Layout_code {
  Layout_cmd cmd;
  uintptr_t data0;
  uintptr_t data1;
  uintptr_t data2;
  uintptr_t data3;
  const char* description;
};

struct Field_layout {
  size_t field_offset;
};

struct Field_info {
  const char* field_name;
  size_t data_type;
};

struct Container_info {
  const char* field_name;
  size_t data_type;
};

struct Container_layout {
  // A bitmap of pointer fields for mps fixing and boehm marking
  uintptr_t container_field_pointer_bitmap = 0;
  int container_field_pointer_count = 0;
  Field_layout* field_layout_start; // Points into global_field_layout_table
  uint number_of_fields = 0;
  uint element_size;
  uint              bits_per_bitunit = 0;
  size_t            data_offset;
  size_t            end_offset;
  size_t            capacity_offset;
};

enum Layout_operation { class_container_op, bitunit_container_op, templated_op, undefined_op };
struct Stamp_info {
  Layout_operation layout_op;
  const char* name;
  Field_info* field_info_ptr = nullptr;         // Only applies to classes
  Container_info* container_info_ptr = nullptr; //
};

#define KIND_UNDEFINED 99999
struct Boehm_info {
  bool _kind_defined = false;
  int _container_element_work = 0;
  uintptr_t _kind = KIND_UNDEFINED;
};

struct Stamp_layout {
  // One of class_container_op, bitunit_container_op, templated_op
  Layout_operation layout_op = undefined_op;
  Boehm_info boehm;
  // A bitmap of pointer fields for mps fixing and (once shifted right to skip clasp header - boehm marking)
  // The most significant bit indicates the vtable - it must be zero
  uintptr_t class_field_pointer_bitmap = 0;
  uint flags;
  uint number_of_fields = 0;
  uint size = 0;
  // Points into global_field_layout_table
  Field_layout* field_layout_start = nullptr;
  Container_layout* container_layout = nullptr;
};

extern Layout_code* get_stamp_layout_codes();
extern size_t global_stamp_max;
extern Stamp_info* global_stamp_info;
extern Stamp_layout* global_stamp_layout;
extern Field_info* global_field_info;
extern Field_layout* global_field_layout;

typedef enum { precise_info, lldb_info } WalkKind;
void walk_stamp_field_layout_tables(WalkKind walk, std::ostream& stream);

#define FRIEND_GC_INTERFACE() friend gctools::Layout_code* gctools::get_stamp_layout_codes()

}; // namespace gctools
