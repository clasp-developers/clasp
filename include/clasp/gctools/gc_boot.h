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
    DONT_EXPOSE_OFFSET,
  SMART_PTR_OFFSET,
  ATOMIC_SMART_PTR_OFFSET,
  TAGGED_POINTER_OFFSET,
  RAW_POINTER_OFFSET,
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
  ctype_opaque_ptr,
  CXX_FIXUP_OFFSET,
  ATOMIC_POD_OFFSET_unsigned_long,
  ATOMIC_POD_OFFSET_mp__ProcessPhase,
  ATOMIC_POD_OFFSET_unsigned_int,
  ATOMIC_POD_OFFSET_long_long,
  ATOMIC_POD_OFFSET__Bool,
  CXX_SHARED_MUTEX_OFFSET,
  last_data_type };

extern uintptr_t global_lisp_kind;
extern uintptr_t global_cons_kind;
extern uintptr_t global_class_kind;
extern uintptr_t global_container_kind;
extern uintptr_t global_code_kind;
extern uintptr_t global_atomic_kind;
extern uintptr_t global_strong_weak_kind;

extern int              global_container_proc_index;

inline void dump_data_types(FILE* fout, const std::string& indent)
{
#define DTNAME(_type_,_name_,_sz_) fprintf(fout,"%sInit_data_type( data_type=%d, name=\"%s\",sizeof=%lu)\n", indent.c_str(), _type_, _name_, _sz_)
  DTNAME(SMART_PTR_OFFSET,"smart_ptr",sizeof(void*));
  DTNAME(ATOMIC_SMART_PTR_OFFSET,"smart_ptr",sizeof(void*));
  DTNAME(TAGGED_POINTER_OFFSET,"tagged_ptr",sizeof(void*));
  DTNAME(ARRAY_OFFSET,"array",sizeof(void*));
  DTNAME(POINTER_OFFSET,"pointer",sizeof(void*));
  DTNAME(CONSTANT_ARRAY_OFFSET,"constant_array",sizeof(void*));
  DTNAME(ctype_double,"double",sizeof(double));
  DTNAME(ctype_float,"float",sizeof(float));
  DTNAME(ctype_int,"int",sizeof(int));
  DTNAME(ctype_short,"short",sizeof(short));
  DTNAME(ctype_unsigned_char,"unsigned_char",sizeof(unsigned char));
  DTNAME(ctype_signed_char,"signed_char",sizeof(signed char));
  DTNAME(ctype_unsigned_short,"unsigned_short",sizeof(unsigned short));
  DTNAME(ctype_signed_short,"signed_short",sizeof(signed short));
  DTNAME(ctype_unsigned_long,"unsigned_long",sizeof(unsigned long));
  DTNAME(ctype_unsigned_int,"unsigned_int",sizeof(unsigned int));
  DTNAME(ctype_long,"long",sizeof(long));
  DTNAME(ctype_long_long,"long_long",sizeof(long long));
  DTNAME(ctype_char,"char",sizeof(char));
  DTNAME(ctype__Bool,"_Bool",sizeof(bool));
  DTNAME(ctype_enum_core__StreamMode,"enum_core__StreamMode",sizeof(int));
  DTNAME(ctype_const_char_ptr,"const_char_ptr",sizeof(const char*));
  DTNAME(ctype_size_t,"size_t",sizeof(size_t));
  DTNAME(ctype_opaque_ptr,"opaque_ptr",sizeof(void*));
#define Init_global_ints(_name_,_value_) fprintf(fout,"%sInit_global_ints(name=\"%s\",value=%d)\n", indent.c_str(), _name_,_value_);
  Init_global_ints("TAG_BITS",TAG_BITS);
  Init_global_ints("IMMEDIATE_MASK",IMMEDIATE_MASK);
  Init_global_ints("FIXNUM_MASK",FIXNUM_MASK);
  Init_global_ints("GENERAL_TAG",GENERAL_TAG);
  Init_global_ints("CONS_TAG",CONS_TAG);
  Init_global_ints("SINGLE_FLOAT_TAG",SINGLE_FLOAT_TAG);
  Init_global_ints("CHARACTER_TAG",CHARACTER_TAG);
  Init_global_ints("VASLIST0_TAG",VASLIST0_TAG);
  Init_global_ints("FIXNUM_SHIFT",FIXNUM_SHIFT);
}

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
  uintptr_t     data0;
  uintptr_t     data1;
  uintptr_t     data2;
  const char*   description;
};

struct Field_layout {
  size_t         field_offset;
};

struct Field_info {
  const char*    field_name;
  size_t         data_type;
};

struct Container_info {
  const char*    field_name;
  size_t         data_type;
};

struct Container_layout {
  // A bitmap of pointer fields for mps fixing and boehm marking
  uintptr_t         container_field_pointer_bitmap;
  int               container_field_pointer_count;
  Field_layout*     field_layout_start; // Points into global_field_layout_table
  uint              number_of_fields;
  //    uint              bits_per_bitunit;
  //    size_t            data_offset;
  //    size_t            end_offset;
  //    size_t            capacity_offset;
  Container_layout() : container_field_pointer_bitmap(0)
                     , container_field_pointer_count(0)
  {};
};

enum Layout_operation { class_container_op, bitunit_container_op, templated_op, undefined_op };
struct Stamp_info {
  Layout_operation    layout_op;
  const char*   name;
  Field_info*   field_info_ptr; // Only applies to classes
  Container_info* container_info_ptr; // 
};

struct Boehm_info {
  bool              _kind_defined;
  uintptr_t         _class_bitmap;
  uintptr_t         _container_bitmap;
  int               _container_pointer_count;
  int               _container_element_work;
  uintptr_t         _kind;
  Boehm_info() : _kind_defined(false)
               , _class_bitmap(0)
               , _container_bitmap(0)
               , _container_pointer_count(0)
               , _container_element_work(0)
               , _kind(99999)
  {};
};

struct Stamp_layout {
  Layout_operation  layout_op; // One of class_container_op, bitunit_container_op, templated_op
  Boehm_info        boehm;
  // A bitmap of pointer fields for mps fixing and (once shifted right to skip clasp header - boehm marking)
  // The most significant bit indicates the vtable - it must be zero
  uintptr_t         class_field_pointer_bitmap; 
  uint              number_of_fields;
  uint              bits_per_bitunit;
  uint              size;
  uint              element_size;
  uint              data_offset;
  uint              end_offset;
  uint              capacity_offset;
  uint              image_save_load_poison;
  Field_layout*     field_layout_start; // Points into global_field_layout_table
  Container_layout* container_layout;
Stamp_layout() : layout_op(undefined_op)
  , boehm()
    , class_field_pointer_bitmap(0)
    , number_of_fields(0)
    , bits_per_bitunit(0)
    , size(0)
    , element_size(0)
    , data_offset(0)
    , end_offset(0)
    , capacity_offset(0)
    , image_save_load_poison(0)
    , field_layout_start(NULL) // Points into global_field_layout_table
  {};
};

extern Layout_code* get_stamp_layout_codes();
extern size_t           global_stamp_max;
extern Stamp_info*       global_stamp_info;
extern Stamp_layout*     global_stamp_layout;
extern Field_info*      global_field_info;
extern Field_layout*    global_field_layout;


typedef enum { mps_info, lldb_info } WalkKind;
void walk_stamp_field_layout_tables(WalkKind walk, FILE* fout=NULL);


#define FRIEND_GC_INTERFACE() friend gctools::Layout_code* gctools::get_stamp_layout_codes()

};

#endif
