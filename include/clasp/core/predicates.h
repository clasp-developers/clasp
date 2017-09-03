/*
    File: predicates.h
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
#ifndef _core_predicates_H
#define _core_predicates_H

#include <clasp/core/object.h>

namespace core {

CL_DEFUN inline bool cl__symbolp(T_sp obj) {
  return gc::IsA<Symbol_sp>(obj);
}

 CL_DEFUN inline bool core__fixnump(T_sp arg)
 {
   return arg.fixnump();
 }
 
 CL_LAMBDA(arg);
 CL_DECLARE();
 CL_DOCSTRING("endp");
CL_DEFUN inline bool cl__endp(T_sp arg) {
  if (arg.consp())
    return false;
  if (arg.nilp())
    return true;
  TYPE_ERROR(arg, cl::_sym_list);
};

 CL_LAMBDA(arg);
 CL_DECLARE();
 CL_DOCSTRING("atom");
inline CL_DEFUN bool cl__atom(T_sp obj) {
  return !obj.consp();
}

CL_DEFUN inline bool cl__consp(T_sp obj) {
  return obj.consp();
};

 CL_DEFUN inline bool cl__characterp(T_sp obj) {
   return obj.characterp();
 }

CL_DEFUN inline bool cl__listp(T_sp obj) {
  if (obj.consp())
    return true;
  return obj.nilp();
}


bool cl__bit_vector_p(T_sp obj);
bool clos__classp(T_sp obj);
bool cl__compiled_bodyP(T_sp obj);
bool cl__compiled_function_p(T_sp obj);
bool cl__complexp(T_sp obj);
bool cl__consp(T_sp obj);
bool cl__floatp(T_sp obj);
bool cl__functionp(T_sp obj);
bool cl__hash_table_p(T_sp obj);
bool cl__integerp(T_sp obj);
bool cl__keywordp(T_sp obj);
bool cl__numberp(T_sp obj);
bool cl__packagep(T_sp obj);
bool cl__pathnamep(T_sp obj);
bool cl__rationalp(T_sp obj);
bool cl__readtablep(T_sp obj);
bool cl__realP(T_sp obj);
bool cl__simple_bit_vector_p(T_sp obj);
bool cl__simple_string_p(T_sp obj);
bool cl__stringp(T_sp obj);
 bool core__base_string_p(T_sp obj);
 bool core__extended_string_p(T_sp obj);
bool cl__vectorp(T_sp obj);
bool core__activation_frame_p(T_sp obj);
bool core__array_objects_p(T_sp obj);
bool core__arrayp(T_sp obj);
bool core__bignump(T_sp obj);
bool core__booleanp(T_sp obj);
bool core__double_float_p(T_sp obj);
bool core__executableP(T_sp obj);
bool core__external_object_p(T_sp obj);
bool core__fixnump(T_sp obj);
bool core__interpreted_function_p(T_sp obj);
bool core__lambda_list_handler_p(T_sp obj);
bool core__logical_pathname_p(T_sp obj);
bool core__long_float_p(T_sp obj);
bool core__pointerp(T_sp obj);
bool core__ratio_p(T_sp obj);
bool core__short_float_p(T_sp obj);
bool core__single_dispatch_activation_frame_p(T_sp obj);
bool core__single_dispatch_generic_function_p(T_sp obj);
bool core__single_float_p(T_sp obj);
bool core__special_form_p(T_sp obj);
bool core__standardObjectP(T_sp obj);
inline bool core__non_simple_stringp(T_sp obj) { return gc::IsA<StrNs_sp>(obj); };
bool llvmo__llvm_sys_value_p(T_sp obj);
 bool core__proper_list_p(T_sp obj);
}

namespace ext {
  bool local_function_form_p(core::T_sp form);
};

  

#endif // _core_predicates_H
