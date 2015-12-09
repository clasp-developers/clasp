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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>

namespace core {

inline bool cl_symbolp(T_sp obj) {
  return gc::IsA<Symbol_sp>(obj);
}

 LAMBDA(arg);
 DECLARE();
 DOCSTRING("endp");
inline CL_DEFUN bool cl__endp(T_sp arg) {
  if (arg.consp())
    return false;
  if (arg.nilp())
    return true;
  TYPE_ERROR(arg, cl::_sym_list);
};

 LAMBDA(arg);
 DECLARE();
 DOCSTRING("atom");
inline CL_DEFUN bool cl__atom(T_sp obj) {
  return !obj.consp();
}

inline bool cl_consp(T_sp obj) {
  return obj.consp();
};

inline bool cl_listp(T_sp obj) {
  if (obj.consp())
    return true;
  return obj.nilp();
}

bool af_llvm_sys_value_p(T_sp obj);
bool core__interpreted_function_p(T_sp obj);
bool af_classp(T_sp obj);
bool af_integerP(T_sp obj);
bool af_realP(T_sp obj);
bool cl_consp(T_sp obj);
bool af_sourceCodeConsP(T_sp obj);
bool af_vectorP(T_sp obj);
bool af_vectorObjectsP(T_sp obj);
bool core__lambda_list_handler_p(T_sp obj);
bool af_compiled_bodyP(T_sp obj);
bool af_keywordP(T_sp obj);
bool af_standardObjectP(T_sp obj);
//    bool core__structure_object_p(T_sp obj);
bool af_executableP(T_sp obj);
bool cl__functionp(T_sp obj);
bool af_compiledFunctionP(T_sp obj);
bool af_arrayP(T_sp obj);
bool af_arrayObjectsP(T_sp obj);
bool cl__numberp(T_sp obj);
bool af_floatP(T_sp obj);
bool core__short_float_p(T_sp obj);
bool core__single_float_p(T_sp obj);
bool core__double_float_p(T_sp obj);
bool core__long_float_p(T_sp obj);
bool af_complexP(T_sp obj);
bool af_rationalP(T_sp obj);
bool core__ratio_p(T_sp obj);
bool af_pointerP(T_sp obj);
bool cl__bit_vector_p(T_sp obj);
bool af_fixnumP(T_sp obj);
bool af_bignumP(T_sp obj);
bool af_stringP(T_sp obj);
bool af_simpleStringP(T_sp obj);
bool core__simple_string_p(T_sp obj);
bool cl__packagep(T_sp obj);
bool af_booleanP(T_sp obj);
bool af_specialFormP(T_sp obj);
bool cl__hash_table_p(T_sp obj);
bool cl__readtablep(T_sp obj);
bool af_characterP(T_sp obj);
bool core__path_p(T_sp obj);
bool cl__simple_bit_vector_p(T_sp obj);
bool core__activation_frame_p(T_sp obj);
bool af_single_dispatch_activation_frame_p(T_sp obj);
bool core__single_dispatch_generic_function_p(T_sp obj);
bool core__external_object_p(T_sp obj);

bool cl__pathnamep(T_sp obj);
bool core__logical_pathname_p(T_sp obj);

void initialize_predicates();
}

#endif // _core_predicates_H
