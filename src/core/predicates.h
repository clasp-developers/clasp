/*
    File: predicates.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#ifndef	_core_predicates_H
#define _core_predicates_H

#include "core/foundation.h"
#include "core/object.h"


namespace core
{

    bool af_symbolp(T_sp obj);
    bool af_endp(T_sp obj);
    bool af_atom(T_sp obj);

    bool af_listp(T_sp obj);

    bool af_llvm_sys_value_p(T_sp obj);
    bool af_interpretedFunctionP(T_sp obj);
    bool af_endp(T_sp obj);
    bool af_classp(T_sp obj);
    bool af_integerP(T_sp obj);
    bool af_realP(T_sp obj);
    bool af_consP(T_sp obj);
    bool af_sourceCodeConsP(T_sp obj);
    bool af_vectorP(T_sp obj);
    bool af_vectorObjectsP(T_sp obj);
    bool af_lambda_list_handler_p(T_sp obj);
    bool af_compiled_bodyP(T_sp obj);
    bool af_keywordP(T_sp obj);
    bool af_standardObjectP(T_sp obj);
//    bool af_structureObjectP(T_sp obj);
    bool af_executableP(T_sp obj);
    bool af_functionP(T_sp obj);
    bool af_compiledFunctionP(T_sp obj);
    bool af_arrayP(T_sp obj);
    bool af_arrayObjectsP(T_sp obj);
    bool af_numberP(T_sp obj);
    bool af_floatP(T_sp obj);
    bool af_shortFloatP(T_sp obj);
    bool af_singleFloatP(T_sp obj);
    bool af_doubleFloatP(T_sp obj);
    bool af_longFloatP(T_sp obj);
    bool af_complexP(T_sp obj);
    bool af_rationalP(T_sp obj);
    bool af_ratioP(T_sp obj);
    bool af_pointerP(T_sp obj);
    bool af_bitVectorP(T_sp obj);
    bool af_fixnumP(T_sp obj);
    bool af_bignumP(T_sp obj);
    bool af_stringP(T_sp obj);
    bool af_simpleStringP(T_sp obj);
    bool af_strP(T_sp obj);
    bool af_packageP(T_sp obj);
    bool af_booleanP(T_sp obj);
    bool af_specialFormP(T_sp obj);
    bool af_hashTableP(T_sp obj);
    bool af_readtableP(T_sp obj);
    bool af_characterP(T_sp obj);
    bool af_pathP(T_sp obj);
    bool af_simple_bit_vector_p(T_sp obj);
    bool af_activation_frame_p(T_sp obj);
    bool af_single_dispatch_activation_frame_p(T_sp obj);
    bool af_singleDispatchGenericFunctionP(T_sp obj);
    bool af_externalObjectP(T_sp obj);

    bool af_pathnamep(T_sp obj);
    bool af_logicalPathnameP(T_sp obj);

    void initialize_predicates();
}

#endif // _core_predicates_H
