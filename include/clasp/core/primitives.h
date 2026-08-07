#pragma once
/*
    File: primitives.h
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

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/array.h>
#include <clasp/core/character.fwd.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/externalObject.h>

namespace core {

extern T_sp cl__macro_function(Symbol_sp symbol, T_sp env);
extern T_mv core__separate_pair_list(List_sp listOfPairs);

extern Symbol_sp core__function_block_name(T_sp functionName);

extern List_sp cl__read_delimited_list(Character_sp chr, T_sp input_stream_designator, T_sp recursive_p);

T_sp cl__read(T_sp input_stream_designator, T_sp eof_error_p = nil<T_O>(), T_sp eof_value = nil<T_O>(),
              T_sp recursive_p = nil<T_O>());
T_sp cl__read_preserving_whitespace(T_sp input_stream_designator, T_sp eof_error_p = nil<T_O>(), T_sp eof_value = nil<T_O>(),
                                    T_sp recursive_p = nil<T_O>());

T_sp cl__type_of(T_sp x);
T_sp core__notany_list(T_sp predicate, List_sp sequences);
T_sp core__every_list(T_sp predicate, List_sp sequences);

T_sp cl__mapcar(T_sp func_desig, List_sp lists);

T_sp cl__append(Vaslist_sp lists);

//    Stream_mv af_open(T_sp filespec, Symbol_sp direction, T_sp element_type, T_sp if_exists, T_sp if_does_not_exist, T_sp
//    external_format );

Symbol_sp cl__gensym(T_sp x = nil<T_O>());

}; // namespace core

namespace core {
T_sp cl__mapc(T_sp op, List_sp lists);
T_sp cl__mapcar(T_sp op, List_sp lists);
}; // namespace core

namespace core {
int clasp_musleep(double dsec, bool alertable);

T_sp core__valid_function_name_p(T_sp arg);
T_sp core__create_tagged_immediate_value_or_nil(T_sp object);
bool cl__constantp(T_sp obj, T_sp env = nil<T_O>());
T_mv cl__values_list(T_sp list);
T_sp cl__compiler_macro_function(core::T_sp name, core::T_sp env);
bool cl__fboundp(T_sp functionName);
T_sp core__get_global_inline_status(core::T_sp name, core::T_sp env);
void core__setf_global_inline_statis(core::T_sp name, bool status, core::T_sp env);
T_sp cl__fdefinition(T_sp functionName);
FunctionCell_sp core__ensure_function_cell(T_sp functionName);
T_sp cl__special_operator_p(Symbol_sp sym);
T_sp cl__sleep(Real_sp oseconds);
List_sp core__list_from_vaslist(Vaslist_sp valist);
void core__gdb(T_sp msg);
T_sp core__next_number();
}; // namespace core

namespace core {
/*! Calculate crc32 for a block of data */
void crc32(const void* data, size_t n_bytes, uint32_t* crc);
}; // namespace core
