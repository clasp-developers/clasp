/*
    File: print.h
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
#ifndef _core_print_H
#define _core_print_H

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/character.fwd.h>
#include <clasp/core/write_object.h>
#include <clasp/core/wrappers.h>

namespace core {

cl_index clasp_print_base();
cl_index clasp_print_level();
cl_index clasp_print_length();
bool clasp_print_radix();
Symbol_sp clasp_print_case();
bool clasp_print_gensym();
bool clasp_print_array();
bool clasp_print_readably();
bool clasp_print_escape();
bool clasp_print_circle();

T_mv af_write(T_sp x, T_sp strm, T_sp array, T_sp base,
              T_sp cas, T_sp escape, T_sp gensym, T_sp length,
              T_sp level, T_sp lines, T_sp miser_width, T_sp pprint_dispatch,
              T_sp pretty, T_sp radix, T_sp readability, T_sp right_margin);

T_sp cl_print(T_sp obj, T_sp stream);
T_sp cl_prin1(T_sp obj, T_sp stream);

void initialize_print();
};
#endif
