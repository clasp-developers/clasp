/*
    File: print.h
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
#ifndef	_core_print_H
#define _core_print_H

#include "core/object.h"
#include "corePackage.fwd.h"
#include "core/character.fwd.h"
#include "core/write_object.h"
#include "wrappers.h"

namespace core
{

    int clasp_print_base();
    int brcl_print_level();
    int brcl_print_length();
    bool brcl_print_radix();
    Symbol_sp clasp_print_case();
    bool clasp_print_gensym();
    bool brcl_print_array();
    bool clasp_print_readably();
    bool clasp_print_escape();
    bool brcl_print_circle();

    T_mv af_write(T_sp x, T_sp strm, T_sp array, T_sp base,
		  T_sp cas, T_sp escape, T_sp gensym, T_sp length,
		  T_sp level, T_sp lines, T_sp miser_width, T_sp pprint_dispatch,
		  T_sp pretty, T_sp radix, T_sp readability, T_sp right_margin );

    void initialize_print();

};
#endif
