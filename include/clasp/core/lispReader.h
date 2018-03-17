/*
    File: lispReader.h
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
#ifndef _core_lispReader_H_
#define _core_lispReader_H_

#include <clasp/core/object.h>
#include <clasp/core/lispReader.h>

namespace core {

extern List_sp read_list(T_sp sin, claspCharacter end_char, bool allow_consing_dot);

extern T_mv lisp_object_query(T_sp, bool eofErrorP, T_sp eofValue, bool recursiveP);

extern T_sp read_lisp_object(T_sp, bool eofErrorP, T_sp eofValue, bool recursiveP);

 void unread_ch(T_sp sin, Character_sp c);

 Character_sp lexeme_character(T_sp lexeme);
 List_sp collect_lexemes(/*Character_sp*/ T_sp tc, T_sp sin);
 void make_str_preserve_case(StrWNs_sp sout, List_sp cur_char);
 void make_str_upcase(StrWNs_sp sout, List_sp cur_char);
 void make_str(StrWNs_sp sout, List_sp cur_char);
 
 
extern void exposeCore_lisp_reader();
};
#endif /* _lispReader_H_ */
