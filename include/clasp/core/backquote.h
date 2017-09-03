/*
    File: backquote.h
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
#ifndef _core_backquote_H_
#define _core_backquote_H_

#include <clasp/core/object.h>

namespace core {

extern T_mv core__backquote_completely_process(T_sp ox);
extern T_sp core__backquote_process(T_sp ox);
extern T_sp core__backquote_bracket(T_sp ox);
extern T_sp core__backquote_simplify_args(T_sp x);
extern T_sp core__backquote_attach_append(T_sp op, T_sp item, T_sp result);
extern List_sp core__backquote_attach_conses(T_sp items, T_sp result);

extern T_sp core__backquote_simplify(T_sp x);
extern T_sp core__backquote_remove_tokens(T_sp x);

 void initialize_backquote();
};
#endif /* _backquote_H_ */
