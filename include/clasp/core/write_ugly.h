/*
    File: write_ugly.h
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
#ifndef _core_write_ugly_H
#define _core_write_ugly_H

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/character.fwd.h>
#include <clasp/core/wrappers.h>

namespace core {
void write_fixnum(T_sp strm, T_sp i);
T_sp write_ugly_object(T_sp x, T_sp stream);
void _clasp_write_fixnum(gc::Fixnum i, T_sp stream);
void write_float(Float_sp f, T_sp stream);

};
#endif
