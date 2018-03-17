/*
    File: BridgeCommonLisp.h
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
#ifndef core_BridgeCommonLisp_H
#define core_BridgeCommonLisp_H

//
// Plays the role of Python.h
//

#include <assert.h>
#include <brclport.h>

#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/lisp.h>
#include <clasp/core/singleDispatchMethod.h>

typedef core::T_O BrclObject;
typedef core::Class_O BrclTypeObject;
typedef core::SingleDispatchMethod_O BrclMethodObject;

core::T_O *Brcl_None = _Nil<core::T_O>().get();

#endif
