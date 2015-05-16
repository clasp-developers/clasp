/*
    File: clasp.h
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
#ifndef _core_clasp_H
#define _core_clasp_H

#error "This file doesn't do anything"

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/lisp.h>

struct clasp_State {
  int x;
};

#define CLASP_NOREF 0
#define clasp_NOREF 0
#define CLASP_REGISTRYINDEX 0

int brclL_ref(clasp_State *, int);
void clasp_rawgeti(clasp_State *, int, int);
void clasp_rawseti(clasp_State *, int, int);
void brclL_unref(clasp_State *, int, int);
void *clasp_touserdata(clasp_State *, int);
void clasp_pushvalue(clasp_State *, int);
void clasp_pushnil(clasp_State *);
bool clasp_isnil(clasp_State *, int);
void clasp_pop(clasp_State *, int);
void clasp_pushliteral(clasp_State *, const char *);
void clasp_rawget(clasp_State *, int);
int clasp_gettop(clasp_State *);
void clasp_pushnumber(clasp_State *, int);
double clasp_tonumber(clasp_State *, int);
bool clasp_isnumber(clasp_State *, int);

#endif
