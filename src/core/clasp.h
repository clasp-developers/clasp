/*
    File: clasp.h
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
#ifndef _core_brcl_H
#define _core_brcl_H


#include "foundation.h"
#include "object.h"
#include "cons.h"
#include "lisp.h"



struct brcl_State
{
    int x;
};

#define	BRCL_NOREF 0
#define	brcl_NOREF 0
#define BRCL_REGISTRYINDEX 0

int brclL_ref(brcl_State*, int);
void brcl_rawgeti(brcl_State*, int, int );
void brcl_rawseti(brcl_State*, int, int );
void brclL_unref(brcl_State*, int, int );
void* brcl_touserdata(brcl_State*, int );
void brcl_pushvalue(brcl_State*, int);
void brcl_pushnil(brcl_State*);
bool brcl_isnil(brcl_State*,int);
void brcl_pop(brcl_State*,int);
void brcl_pushliteral(brcl_State*,const char*);
void brcl_rawget(brcl_State*,int);
int brcl_gettop(brcl_State*);
void brcl_pushnumber(brcl_State*,int);
double brcl_tonumber(brcl_State*,int);
bool brcl_isnumber(brcl_State*,int);

#endif
