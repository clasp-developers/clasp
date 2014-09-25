/*
    File: arg.h
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
/* $Id$
arg.h
   useful things for arg-err tests
*/

#ifndef arg_h
#define arg_h

#include "testlib.h"

#define UNALIGNED ((mps_addr_t) (((char *) NULL) + 1))

#define MPS_RANK_MIN 0
#define MPS_RANK_MAX 3

#define MPS_RM_MIN 0
#define MPS_RM_MAX 3

#define MPS_ALIGN_MIN 1

/* possibly nasty values with high bits set */

#define HIGHBIT_CHAR (~(((unsigned char)  -1) >> 1))
#define HIGHBIT_INT (~(((unsigned int) -1) >> 1))
#define HIGHBIT_SHORT (~(((unsigned short) -1) >> 1))
#define HIGHBIT_LONG (~(((unsigned long) -1) >> 1))
#define HIGHBIT_SIZE (~((~ (size_t) 0) >> 1))

/* n.b. the last line above will work in ansi C because
   size_t is an unsigned type. In sos libs, size_t, however
   is a signed type; it still works tho', because in sos,
   >> on a negative value is a logical (not arithmetical) shift.
*/

#endif
