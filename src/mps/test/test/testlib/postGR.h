/*
    File: postGR.h
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
In grotesq interface
 - "space" becomes "arena"
*/

#include "postMO.h"

#define mps_space_clamp(a) \
 mps_arena_clamp(a)

#define mps_space_release(a) \
 mps_arena_release(a)

#define mps_space_park(a) \
 mps_arena_park(a)

#define mps_space_collect(a) \
 mps_arena_collect(a)

#define mps_space_destroy(a) \
 mps_arena_destroy(a)

#define mps_space_reserved(a) \
 mps_arena_reserved(a)

#define mps_space_committed(a) \
 mps_arena_committed(a)
