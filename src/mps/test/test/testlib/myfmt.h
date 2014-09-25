/*
    File: myfmt.h
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
myfmt.h
   a format for scannable objects
*/

#ifndef myfmt_h
#define myfmt_h

#include "testlib.h"

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 formatcomments   1   print comments on scanning, fixing, copying
 copysurplus      1   copy the surplus space in objects when moving 

*/

extern int formatcomments;
extern int copysurplus;

typedef struct mycell
  {
   mps_word_t    tag;
   mps_word_t    data;
   mps_word_t    size;
   struct mycell *ref[2];
  } mycell;

/* we don't have a separate type for leaf nodes;
   instead the scanning function doesn't fix null refs

   the words after ref[1] are copied by mycopy,
   (so you can use them to store data) as long as copysurplus=1
*/

extern struct mps_fmt_A_s fmtA;

mycell *allocone(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size);

#endif
