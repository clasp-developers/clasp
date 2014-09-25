/*
    File: lofmt.h
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
lofmt.h
   A format for the LO pool class. We have to allow for copying
   and forwarding, but scan should never be called.
*/

#ifndef lofmt_h
#define lofmt_h

#include "testlib.h"

extern int alloclocomments;
extern int allowlocopies;

/* the object format is visible so tests that want to
   can hack around with it
*/

#define MAXSIZE 10000

enum {LOpadsingle=(int) 0xBAD51497, LOpadmany=(int) 0xBAD3A41,
      LOheart=(int) 0x8EA62, LOdata=(int) 0x7EAFDA2A};

typedef union locell locell;

typedef int tag;

struct lopadsingle {tag tag;};

struct lopadmulti {tag tag; mps_addr_t next;};

struct loheart {tag tag; size_t size; mps_addr_t obj;};

struct lodata
{
 tag tag;
 size_t size;
 long int id;
 long int copycount;
 size_t len;
 char data[MAXSIZE];
};

union locell
{
 tag tag;
 struct lopadsingle padsingle;
 struct lopadmulti  padmulti;
 struct loheart     heart;
 struct lodata      data;
};

extern struct mps_fmt_A_s fmtLO;

locell *alloclo(mps_ap_t ap, size_t bytes);

long int getloid(locell *obj);
long int getlocopycount(locell *obj);
long int getlosize(locell *obj);

#endif
