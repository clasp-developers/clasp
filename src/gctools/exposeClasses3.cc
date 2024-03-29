
/*
    File: exposeClasses1.cc
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

#ifndef SCRAPING // #endif at bottom

#include <clasp/gctools/exposeCommon.h>

#include <clasp/core/wrappers.h>
#include <clasp/core/external_wrappers.h>

#define BATCH3

#ifndef SCRAPING
// include INIT_CLASSES_INC_H despite USE_PRECISE_GC
#define EXPOSE_METHODS
#include INIT_CLASSES_INC_H
#undef EXPOSE_METHODS
#endif

void initialize_exposeClasses3() {
#ifndef SCRAPING
  // include INIT_CLASSES_INC_H despite USE_PRECISE_GC
#define EXPOSE_CLASSES_AND_METHODS
#include INIT_CLASSES_INC_H
#undef EXPOSE_CLASSES_AND_METHODS
#endif
}

#endif // #ifndef SCRAPING at top
