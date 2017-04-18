/*
    File: queue.fwd.h
*/

/*
Copyright (c) 2017, Christian E. Schafmeister
Copyright (c) 2017, Frank Goenninger, Goenninger B&T UG, Germany

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

#if !defined( __CLASP_MP_QUEUE_FWD_H__ )
#define __CLASP_MP_QEUEUE_FWD_H__

#include "clasp/core/mpPackage.fwd.h"
#include "clasp/ext/concurrentqueue/concurrentqueue.h"
#include "clasp/ext/concurrentqueue/blockingconcurrentqueue.h"

namespace mp {

  class Queue_O;
  typedef gctools::smart_ptr<Queue_O> Queue_sp;

};

#endif
