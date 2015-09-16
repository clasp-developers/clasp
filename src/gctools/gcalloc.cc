/*
    File: gcalloc.cc
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
#include <clasp/core/foundation.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/gcalloc.h>

#ifdef USE_MPS
// header for SNC pool
#include "mps/code/mpscsnc.h"
#endif

namespace gctools {


void* GCStack::pushFrameImpl(size_t frameSize) {
  frameSize = STACK_ALIGN_UP(frameSize);
  size_t headerAndFrameSize = FRAME_HEADER_SIZE + frameSize;
#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
  uintptr_t* headerAndFrame = (uintptr_t*)this->_StackTop;
  this->_StackTop = (uintptr_t*)((char*)this->_StackTop+headerAndFrameSize);
#else
  uintptr_t* headerAndFrame = (uintptr_t*)GC_MALLOC(headerAndFrameSize);
#endif
  FRAME_HEADER_TYPE_FIELD(headerAndFrame) = frame_t;
  FRAME_HEADER_SIZE_FIELD(headerAndFrame) = headerAndFrameSize;
  void* frameStart = headerAndFrame + 1; // skip uintptr_t header
  this->_TotalSize += headerAndFrameSize;
  goto DONE;
#endif
#ifdef USE_MPS
  mps_frame_t frame_o;
  STACK_TELEMETRY7(telemetry::label_stack_push_prepare,
                   this->_AllocationPoint,
                   this->_AllocationPoint->init,
                   this->_AllocationPoint->alloc,
                   this->_AllocationPoint->limit,
                   this->_AllocationPoint->_frameptr,
                   this->_AllocationPoint->_enabled,
                   this->_AllocationPoint->_lwpoppending);
#if 0
  if ( ((this->_TotalSize + headerAndFrameSize)%4096) == 0 ) {
    headerAndFrameSize = STACK_ALIGN_UP(headerAndFrameSize+1);
//            printf("%s:%d About to mps_ap_frame_push frame where total stack size would be 4096 - increasing the allocation request to bump the total stack size to %zu bytes\n", __FILE__, __LINE__, (this->_TotalSize+headerAndFrameSize) );
  }
#endif
#if 0
  if ( this->_TotalSize!=0 && this->_TotalSize%4096 == 0 ) {
    printf("%s:%d Despite my best effort above... about to mps_ap_frame_push frame where total stack size is a multiple of 4096\n", __FILE__, __LINE__ );
  }
#endif
  mps_res_t respush = mps_ap_frame_push(&frame_o,this->_AllocationPoint);
  if (respush != MPS_RES_OK) {
    printf("%s:%d There was a problem with mps_ap_frame_push result=%d\n", __FILE__, __LINE__, respush);
    abort();
  }
  this->frames.push_back(frame_o);
  STACK_TELEMETRY3(telemetry::label_stack_push,this->_AllocationPoint,frame_o,this->frames.size());
  if (respush != MPS_RES_OK) {
    THROW_HARD_ERROR(BF("Could not mps_ap_frame_push"));
  }
  mps_addr_t p;
  uintptr_t* allocP;
  do {
    mps_res_t res = mps_reserve(&p, this->_AllocationPoint, headerAndFrameSize);
    if (res != MPS_RES_OK) {
      THROW_HARD_ERROR(BF("Out of memory in GCStack::allocateFrame"));
    }
    allocP = reinterpret_cast<uintptr_t*>(p);
    memset(allocP,0,headerAndFrameSize);
  } while (!mps_commit(this->_AllocationPoint, p, headerAndFrameSize)); /* see note 2 */
  STACK_TELEMETRY2(telemetry::label_stack_allocate,allocP,headerAndFrameSize);
  DEBUG_MPS_UNDERSCANNING_TESTS();
  FRAME_HEADER_TYPE_FIELD(allocP) = frame_t;
  FRAME_HEADER_SIZE_FIELD(allocP) = headerAndFrameSize;
  void* frameStart = FRAME_START(allocP); // skip uintptr_t header
  this->_TotalSize += headerAndFrameSize;
  goto DONE;
#endif
 DONE:
#if defined(BEOHM_ONE_BIG_STACK) && defined(USE_BOEHM) && defined(DEBUG_BOEHM_STACK)
  size_t calcSize = (char*)this->_StackTop - (char*)this->_StackBottom;
  if ( calcSize != this->_TotalSize ) {
    THROW_HARD_ERROR(BF("The side-stack has gotten out of whack!  this->_TotalSize = %u  calcSize = %u\n") % this->_TotalSize % calcSize );
  }
#endif
  if ( this->_TotalSize > this->_MaxSize ) {
    this->_MaxSize = this->_TotalSize;
  }
  return frameStart;
}


};
