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
#include <clasp/mps/code/mpscsnc.h>
#endif

namespace gctools {

#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
void GCStack::growStack() {
  size_t oldSize = (this->_StackLimit - this->_StackBottom);
  size_t newSize = oldSize * 2;
  uintptr_t *newStack = (uintptr_t *)GC_MALLOC(newSize);
  memcpy(newStack, this->_StackBottom, oldSize);    // Copy old to new
  memset(newStack + oldSize, 0, newSize - oldSize); // Zero end of new part
  long long int stack_top_offset = (char *)this->_StackCur - (char *)this->_StackBottom;
  uintptr_t *oldStack = this->_StackBottom;
  this->_StackBottom = newStack;
  this->_StackLimit = (uintptr_t *)((char *)newStack + newSize);
  this->_StackMiddleOffset = (newSize / 2);
  this->_StackCur = (uintptr_t *)((char *)newStack + stack_top_offset);
  GCTOOLS_ASSERT(this->_StackBottom <= this->_StackCur && this->_StackCur < this->_StackLimit);
  GC_FREE(oldStack);
}

void GCStack::shrinkStack() {
  GCTOOLS_ASSERT((this->_StackCur - this->_StackBottom) < this->_StackMiddleOffset);
  size_t oldSize = (this->_StackLimit - this->_StackBottom);
  size_t newSize = oldSize / 2;
  uintptr_t *newStack = (uintptr_t *)GC_MALLOC(newSize);
  memcpy(newStack, this->_StackBottom, newSize); // Copy old to new
  long long int stack_top_offset = (char *)this->_StackCur - (char *)this->_StackBottom;
  uintptr_t *oldStack = this->_StackBottom;
  this->_StackBottom = newStack;
  this->_StackLimit = (uintptr_t *)((char *)newStack + newSize);
  this->_StackMiddleOffset = (newSize / 2);
  this->_StackCur = (uintptr_t *)((char *)newStack + stack_top_offset);
  GCTOOLS_ASSERT(this->_StackBottom <= this->_StackCur && this->_StackCur < this->_StackLimit);
  GC_FREE(oldStack);
}
#endif
#endif

void *GCStack::pushFrameImpl(size_t frameSize) {
  frameSize = STACK_ALIGN_UP(frameSize);
  size_t headerAndFrameSize = FRAME_HEADER_SIZE + frameSize;
#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
  uintptr_t *headerAndFrame = (uintptr_t *)this->_StackCur;
  uintptr_t *stackCur = (uintptr_t *)((char *)this->_StackCur + headerAndFrameSize);
  if (stackCur > this->_StackLimit)
    this->growStack();
  this->_StackCur = (uintptr_t *)((char *)this->_StackCur + headerAndFrameSize);
  GCTOOLS_ASSERT(this->_StackBottom <= this->_StackCur && this->_StackCur < this->_StackLimit);
#else
  uintptr_t *headerAndFrame = (uintptr_t *)GC_MALLOC(headerAndFrameSize);
#endif
  FRAME_HEADER_TYPE_FIELD(headerAndFrame) = frame_t;
  FRAME_HEADER_SIZE_FIELD(headerAndFrame) = headerAndFrameSize;
  void *frameStart = headerAndFrame + 1; // skip uintptr_t header
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
  mps_res_t respush = mps_ap_frame_push(&frame_o, this->_AllocationPoint);
  if (respush != MPS_RES_OK) {
    printf("%s:%d There was a problem with mps_ap_frame_push result=%d\n", __FILE__, __LINE__, respush);
    abort();
  }
  this->frames.push_back(frame_o);
  STACK_TELEMETRY3(telemetry::label_stack_push, this->_AllocationPoint, frame_o, this->frames.size());
  if (respush != MPS_RES_OK) {
    THROW_HARD_ERROR(BF("Could not mps_ap_frame_push"));
  }
  mps_addr_t p;
  uintptr_t *allocP;
  do {
    mps_res_t res = mps_reserve(&p, this->_AllocationPoint, headerAndFrameSize);
    if (res != MPS_RES_OK) {
      THROW_HARD_ERROR(BF("Out of memory in GCStack::allocateFrame"));
    }
    allocP = reinterpret_cast<uintptr_t *>(p);
    memset(allocP, 0, headerAndFrameSize);
  } while (!mps_commit(this->_AllocationPoint, p, headerAndFrameSize)); /* see note 2 */
  STACK_TELEMETRY2(telemetry::label_stack_allocate, allocP, headerAndFrameSize);
  DEBUG_MPS_UNDERSCANNING_TESTS();
  FRAME_HEADER_TYPE_FIELD(allocP) = frame_t;
  FRAME_HEADER_SIZE_FIELD(allocP) = headerAndFrameSize;
  void *frameStart = FRAME_START(allocP); // skip uintptr_t header
  this->_TotalSize += headerAndFrameSize;
  goto DONE;
#endif
DONE:
#if defined(BEOHM_ONE_BIG_STACK) && defined(USE_BOEHM) && defined(DEBUG_BOEHM_STACK)
  GCTOOLS_ASSERT(this->_StackBottom <= this->_StackCur && this->_StackCur < this->_StackLimit);
  size_t calcSize = (char *)this->_StackCur - (char *)this->_StackBottom;
  if (calcSize != this->_TotalSize) {
    THROW_HARD_ERROR(BF("The side-stack has gotten out of whack!  this->_TotalSize = %u  calcSize = %u\n") % this->_TotalSize % calcSize);
  }
#endif
  if (this->_TotalSize > this->_MaxSize) {
    this->_MaxSize = this->_TotalSize;
  }
  return frameStart;
}
};
