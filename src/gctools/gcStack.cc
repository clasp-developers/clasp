/*
    File: gcStack.cc
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
namespace gctools {
namespace frame {

#ifdef USE_ALLOCA_FOR_FRAME
Frame::Frame(ElementType *buffer, size_t numArguments) : _frameBlock(buffer), _ElementCapacity(FrameElements(numArguments)), _ArrayLength(numArguments) {
  this->lowLevelElementRef(IdxNumElements) = reinterpret_cast<ElementType>(numArguments);
  for (size_t i(0), iEnd(numArguments); i < iEnd; ++i) {
    this->operator[](i) = gctools::tag_unbound<core::T_O *>();
  }
}
#else
Frame::Frame(size_t numArguments) : _ElementCapacity(FrameElements(numArguments)), _ArrayLength(numArguments) {
  size_t sz = FrameBytes(numArguments);
  this->_frameBlock = reinterpret_cast<ElementType *>(threadLocalStack()->pushFrameImpl(sz));
  //  printf("%s:%d Pushing frame@%p\n", __FILE__, __LINE__, this->_frameImpl);
  // I can't use a FIXNUM here - it has to be a raw size_t
  this->lowLevelElementRef(IdxNumElements) = reinterpret_cast<ElementType>(numArguments);
  for (size_t i(0), iEnd(numArguments); i < iEnd; ++i) {
    this->operator[](i) = gctools::tag_unbound<core::T_O *>();
  }
}
#endif
void Frame::dump() const {
  void *frameImplHeaderAddress = threadLocalStack()->frameImplHeaderAddress(this->_frameBlock);
  GCStack::frameType frameImplHeaderType = threadLocalStack()->frameImplHeaderType(this->_frameBlock);
  int frameImplHeaderSize = threadLocalStack()->frameImplHeaderSize(this->_frameBlock);
  printf("Frame info\n");
  printf("    Frame._ArrayLength = %d    Frame._ElementCapacity = %d\n", this->_ArrayLength, this->_ElementCapacity);
  printf("    Frame._frameImpl = %p\n", this->_frameBlock);
  printf("    frameImplHeaderAddress(Frame._frameImpl) = %p\n", frameImplHeaderAddress);
  void *frameImplEnd = (char *)frameImplHeaderAddress + frameImplHeaderSize;
  printf("    frameImplEnd                             = %p\n", frameImplEnd);
  printf("    frameImplHeaderType(Frame._frameImpl)    = %d\n", frameImplHeaderType);
  printf("    frameImplHeaderSize(Frame._frameImpl)    = %d\n", frameImplHeaderSize);
  int frameImplBodySize = threadLocalStack()->frameImplBodySize(this->_frameBlock);
  printf("    frameImplBodySize(Frame._frameImpl)      = %d\n", frameImplBodySize);
  int numEntries = frameImplBodySize / sizeof(ElementType);
  printf("    numEntries in Frame._frameImpl           = %d\n", numEntries);
  for (int i(0); i < numEntries; ++i) {
    ElementType val = this->lowLevelElementRef(i);
    stringstream desc;
    desc << "entry[" << i << "] ";
    if (i >= IdxRegisterSaveArea && i < IdxOverflowArgs) {
      desc << "reg" << (i - IdxRegisterSaveArea);
    };
    if (i >= IdxRegisterArgumentsStart && i < IdxOverflowArgs) {
      desc << " reg_arg" << (i - IdxRegisterArgumentsStart);
    }
    if (i >= IdxOverflowArgs) {
      desc << "  overflow[" << (i - IdxOverflowArgs) << "] ";
    }
    printf("%p %30s --> %p\n", &this->lowLevelElementRef(i), desc.str().c_str(), val);
    if ((char *)&this->lowLevelElementRef(i) >= frameImplEnd) {
      printf("PROBLEM!  This frame entry indexes past the end of the frameImpl\n");
    }
  }
}

Frame::~Frame() {
//  printf("%s:%d Popping frame@%p\n", __FILE__, __LINE__, this->_frameImpl);
#ifdef USE_ALLOCA_FOR_FRAME
// Nothing
#else
  threadLocalStack()->popFrameImpl(reinterpret_cast<void *>(this->_frameBlock));
#endif
}
};
};
