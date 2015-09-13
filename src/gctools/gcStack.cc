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
Frame::Frame(size_t numArguments,core::T_sp parent) : _Capacity(numArguments), _Length(numArguments) {
  size_t sz = FrameSize(numArguments);
  this->_frameImpl = reinterpret_cast<ElementType*>(threadLocalStack()->pushFrameImpl(sz));
//  printf("%s:%d Pushing frame@%p\n", __FILE__, __LINE__, this->_frameImpl);
  this->_frameImpl[IdxNumElements] = reinterpret_cast<core::T_O*>(numArguments);
  for (size_t i(IdxValuesArray), iEnd(IdxValuesArray + numArguments); i < iEnd; ++i) {
    this->_frameImpl[i] = gctools::tag_unbound<core::T_O *>();
  }
}

void Frame::dump() const {
  void* frameImplHeaderAddress = threadLocalStack()->frameImplHeaderAddress(this->_frameImpl);
  GCStack::frameType frameImplHeaderType = threadLocalStack()->frameImplHeaderType(this->_frameImpl);
  int frameImplHeaderSize = threadLocalStack()->frameImplHeaderSize(this->_frameImpl);
  printf("Frame info\n");
  printf("    Frame._Length = %d    Frame._Capacity = %d\n", this->_Length, this->_Capacity );
  printf("    Frame._frameImpl = %p\n", this->_frameImpl );
  printf("    frameImplHeaderAddress(Frame._frameImpl) = %p\n", frameImplHeaderAddress );
  void* frameImplEnd = (char*)frameImplHeaderAddress + frameImplHeaderSize;
  printf("    frameImplEnd                             = %p\n", frameImplEnd );
  printf("    frameImplHeaderType(Frame._frameImpl)    = %d\n", frameImplHeaderType );
  printf("    frameImplHeaderSize(Frame._frameImpl)    = %d\n", frameImplHeaderSize );
  int frameImplBodySize = threadLocalStack()->frameImplBodySize(this->_frameImpl);
  printf("    frameImplBodySize(Frame._frameImpl)      = %d\n", frameImplBodySize );
  int numEntries = frameImplBodySize/sizeof(ElementType);
  printf("    numEntries in Frame._frameImpl           = %d\n", numEntries );
  ElementType* cur = this->_frameImpl;
  for ( int i(0); i<numEntries; ++i ) {
    ElementType val = cur[i];
    stringstream desc;
    desc << "entry[" << i << "] ";
    if ( i >= IdxRegisterSaveArea && i < IdxOverflowArgs ) {
      desc << "reg" << (i-IdxRegisterSaveArea);
    };
    if ( i >= IdxRegisterArgumentsStart && i < IdxOverflowArgs ) {
      desc << " reg_arg" << (i - IdxRegisterArgumentsStart);
    }
    if ( i >= IdxOverflowArgs ) {
      desc << "  overflow[" << (i-IdxOverflowArgs) << "] ";
    }
    printf("%p %30s --> %p\n", &cur[i], desc.str().c_str(), val);
    if ((char*)&cur[i] >= frameImplEnd ) {
      printf("PROBLEM!  This frame entry indexes past the end of the frameImpl\n");
    }
  }
}

Frame::~Frame() {
//  printf("%s:%d Popping frame@%p\n", __FILE__, __LINE__, this->_frameImpl);
  threadLocalStack()->popFrameImpl(reinterpret_cast<void*>(this->_frameImpl));
}

};
};
