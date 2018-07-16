/*
    File: multiStringBuffer.cc
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
#define DEBUG_LEVEL_NONE

#include <string.h>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/multiStringBuffer.h>
#include <clasp/core/lisp.h>
#include <clasp/core/bformat.h>
#include <clasp/core/environment.h>
#
// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

MultiStringBlock::MultiStringBlock(uint blockId) {
  this->_BlockId = blockId;
  LOG(BF("Allocating MultiStringBlock buffer with size(%d)") % MultiStringBlockSize);
  this->_BlockStart = (char *)malloc(MultiStringBlockSize);
  LOG(BF("Allocating MultiStringBlock@%p") % this->_BlockStart);
  if (this->_BlockStart == NULL) {
    THROW_HARD_ERROR(BF("Could not allocate MultiStringBlock - ran out of memory"));
  }
  this->_NextStringIndex = 0;
  this->_NumberOfStrings = 0;
}

MultiStringBlock::~MultiStringBlock() {
  LOG(BF("About to free MultiStringBlock@%p") % this->_BlockStart);
  free(this->_BlockStart);
  this->_BlockStart = NULL;
}

uint MultiStringBlock::addCharacters(const char *str) {
  uint len = strlen(str);
  uint nextStringIndex = this->_NextStringIndex + len + 1; // \0 terminated string
  if (nextStringIndex > MultiStringBlockSize) {
    LOG(BF("In MultiStringBlock::addCharacters>>Could not add string<%s> nextStringIndex(%d) MultiStringBlockSize(%d)") % str % nextStringIndex % MultiStringBlockSize);
    return UndefinedUnsignedInt;
  }
  uint index = this->_NextStringIndex;
  strcpy(this->_BlockStart + this->_NextStringIndex, str);
  this->_NextStringIndex = nextStringIndex;
  return index;
}

uint MultiStringBlock::addNumberOfCharacters(const char *str, uint num) {
  uint len = num;
  uint nextStringIndex = this->_NextStringIndex + len + 1; // \0 terminated string
  if (nextStringIndex > MultiStringBlockSize) {
    return UndefinedUnsignedInt;
  }
  uint index = this->_NextStringIndex;
  strncpy(this->_BlockStart + this->_NextStringIndex, str, len);
  this->_BlockStart[nextStringIndex - 1] = '\0';
  this->_NextStringIndex = nextStringIndex;
  return index;
}

const char *MultiStringBlock::getString(uint localIndex) {
  HARD_ASSERT(localIndex < MultiStringBlockSize);
  return this->_BlockStart + localIndex;
}

void MultiStringBlock::dump(uint startIndex, std::ostream &out) {
  const char *cur = this->_BlockStart;
  while (cur - this->_BlockStart < this->_NextStringIndex) {
    out << "string@";
    out << this->_BlockId;
    out << ":";
    out << (cur - this->_BlockStart + startIndex);
    out << "\"";
    out << cur;
    out << "\"";
    out << std::endl;
    cur += strlen(cur) + 1;
  }
}




void MultiStringBuffer_O::initialize() {
  this->Base::initialize();
  this->_Blocks.clear();
}

#if defined(XML_ARCHIVE)
void MultiStringBuffer_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

uint MultiStringBuffer_O::addNumberOfCharacters(const char *str, uint num) {
  if (this->_Blocks.size() == 0) {
    MultiStringBlock *block = new MultiStringBlock(0);
    this->_Blocks.push_back(block);
  }
  MultiStringBlock *curBlock = (this->_Blocks.back());
  uint index;
  index = curBlock->addNumberOfCharacters(str, num);
  if (index == UndefinedUnsignedInt) {
    MultiStringBlock *newBlock = new MultiStringBlock(this->_Blocks.size());
    this->_Blocks.push_back(newBlock);
    curBlock = (this->_Blocks.back());
    index = curBlock->addCharacters(str);
    if (index == UndefinedUnsignedInt) {
      stringstream serr;
      serr << "This should NEVER happen, a string could not " << std::endl;
      serr << " be added to a brand new MultiStringBlock." << std::endl;
      serr << " MultiStringBlockSize needs to be increased to " << std::endl;
      serr << " handle strings larger than " << num << " bytes." << std::endl;
      serr << " First min(512,num) characters of string are: [";
      uint mn = 512;
      if (num < mn)
        mn = num;
      for (uint ii = 0; ii < mn; ii++) {
        serr << str[ii];
        ;
      }
      serr << "]" << std::endl;

      THROW_HARD_ERROR(BF("%s") % serr.str());
    }
  }
  uint blockIndex = MultiStringBlockSize * (this->_Blocks.size() - 1);
  return blockIndex + index;
}

uint MultiStringBuffer_O::addCharacters(const char *str) {
  LOG(BF("Adding characters(%s)") % str);
  if (this->_Blocks.size() == 0) {
    MultiStringBlock *block = new MultiStringBlock(0);
    this->_Blocks.push_back(block);
  }
  MultiStringBlock *curBlock = (this->_Blocks.back());
  uint index;
  index = curBlock->addCharacters(str);
  if (index == UndefinedUnsignedInt) {
    MultiStringBlock *newBlock = new MultiStringBlock(this->_Blocks.size());
    this->_Blocks.push_back(newBlock);
    curBlock = (this->_Blocks.back());
    index = curBlock->addCharacters(str);
    if (index == UndefinedUnsignedInt) {
      stringstream serr;
      serr << "This should NEVER happen, a string could not " << std::endl;
      serr << " be added to a brand new MultiStringBlock." << std::endl;
      serr << " MultiStringBlockSize needs to be increased to " << std::endl;
      serr << " handle strings larger than " << strlen(str) << " bytes." << std::endl;
      serr << " First 512 characters of string are: [";
      for (uint ii = 0; ii < 512; ii++) {
        serr << str[ii];
        ;
      }
      serr << "]" << std::endl;

      SIMPLE_ERROR(BF(serr.str()));
    }
  }
  uint blockIndex = MultiStringBlockSize * (this->_Blocks.size() - 1);
  return blockIndex + index;
}

CL_LISPIFY_NAME("addString");
CL_DEFMETHOD uint MultiStringBuffer_O::addString(const string &str) {
  return this->addCharacters(str.c_str());
}

const char *MultiStringBuffer_O::getCharacters(uint index) {
  _OF();
  if (index == UndefinedUnsignedInt)
    return "";
  uint blockIndex = index >> MultiStringBlockPowerOf2;
  uint offset = index & MultiStringBlockLocalMask;
  ASSERT_lt(blockIndex, this->_Blocks.size());
  MultiStringBlock &block = *(this->_Blocks[blockIndex]);
  ASSERT_lt(offset, MultiStringBlockSize);
  return block.getString(offset);
}

CL_LISPIFY_NAME("getString");
CL_DEFMETHOD string MultiStringBuffer_O::getString(uint index) {
  const char *chars = this->getCharacters(index);
  string result = chars;
  return result;
}

void MultiStringBuffer_O::dumpToStream(std::ostream &ss) {
  ss << "MultiStringBuffer dump" << std::endl;
  uint startIndex = 0;
  for (vector<MultiStringBlock *>::iterator it = this->_Blocks.begin();
       it < this->_Blocks.end(); it++) {
    (*it)->dump(startIndex, ss);
    startIndex += MultiStringBlockSize;
  }
}

CL_LISPIFY_NAME("dump");
CL_DEFMETHOD void MultiStringBuffer_O::dump() {
  stringstream ss;
  this->dumpToStream(ss);
  printf("%s\n", ss.str().c_str());
}

LongLongInt MultiStringBuffer_O::describeMemoryUsage() {
  LongLongInt total = 0;
  uint headerMemory = (sizeof(MultiStringBlock) + sizeof(MultiStringBlock *)) * this->_Blocks.size();
  BFORMAT_T(BF("MultiStringBlock storage = %d bytes") % headerMemory);
  total += headerMemory;
  LongLongInt storage = MultiStringBlockSize * this->_Blocks.size();
  BFORMAT_T(BF("String storage = %d bytes") % storage);
  total += storage;
  return total;
}


};
