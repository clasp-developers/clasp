/*
    File: microHeap.cc
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

#include <vector>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/microHeap.h>
#include <clasp/core/wrappers.h>

namespace core {

MicroHeapBlock::MicroHeapBlock() {
  LOG(BF("no arg ctor for MicroHeapBlock@%p") % this);
  this->_EntrySize = UndefinedUnsignedInt;
  this->_NextEntryIndex = UndefinedUnsignedInt;
  this->_Entries = NULL;
}

MicroHeapBlock::MicroHeapBlock(uint maxEntries, uint entrySize) {
  this->_EntrySize = entrySize;
  this->_MaxEntries = maxEntries;
  this->_NextEntryIndex = 0;
  this->_Entries = (byte *)(malloc(entrySize * maxEntries));
  HARD_ASSERT(this->_Entries != NULL);
}

MicroHeapBlock::~MicroHeapBlock() {
  if (this->_Entries != NULL) {
    free(this->_Entries);
  }
}

void *MicroHeapBlock::rawGetEntry(uint i) {
  HARD_ASSERT(i < this->_MaxEntries);
  byte *bp = this->_Entries + (i * this->_EntrySize);
  return (void *)(bp);
}

bool MicroHeapBlock::canAllocateNewEntry() {
  //    LOG(BF("Test if can allocate new entry, this->_NextEntryIndex(%u)<this->_MaxEntries(%u)") % this->_NextEntryIndex % this->_MaxEntries  );
  if (this->_NextEntryIndex < this->_MaxEntries) {
    //	LOG(BF("true") );
    return true;
  }
  //    LOG(BF("False") );
  return false;
}

void *MicroHeapBlock::rawNewEntry(uint &idx) {
  HARD_ASSERT(this->canAllocateNewEntry());
  idx = this->_NextEntryIndex;
  byte *p = this->_Entries + (this->_EntrySize * this->_NextEntryIndex);
  this->_NextEntryIndex++;
  //    LOG(BF("Returning rawNewEntry this->_NextEntryIndex is now %u") % this->_NextEntryIndex );
  return (void *)(p);
}

MicroHeap_sp MicroHeap_O::create(Lisp_sp e, uint maxEntries, uint entrySize) {
  _G();
  GC_ALLOCATE(MicroHeap_O, h);
  h->setEntrySize(entrySize);
  h->setMaxEntries(maxEntries);
  h->createNewBlock();
  return h;
}

MicroHeap_O::MicroHeap_O() : Base() {
  this->_Blocks.clear();
}

MicroHeap_O::~MicroHeap_O() {
  // do nothing dtors called automatically
  for (vector<MicroHeapBlock *>::iterator it = this->_Blocks.begin();
       it != this->_Blocks.end(); it++) {
    delete (*it);
  }
  this->_Blocks.clear();
}

void MicroHeap_O::initialize() {
  this->Base::initialize();
  this->_Blocks.clear();
  this->_EntrySize = UndefinedUnsignedInt;
  this->_MaxEntriesPerBlock = UndefinedUnsignedInt;
}

uint MicroHeap_O::numberOfEntries() {
  if (this->_Blocks.size() == 0) {
    return 0;
  }
  uint num = (this->_Blocks.size() - 1) * this->_MaxEntriesPerBlock;
  num += this->_Blocks.back()->numberOfEntries();
  return num;
}

void *MicroHeap_O::rawGetEntry(uint i) {
  _G();
  ASSERT(this->_Blocks.size() > 0);
  uint blocki = i / (this->_MaxEntriesPerBlock);
  uint blocko = i % (this->_MaxEntriesPerBlock);
  MicroHeapBlock *Pblock = this->_Blocks[blocki];
  return Pblock->rawGetEntry(blocko);
}

void MicroHeap_O::createNewBlock() {
  _G();
  ASSERTP(this->_EntrySize != UndefinedUnsignedInt, "You must set EntrySize first");
  MicroHeapBlock *block = new MicroHeapBlock(this->_MaxEntriesPerBlock, this->_EntrySize);
  this->_Blocks.push_back(block);
}

void *MicroHeap_O::rawNewEntry(uint &i) {
  _G();
  if (this->_Blocks.size() == 0) {
    this->createNewBlock();
  }
  ASSERT_gt(this->_Blocks.size(), 0);
  MicroHeapBlock *PBlock = this->_Blocks.back();
  if (!PBlock->canAllocateNewEntry()) {
    this->createNewBlock();
    PBlock = this->_Blocks.back();
  }
  uint offset;
  i = (this->_Blocks.size() - 1) * this->_MaxEntriesPerBlock;
  void *ptr = PBlock->rawNewEntry(offset);
  i += offset;
  return ptr;
}

REGISTER_CLASS(core, MicroHeap_O);
};
