/*
    File: microHeap.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#ifndef	MicroHeap_H
#define	MicroHeap_H

#include "foundation.h"
#include "object.h"



namespace core {

typedef	unsigned char byte;

class MicroHeapBlock
{
private:
    byte*	_Entries;
    uint	_EntrySize;
    uint	_NextEntryIndex;
    uint	_MaxEntries;
public:
    MicroHeapBlock();
    MicroHeapBlock(uint maxEntries, uint entrySize);
    virtual ~MicroHeapBlock();

    uint numberOfEntries() { return this->_NextEntryIndex; };

    template <typename T>
    T* newEntry()
    {
	uint i;
	return (T*)(this->rawNewEntry(i));
    }

    void* rawNewEntry(uint& i);
    void* rawGetEntry(uint i);

    bool canAllocateNewEntry();
};



SMART(MicroHeap );
class MicroHeap_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,MicroHeap_O,"MicroHeap");

    public:
void initialize();
private:
    uint	_EntrySize;
    uint	_MaxEntriesPerBlock;
    vector<MicroHeapBlock*>	_Blocks;
public:
    static MicroHeap_sp create(Lisp_sp e,uint maxEntriesPerBlock, uint entrySize );
public:


    void setMaxEntries(uint m) { this->_MaxEntriesPerBlock = m;};
    void setEntrySize(uint m) { this->_EntrySize = m;};
    
    uint numberOfEntries();
    
    template <typename T> T* getEntry(uint i) 
    {
	return (T*)(rawGetEntry(i));
    }

    void* rawGetEntry(uint i); 
  
    template <typename T> T* newEntry()
    {_OF();
	ASSERTP(sizeof(T)==this->_EntrySize,"Mismatch in size of type for MicroHeap");
	uint t;
	return (T*)(this->rawNewEntry(t));
    }

    void* rawNewEntry(uint& i);


    void createNewBlock();
    explicit MicroHeap_O();
    virtual ~MicroHeap_O();
};

};
TRANSLATE(core::MicroHeap_O);
#endif
