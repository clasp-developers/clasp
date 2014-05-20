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
