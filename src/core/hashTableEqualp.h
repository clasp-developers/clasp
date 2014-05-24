#ifndef	_core_HashTableEqualp_H
#define _core_HashTableEqualp_H

#include "core/foundation.h"
#include "core/object.h"
#include "hashTable.h"
#include "symbolTable.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(HashTableEqualp);
    class HashTableEqualp_O : public HashTable_O
    {
	LISP_BASE1(HashTable_O);
	LISP_CLASS(core,CorePkg,HashTableEqualp_O,"HashTableEqualp");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
	DEFAULT_CTOR_DTOR(HashTableEqualp_O);
    public:
	static HashTableEqualp_sp create( uint sz,  Number_sp rehashSize, double rehashThreshold);
    public:
//	static int sxhash_equalp(T_sp obj);
    public: // Functions here

	virtual T_sp hashTableTest() const { return cl::_sym_equalp;};

	bool keyTest(T_sp entryKey, T_sp searchKey) const;

	int sxhashKey(T_sp key,int bound) const;


    };

}; /* core */
template<> struct gctools::GCAllocatorInfo<core::HashTableEqualp_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::HashTableEqualp_O);

#endif /* _core_HashTableEqualp_H */


