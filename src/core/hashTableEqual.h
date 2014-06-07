#ifndef	_core_HashTableEqual_H
#define _core_HashTableEqual_H

#include "core/foundation.h"
#include "core/object.h"
#include "core/symbolTable.h"
#include "hashTable.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(HashTableEqual);
    class HashTableEqual_O : public HashTable_O
    {
	LISP_BASE1(HashTable_O);
	LISP_CLASS(core,CorePkg,HashTableEqual_O,"HashTableEqual");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
	DEFAULT_CTOR_DTOR(HashTableEqual_O);
    public:
	static HashTableEqual_sp create( uint sz,  Number_sp rehashSize, double rehashThreshold);
	static HashTableEqual_sp create_default();
    public:
//	static int sxhash_equal(T_sp obj);
    public: // Functions here

	virtual T_sp hashTableTest() const { return cl::_sym_equal;};

	bool keyTest(T_sp entryKey, T_sp searchKey) const;

	int sxhashKey(T_sp key,int bound, bool willAddKey) const;


    };

}; /* core */
template<> struct gctools::GCInfo<core::HashTableEqual_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::HashTableEqual_O);

#endif /* _core_HashTableEqual_H */


