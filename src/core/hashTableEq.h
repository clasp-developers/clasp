#ifndef	_core_HashTableEq_H
#define _core_HashTableEq_H

#include "core/foundation.h"
#include "core/object.h"
#include "hashTable.h"
#include "symbolTable.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(HashTableEq);
    class HashTableEq_O : public HashTable_O
    {
	LISP_BASE1(HashTable_O);
	LISP_CLASS(core,CorePkg,HashTableEq_O,"HashTableEq");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
	DEFAULT_CTOR_DTOR(HashTableEq_O);
    private: // instance variables here
    public:
	static HashTableEq_sp create(uint sz,  Number_sp rehashSize, double rehashThreshold);
        static HashTableEq_sp create_default();
    public:
	static int sxhash_eq(T_sp obj);
    public: // Functions here

	virtual T_sp hashTableTest() const { return cl::_sym_eq;};
	bool keyTest(T_sp entryKey, T_sp searchKey) const;

	int sxhashKey(T_sp key,int bound, bool willAddKey) const;


    };

}; /* core */
template<> struct gctools::GCInfo<core::HashTableEq_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::HashTableEq_O);

#endif /* _core_HashTableEq_H */


