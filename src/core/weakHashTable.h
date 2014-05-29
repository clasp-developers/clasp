#ifndef	_core_WeakHashTable_H
#define _core_WeakHashTable_H

#include "core/foundation.h"
#include "core/object.h"
#include "hashTable.h"
#include "symbolTable.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(WeakHashTable);
    c l a ss WeakHashTable_O : public T_O
    {
	L I S P_BASE1(T_O);
	L I S P_CLASS(core,CorePkg,WeakHashTable_O,"WeakHashTable");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
	DEFAULT_CTOR_DTOR(WeakHashTable_O);
    private: // instance variables here
    public:
	static int sxhash_eq(T_sp obj);
    public: // Functions here

	virtual T_sp hashTableTest() const { return cl::_sym_eq;};
	bool keyTest(T_sp entryKey, T_sp searchKey) const;

	int sxhashKey(T_sp key,int bound, bool willAddKey) const;


    };

}; /* core */
template<> struct gctools::GCAllocatorInfo<core::WeakHashTable_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::WeakHashTable_O);




namespace core
{

    FORWARD(WeakKeyHashTable);
    c l a ss WeakKeyHashTable_O : public WeakHashTable_O
    {
	L I S P_BASE1(WeakHashTable_O);
	L I S P_CLASS(core,CorePkg,WeakKeyHashTable_O,"WeakKeyHashTable");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
	DEFAULT_CTOR_DTOR(WeakKeyHashTable_O);
    private: // instance variables here
        gctools::WeakHashTable<T_sp, T_sp, gctools::GCBucketAllocator<T_sp,gctools::WeakLinks>, gctools::GCStrongBucketAllocator<T_sp,gctools::StrongLinks> >       _HashTable;
    public:
	static WeakKeyHashTable_sp create(uint sz,  Number_sp rehashSize, double rehashThreshold);
        static WeakKeyHashTable_sp create_default();
    public:
	static int sxhash_eq(T_sp obj);
    public: // Functions here

	virtual T_sp hashTableTest() const { return cl::_sym_eq;};
	bool keyTest(T_sp entryKey, T_sp searchKey) const;

	int sxhashKey(T_sp key,int bound, bool willAddKey) const;


    };

}; /* core */
template<> struct gctools::GCAllocatorInfo<core::WeakKeyHashTable_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::WeakKeyHashTable_O);






#endif /* _core_WeakHashTable_H */


