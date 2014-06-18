#ifndef	_core_WeakHashTable_H
#define _core_WeakHashTable_H

#include "core/foundation.h"
#include "core/object.h"
#include "gctools/gcweakhash.h"
#include "hashTable.h"
#include "symbolTable.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(WeakHashTable);
    class WeakHashTable_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,WeakHashTable_O,"WeakHashTable");
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

        virtual int hashTableSize() const {SUBIMP();};
        virtual void put(int idx, T_sp key, T_sp val) {SUBIMP();};
        virtual T_mv get(int idx) {SUBIMP();};




	T_mv gethash(T_sp key, T_sp defaultValue = _Nil<T_O>() ) ;

	T_sp hash_table_setf_gethash(T_sp key, T_sp value);
        void setf_gethash(T_sp key, T_sp val) { this->hash_table_setf_gethash(key,val);};

	void clrhash();

	bool remhash(T_sp key);



    };

}; /* core */
template<> struct gctools::GCInfo<core::WeakHashTable_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::WeakHashTable_O);



#if 0

namespace core
{

    FORWARD(WeakKeyHashTable);
    c l a s s WeakKeyHashTable_O : public WeakHashTable_O
    {
	L I S P_BASE1(WeakHashTable_O);
	L I S P_CLASS(core,CorePkg,WeakKeyHashTable_O,"WeakKeyHashTable");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
    private: // instance variables here
        typedef gctools::tagged_backcastable_base_ptr<T_O> value_type;
        gctools::WeakHashTable<value_type
                               , value_type
                               , gctools::GCBucketAllocator<gctools::Buckets<value_type,value_type>,gctools::WeakLinks>
                               , gctools::GCBucketAllocator<gctools::Buckets<value_type,value_type>,gctools::StrongLinks> >
            _HashTable;
    public:
        WeakKeyHashTable_O() : _HashTable(4) {};
        WeakKeyHashTable_O(uint sz) : _HashTable(sz) {};
    public:
	static WeakKeyHashTable_sp make(uint sz ); // ,  Number_sp rehashSize, double rehashThreshold);
        static WeakKeyHashTable_sp create_default();
    public:
	static int sxhash_eq(T_sp obj);
    public: // Functions here

        virtual int hashTableSize() const;
        virtual void put(int idx, T_sp key, T_sp val);
        virtual T_mv get(int idx);

        void describe(); 
	virtual T_sp hashTableTest() const { return cl::_sym_eq;};
	bool keyTest(T_sp entryKey, T_sp searchKey) const;

	int sxhashKey(T_sp key,int bound, bool willAddKey) const;


    };
}; /* core */
template<> struct gctools::GCInfo<core::WeakKeyHashTable_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::WeakKeyHashTable_O);
#endif





#endif /* _core_WeakHashTable_H */


