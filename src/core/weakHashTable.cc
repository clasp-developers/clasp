#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "weakHashTable.h"
#include "wrappers.h"


namespace core {

    EXPOSE_CLASS(core,WeakHashTable_O);

        
    void WeakHashTable_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<WeakHashTable_O>()
	    ;
    }
    
    void WeakHashTable_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakHashTable,"","",_lisp)
	    ;
#endif
    }

#if 0
    EXPOSE_CLASS(core,WeakKeyHashTable_O);


    void WeakKeyHashTable_O::describe()
    {
        printf("WeakKeyHashTable   size: %zu\n", this->_HashTable.size());
        printf("   keys memory range:  %p  - %p \n", &this->_HashTable.keyAt(0).base_ref().px_ref(), &this->_HashTable.keyAt(this->_HashTable.size()).base_ref().px_ref());
        for ( int i(0); i<this->_HashTable.size(); ++i ) {
            value_type& key = this->_HashTable.keyAt(i);
            value_type& val = this->_HashTable.valueAt(i);
            stringstream sentry;
            sentry << i << "  key.px@" << (void*)(&key.base_ref().px_ref()) << "  ";
            if ( !key.base_ref() ) {
                sentry << "splatted";
            } else if ( key.base_ref().unboundp() ) {
                sentry << "unbound";
            } else if ( key.base_ref().nilp() ) {
                sentry << "nil";
            } else if ( key.base_ref().deletedp() ) {
                sentry << "deleted";
            } else {
                T_sp okey = key.backcast();
                sentry << _rep_(okey);
                sentry << "@" << (void*)(key.base_ref().px_ref());
                sentry << "   -->   " << _rep_(val.backcast());
            }
            printf("   %s\n", sentry.str().c_str() );
        }
    }


    int WeakKeyHashTable_O::hashTableSize() const
    {
        return this->_HashTable.size();
    }

    void WeakKeyHashTable_O::put(int idx, T_sp key, T_sp val)
    {
        this->_HashTable.set(idx,key,val);
    }

    /*! Return the key/value as three values
      (values key value invalid-key)
      invalid-key is one of nil, :unused :deleted :splatted 
      depending if the key is valid, unused, deleted or a weak link was splatted */
    T_mv WeakKeyHashTable_O::get(int idx)
    {
        T_sp key = this->_HashTable.keyAt(idx).backcast();
        T_sp val = this->_HashTable.valueAt(idx).backcast();
        if ( key.pointerp() ) {
            return Values(key,val,_Nil<T_O>());
        }
        T_sp keyInfo(_Nil<T_O>());
        if ( !key ) {
            keyInfo = kw::_sym_splatted;
        } else if ( key.unboundp()) {
            keyInfo = kw::_sym_unbound;
        } else if ( key.deletedp()) {
            keyInfo = kw::_sym_deleted;
        }
        return Values(_Nil<T_O>(), val, keyInfo );
    }

    SYMBOL_EXPORT_SC_(KeywordPkg,splatted);
    SYMBOL_EXPORT_SC_(KeywordPkg,unbound);
    SYMBOL_EXPORT_SC_(KeywordPkg,deleted);

    WeakKeyHashTable_sp WeakKeyHashTable_O::make(uint sz)
    {
        WeakKeyHashTable_sp ht = gctools::GCObjectAllocator<WeakKeyHashTable_O>::allocate(sz);
        return ht;
    }
    


    void WeakKeyHashTable_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<WeakKeyHashTable_O>()
            .def("hashTableSize",&WeakKeyHashTable_O::hashTableSize)
            .def("weakHashTableGet",&WeakKeyHashTable_O::get)
            .def("weakHashTablePut",&WeakKeyHashTable_O::put)
	    ;
        af_def(CorePkg,"makeWeakKeyHashTable",&WeakKeyHashTable_O::make);

    }
    
    void WeakKeyHashTable_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakKeyHashTable,"","",_lisp)
	    ;
#endif
    }

#endif

};

