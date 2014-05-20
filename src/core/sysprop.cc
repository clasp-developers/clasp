

#include "foundation.h"
#include "hashTableEql.h"
#include "multipleValues.h"
#include "symbol.h"
#include "sysprop.h"
#include "wrappers.h"




namespace core
{

    
    
#define ARGS_af_put_sysprop "(key area value)"
#define DECL_af_put_sysprop ""
#define DOCS_af_put_sysprop "put_sysprop - returns value"
    T_mv af_put_sysprop(T_sp key, T_sp area, T_sp value)
    {_G();
	ASSERT(_lisp->_Roots._SystemProperties);
	if ( _lisp->_Roots._SystemProperties.unboundp() )
	{
	    _lisp->_Roots._SystemProperties = HashTableEql_O::create_default();
	}
	bool foundHashTable = false;
	HashTable_sp area_hash_table;
	const T_mv& values = _lisp->_Roots._SystemProperties->gethash(area,_Nil<T_O>());
	area_hash_table = values.as<HashTable_O>();
	foundHashTable = values.valueGet(1).as<T_O>().isTrue();
	T_sp retval;
	if ( foundHashTable )
	{
	    retval = area_hash_table->hash_table_setf_gethash(key,value);
	} else
	{
	    HashTable_sp new_hash_table = HashTableEql_O::create_default();
	    new_hash_table->hash_table_setf_gethash(key,value);
	    retval = _lisp->_Roots._SystemProperties->hash_table_setf_gethash(area,new_hash_table);
	}
	return(Values(retval));
    }




    
#define ARGS_af_get_sysprop "(key area)"
#define DECL_af_get_sysprop ""
#define DOCS_af_get_sysprop "get_sysprop - returns (values val foundp)"
    T_mv af_get_sysprop(T_sp key, T_sp area)
    {_G();
	if ( _lisp->_Roots._SystemProperties.pointerp() ) 
	{
	    T_mv values = _lisp->_Roots._SystemProperties->gethash(area,_Nil<T_O>());
	    HashTable_sp hashTable = values.as<HashTable_O>();
	    bool foundHashTable = values.valueGet(1).as<T_O>().isTrue();
	    if ( foundHashTable )
	    {
		return hashTable->gethash(key,_Nil<T_O>());
	    }
	}
	return(Values(_Nil<T_O>(),_Nil<T_O>()) );
    }



    
    
#define ARGS_af_rem_sysprop "(key area)"
#define DECL_af_rem_sysprop ""
#define DOCS_af_rem_sysprop "rem_sysprop"
    T_sp af_rem_sysprop(T_sp key, T_sp area)
    {_G();
	T_mv mv_values = _lisp->_Roots._SystemProperties->gethash(area,_Nil<T_O>());
	HashTable_sp hashTable = mv_values.as<HashTable_O>();
	bool foundHashTable = mv_values.valueGet(1).as<T_O>().isTrue();
	if (foundHashTable) {
	    bool found = hashTable->remhash(key);
	    return _lisp->_boolean(found);
	}
	return _Nil<T_O>();
    }





    void initialize_sysprop()
    {_G();
	SYMBOL_SC_(CorePkg,put_sysprop);
	Defun(put_sysprop);

	SYMBOL_SC_(CorePkg,get_sysprop);
	Defun(get_sysprop);

	SYMBOL_SC_(CorePkg,rem_sysprop);
	Defun(rem_sysprop);
    }




};
