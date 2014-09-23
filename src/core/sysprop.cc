/*
    File: sysprop.cc
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
