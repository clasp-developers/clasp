/*
    File: documentation.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

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

#define	DEBUG_LEVEL_FULL
#include "core/common.h"
#include "core/str.h"
#include "symbolTable.h"
#include "core/hashTable.h"
#include "core/hashTableEql.h"
#include "core/multipleValues.h"
#include "core/environment.h"
#include "core/evaluator.h"
#include "documentation.h"
#include "core/wrappers.h"
namespace core
{


#define DOCS_af_record_cons "record_cons - see ECL helpfile.lsp>>record-cons"
#define LOCK_af_record_cons 1
#define ARGS_af_record_cons "(record key sub-key)"
#define DECL_af_record_cons ""
    Cons_mv af_record_cons(Cons_sp record, T_sp key, T_sp sub_key)
    {_G();
	Cons_sp cons = Cons_O::create(key,sub_key);
	for ( Cons_sp cur = record; cur.notnilp(); cur = cCdr(cur) )
	{
	    Cons_sp i = oCar(cur).as_or_nil<Cons_O>();
	    T_sp obj = oCar(i);
	    if ( eval::funcall(cl::_sym_equalp,obj,cons).isTrue() ) return(Values(i));
	}
	return(Values(_Nil<Cons_O>()));
    }




#define DOCS_af_record_field "record_field see ecl>>helpfile.lsp>>record-field"
#define LOCK_af_record_field 1
#define ARGS_af_record_field "(record key sub-key)"
#define DECL_af_record_field ""
    T_sp af_record_field(Cons_sp record, T_sp key, T_sp sub_key)
    {_G();
	Cons_sp cons = eval::funcall(_sym_record_cons,record,key,sub_key).as_or_nil<Cons_O>();;
	return oCdr(cons);
    }



#define DOCS_af_set_record_field "set_record_field"
#define LOCK_af_set_record_field 1
#define ARGS_af_set_record_field "(record key sub-key value)"
#define DECL_af_set_record_field ""
    Cons_mv af_set_record_field(Cons_sp record, T_sp key, T_sp sub_key, Str_sp value)
    {_G();
	Cons_sp field = eval::funcall(_sym_record_cons,record,key,sub_key).as_or_nil<Cons_O>();
	if ( field.notnilp() )
	{
	    field->setOCdr(value);
	} else
	{
	    Cons_sp total_key = Cons_O::create(key,sub_key);
	    Cons_sp new_field = Cons_O::create(total_key,value);
	    record = Cons_O::create(new_field,record);
	}
	return(Values(record));
    };



#define DOCS_af_rem_record_field "rem_record_field"
#define LOCK_af_rem_record_field 1
#define ARGS_af_rem_record_field "(record key sub-key)"
#define DECL_af_rem_record_field ""
    T_mv af_rem_record_field(Cons_sp record, T_sp key, T_sp sub_key )
    {_G();
	Cons_sp x = af_record_cons(record,key,sub_key);
	if ( x.notnilp() )
	{
	    Cons_sp output = _Nil<Cons_O>();
	    for ( Cons_sp cur = record; cur.notnilp(); cur = cCdr(cur) )
	    {
		Cons_sp i = oCar(cur).as_or_nil<Cons_O>();
		if ( i != x )
		{
		    output = Cons_O::create(i,output);
		}
	    }
	    return(Values(output));
	}
	return(Values(record));
    }





#define DOCS_af_annotate "annotate - see ecl>>helpfile.lsp>>annotate; key is either 'documentation or 'setf-documentation and I currently think (object) must be a symbol so I'll trigger an exception if it isn't"
#define LOCK_af_annotate 1
#define ARGS_af_annotate "(object key sub-key value)"
#define DECL_af_annotate ""
    T_mv af_annotate(T_sp object, T_sp key, T_sp sub_key, Str_sp value )
    {_G();
	HashTable_sp dict = oCar(_sym_STARdocumentation_poolSTAR->symbolValue().as_or_nil<Cons_O>()).as<HashTable_O>();
	Cons_sp record = dict->gethash(object,_Nil<T_O>()).as_or_nil<Cons_O>();
	record = af_set_record_field(record,key,sub_key,value);
        T_sp result = dict->hash_table_setf_gethash(object,record);
	return(Values(result));
    };



#define DOCS_af_ensure_documentation "ensure_documentation"
#define LOCK_af_ensure_documentation 1
#define ARGS_af_ensure_documentation "(sub-key symbol value)"
#define DECL_af_ensure_documentation ""
    SYMBOL_EXPORT_SC_(ClPkg,documentation);
    void af_ensure_documentation(T_sp sub_key, Symbol_sp symbol, Str_sp value )
    {_G();
	af_annotate(symbol,cl::_sym_documentation,sub_key,value);
    };






    void initialize_documentation_primitives(Lisp_sp lisp)
    {_G();
	SYMBOL_SC_(CorePkg,record_cons);
	Defun(record_cons);
	SYMBOL_SC_(CorePkg,record_field);
	Defun(record_field);
	SYMBOL_SC_(CorePkg,set_record_field);
	Defun(set_record_field);
	SYMBOL_SC_(CorePkg,rem_record_field);
	Defun(rem_record_field);
	SYMBOL_SC_(CorePkg,annotate);
	Defun(annotate);
	SYMBOL_SC_(CorePkg,ensure_documentation);
	Defun(ensure_documentation);
	// TODO move help_file.dat definition somewhere better
	_sym_STARdocumentation_poolSTAR->defparameter(Cons_O::createList(HashTableEql_O::create_default(),Str_O::create("help_file.dat")));
	_sym_STARdocumentation_poolSTAR->exportYourself();
    }



}; /* namespace */
