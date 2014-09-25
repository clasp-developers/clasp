/*
    File: hashTableEqualp.cc
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
#include "core/environment.h"
#include "hashTableEqualp.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,HashTableEqualp_O);

    void HashTableEqualp_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<HashTableEqualp_O>()
//	.initArgs("(self)")
	;
    }

    void HashTableEqualp_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),HashTableEqualp,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }


    HashTableEqualp_sp HashTableEqualp_O::create(uint sz,  Number_sp rehashSize, double rehashThreshold)
    {_G();
        GC_ALLOCATE(HashTableEqualp_O,hashTable );
	hashTable->setup(sz,rehashSize,rehashThreshold);
	return hashTable;
    }



#if 0
    void HashTableEqualp_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if defined(XML_ARCHIVE)
    void HashTableEqualp_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)


    bool HashTableEqualp_O::keyTest(T_sp entryKey, T_sp searchKey) const
    {_OF();
	bool equalp = af_equalp(entryKey,searchKey);
//        printf("%s:%d HashTableEqualp_O::keyTest testing if %s equalp %s -->%d\n",__FILE__,__LINE__,_rep_(entryKey).c_str(),_rep_(searchKey).c_str(),equalp);
        return equalp;
    }

  int HashTableEqualp_O::sxhashKey(T_sp obj,int bound, bool willAddKey) const
    {_OF();
	HashGenerator hg;
	HashTable_O::sxhash_equalp(hg,obj,NULL);
	return hg.hash(bound);
    }
    

}; /* core */
