#define	DEBUG_LEVEL_FULL

#include "foundation.h"
#include "serialize.h"
#include "sexpLoadArchive.h"
#include "hashTable.h"
#include "arguments.h"
#include "designators.h"
#include "lispStream.h"
#include "primitives.h"
#include "arguments.h"
#include "write_object.h"
#include "symbolTable.h"
#include "str.h"
#include "numbers.h"
#include "symbol.h"
#include "wrappers.h"


namespace core 
{

    /*! Convert S-expressions into SNode's */
    SNode_sp parseNode(HashTable_sp objToNode, T_sp obj)
    {
	if ( obj.nilp() ) {
	    return LeafSNode_O::create(_Nil<T_O>());
	} else if ( af_atom(obj) ) {
	    SNode_sp node = objToNode->gethash(obj,_Unbound<T_O>()).as<SNode_O>();
	    if ( node.unboundp() ) {
		node = LeafSNode_O::create(obj);
		objToNode->hash_table_setf_gethash(obj,node);
	    }
	    return node;
	} else {
#if 0
	    {
		DynamicScopeManager scope(cl::_sym_STARprint_circleSTAR,_lisp->_true());
		printf("%s:%d - Building BranchSNode for:\n", __FILE__, __LINE__);
		StringOutStream_sp ss = StringOutStream_O::create();
		write_object(obj,ss);
		printf("     %s\n", ss->str().c_str());
	    }
#endif
	    Cons_sp consObj = obj.asOrNull<Cons_O>();
	    SNode_sp snode = objToNode->gethash(consObj,_Unbound<T_O>()).as<SNode_O>();
	    if ( snode.unboundp() ) {
		snode = BranchSNode_O::create();
		objToNode->hash_table_setf_gethash(obj,snode);
		Symbol_sp head = oFirst(consObj).as<Symbol_O>();
		Cons_sp dummyCons = Cons_O::create(_Nil<T_O>());
		Cons_sp result = dummyCons;
		for ( Cons_sp cur = oSecond(consObj).as<Cons_O>(); cur.notnilp(); cur=cCddr(cur) ) {
		    Symbol_sp propertyName = oCar(cur).as<Symbol_O>();
		    T_sp rawData = oCadr(cur);
		    SNode_sp propertyData = parseNode(objToNode,rawData);
		    Cons_sp one = Cons_O::create(propertyName);
		    result->setCdr(one);
		    result = one;
		    one = Cons_O::create(propertyData);
		    result->setCdr(one);
		    result = one;
		}
		VectorObjects_sp vresult(_Nil<VectorObjects_O>());
		if ( cCddr(consObj).notnilp()) {
		    Vector_sp vdata = oThird(consObj).as<Vector_O>();
		    vresult = VectorObjects_O::make(_Nil<T_O>(),_Nil<Cons_O>(), vdata->length(), true);
		    for ( int i=0,iEnd(vdata->length());i<iEnd;++i) {
			SNode_sp data = parseNode(objToNode,vdata->elt(i));
			vresult->setf_elt(i,data);
		    }
		}
		snode->setKind(head);
		snode->setAttributesUnsafe(dummyCons->cdr());
		snode->setVectorSNodesUnsafe(vresult);
#if 0 // moved BranchSNode_O::create up to top to deal with circular data structures
		snode = BranchSNode_O::create(head,dummyCons->cdr(),vresult);
		objToNode->hash_table_setf_gethash(obj,snode);
#endif
	    }
	    return snode;
	}
	SIMPLE_ERROR(BF("SexpLoadArchive - could not parse %s") % _rep_(obj));
    }


    EXPOSE_CLASS(core,SexpLoadArchive_O);

    void SexpLoadArchive_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<SexpLoadArchive_O>("make-sexp-load-archive")
	    .def("parseFromObject",&SexpLoadArchive_O::parseFromObject)
	    .def("parseFromStream",&SexpLoadArchive_O::parseFromStream)
	    ;
    }
    void SexpLoadArchive_O::exposePython(Lisp_sp lisp)
    {_G();
    }





    void SexpLoadArchive_O::parseFromObject(T_sp object)
    {
	DynamicScopeManager scope(_sym_STARserializerArchiveSTAR,this->asSmartPtr());
	HashTable_sp objToNode = HashTable_O::create(cl::_sym_eq);
	this->_TopNode = parseNode(objToNode,object).as<BranchSNode_O>();
	this->createContents();
    };

    void SexpLoadArchive_O::parseFromStream(T_sp streamDesignator)
    {
	DynamicScopeManager scope(_sym_STARserializerArchiveSTAR,this->asSmartPtr());
	T_sp obj = af_read(streamDesignator,_lisp->_true(), _Unbound<T_O>());
	if ( obj.unboundp() ) {
	    SIMPLE_ERROR(BF("Nothing could be read from stream"));
	}
	this->parseFromObject(obj);
    };





};
