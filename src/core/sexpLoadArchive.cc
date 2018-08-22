/*
    File: sexpLoadArchive.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/serialize.h>
#include <clasp/core/sexpLoadArchive.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/arguments.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/primitives.h>
#include <clasp/core/arguments.h>
#include <clasp/core/write_object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

namespace core {

/*! Convert S-expressions into SNode's */
SNode_sp parseNode(HashTable_sp objToNode, T_sp obj) {
  if (obj.nilp()) {
    return LeafSNode_O::create(_Nil<T_O>());
  } else if (cl__atom(obj)) {
    SNode_sp node = gc::As<SNode_sp>(objToNode->gethash(obj, _Unbound<T_O>()));
    if (node.unboundp()) {
      node = LeafSNode_O::create(obj);
      objToNode->hash_table_setf_gethash(obj, node);
    }
    return node;
  } else {
#if 0
	    {
		DynamicScopeManager scope(cl::_sym_STARprint_circleSTAR,_lisp->_true());
		printf("%s:%d - Building BranchSNode for:\n", __FILE__, __LINE__);
		T_sp ss = clasp_make_string_output_stream();
		write_object(obj,ss);
		printf("     %s\n", ss->str().c_str());
	    }
#endif
    Cons_sp consObj = obj.asOrNull<Cons_O>();
    SNode_sp snode = gc::As<SNode_sp>(objToNode->gethash(consObj, _Unbound<T_O>()));
    if (snode.unboundp()) {
      snode = BranchSNode_O::create();
      objToNode->hash_table_setf_gethash(obj, snode);
      Symbol_sp head = gc::As<Symbol_sp>(oFirst(consObj));
      Cons_sp dummyCons = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
      Cons_sp result = dummyCons;
      for (List_sp cur = oSecond(consObj); cur.consp(); cur = oCddr(cur)) {
        Symbol_sp propertyName = gc::As<Symbol_sp>(oCar(cur));
        T_sp rawData = oCadr(cur);
        SNode_sp propertyData = parseNode(objToNode, rawData);
        Cons_sp one = Cons_O::create(propertyName,_Nil<T_O>());
        result->setCdr(one);
        result = one;
        one = Cons_O::create(propertyData,_Nil<T_O>());
        result->setCdr(one);
        result = one;
      }
      VectorObjects_sp vresult(_Nil<VectorObjects_O>());
      if (oCddr(consObj).notnilp()) {
        Vector_sp vdata = gc::As<Vector_sp>(oThird(consObj));
        vresult = VectorObjects_O::make(vdata->length(),_Nil<T_O>());
        for (int i = 0, iEnd(vdata->length()); i < iEnd; ++i) {
          SNode_sp data = parseNode(objToNode, vdata->rowMajorAref(i));
          vresult->rowMajorAset(i, data);
        }
      }
      snode->setKind(head);
      snode->setAttributesUnsafe(oCdr(dummyCons));
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





CL_LISPIFY_NAME("parseFromObject");
CL_DEFMETHOD void SexpLoadArchive_O::parseFromObject(T_sp object) {
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  HashTable_sp objToNode = HashTable_O::create(cl::_sym_eq);
  this->_TopNode = gc::As<BranchSNode_sp>(parseNode(objToNode, object));
  this->createContents();
};

CL_LISPIFY_NAME("parseFromStream");
CL_DEFMETHOD void SexpLoadArchive_O::parseFromStream(T_sp streamDesignator) {
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  T_sp obj = cl__read(streamDesignator, _lisp->_true(), _Unbound<T_O>());
  if (obj.unboundp()) {
    SIMPLE_ERROR(BF("Nothing could be read from stream"));
  }
  this->parseFromObject(obj);
};
};
