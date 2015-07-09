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
#define DEBUG_LEVEL_FULL

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
#include <clasp/core/str.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

namespace core {

/*! Convert S-expressions into SNode's */
SNode_sp parseNode(HashTable_sp objToNode, T_sp obj) {
  if (obj.nilp()) {
    return LeafSNode_O::create(_Nil<T_O>());
  } else if (af_atom(obj)) {
    SNode_sp node = objToNode->gethash(obj, _Unbound<T_O>()).as<SNode_O>();
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
    SNode_sp snode = objToNode->gethash(consObj, _Unbound<T_O>()).as<SNode_O>();
    if (snode.unboundp()) {
      snode = BranchSNode_O::create();
      objToNode->hash_table_setf_gethash(obj, snode);
      Symbol_sp head = oFirst(consObj).as<Symbol_O>();
      Cons_sp dummyCons = Cons_O::create(_Nil<T_O>());
      Cons_sp result = dummyCons;
      for (Cons_sp cur = oSecond(consObj).as<Cons_O>(); cur.notnilp(); cur = cCddr(cur)) {
        Symbol_sp propertyName = oCar(cur).as<Symbol_O>();
        T_sp rawData = oCadr(cur);
        SNode_sp propertyData = parseNode(objToNode, rawData);
        Cons_sp one = Cons_O::create(propertyName);
        result->setCdr(one);
        result = one;
        one = Cons_O::create(propertyData);
        result->setCdr(one);
        result = one;
      }
      VectorObjects_sp vresult(_Nil<VectorObjects_O>());
      if (cCddr(consObj).notnilp()) {
        Vector_sp vdata = oThird(consObj).as<Vector_O>();
        vresult = VectorObjects_O::make(_Nil<T_O>(), _Nil<Cons_O>(), vdata->length(), true);
        for (int i = 0, iEnd(vdata->length()); i < iEnd; ++i) {
          SNode_sp data = parseNode(objToNode, vdata->elt(i));
          vresult->setf_elt(i, data);
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

EXPOSE_CLASS(core, SexpLoadArchive_O);

void SexpLoadArchive_O::exposeCando(Lisp_sp lisp) {
  _G();
  class_<SexpLoadArchive_O>("make-sexp-load-archive")
      .def("parseFromObject", &SexpLoadArchive_O::parseFromObject)
      .def("parseFromStream", &SexpLoadArchive_O::parseFromStream);
}
void SexpLoadArchive_O::exposePython(Lisp_sp lisp) {
  _G();
}

void SexpLoadArchive_O::parseFromObject(T_sp object) {
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  HashTable_sp objToNode = HashTable_O::create(cl::_sym_eq);
  this->_TopNode = parseNode(objToNode, object).as<BranchSNode_O>();
  this->createContents();
};

void SexpLoadArchive_O::parseFromStream(T_sp streamDesignator) {
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  // Don't track source code for archives
  scope.pushSpecialVariableAndSet(_sym_STARsourceDatabaseSTAR, _Nil<T_O>());
  scope.pushSpecialVariableAndSet(_sym_STARmonitorRegisterSourceInfoSTAR, _lisp->_true());
  T_sp obj = cl_read(streamDesignator, _lisp->_true(), _Unbound<T_O>());
  if (obj.unboundp()) {
    SIMPLE_ERROR(BF("Nothing could be read from stream"));
  }
  this->parseFromObject(obj);
};
};
