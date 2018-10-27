/*
    File: serialize.cc
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
#include <clasp/core/object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/array.h>
#include <clasp/core/arguments.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/serialize.h>

#include <clasp/core/wrappers.h>

namespace core {

SNode_sp getOrCreateSNodeForObjectIncRefCount(T_sp val) {
  SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
  SNode_sp node = saveArchive->getOrCreateSNodeForObjectIncRefCount(val);
  return node;
}

SYMBOL_EXPORT_SC_(KeywordPkg, podSymbolMap);



SNode_sp SNode_O::makeAppropriateSNode(T_sp val, HashTable_sp objToSNodeMap) {
  DEPRECATED();
}



CL_DEFUN BranchSNode_sp core__make_branch_snode() {
  return BranchSNode_O::create();
}

SNode_sp SNode_O::createBranchSNode(Symbol_sp kind) {
  SNode_sp snode = BranchSNode_O::create(kind, _Nil<T_O>(), _Unbound<Vector_O>());
  return snode;
}

bool SNode_O::loading() const {
  return Archive_O::currentArchive()->loading();
}

void SNode_O::needsFinalization() const {
  if (this->loading()) {
    LoadArchive_sp loadArchive = Archive_O::currentLoadArchive();
    loadArchive->needsFinalization(this->asSmartPtr());
  }
}

BranchSNode_sp BranchSNode_O::create() {
  GC_ALLOCATE(BranchSNode_O, v);
  return v;
}

BranchSNode_sp BranchSNode_O::create(Symbol_sp kind, List_sp plist, Vector_sp data) {
  GC_ALLOCATE(BranchSNode_O, v);
  v->_Kind = kind;
  v->_SNodePList = plist;
  v->_VectorSNodes = data;
  return v;
}





T_sp BranchSNode_O::object() const {
  LoadArchive_sp archive = Archive_O::currentLoadArchive();
  BranchSNode_sp me = this->asSmartPtr();
  T_sp obj = archive->loadObjectDirectly(me);
  return obj;
}

/*! Push the SNode into the _VectorSNodes */
void BranchSNode_O::pushVectorSNode(SNode_sp snode) {
  SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
  if (this->_VectorSNodes.nilp()) {
    this->_VectorSNodes = gc::As<Vector_sp>(VectorObjects_O::make( 4, _Nil<T_O>(), clasp_make_fixnum(0)));
  }
  this->_VectorSNodes->vectorPushExtend(snode);
}

/*! Convert the object into an SNode and push it into _VectorSNodes */
void BranchSNode_O::pushVector(T_sp obj) {
  SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
  SNode_sp snode = saveArchive->getOrCreateSNodeForObjectIncRefCount(obj);
  if (this->_VectorSNodes.unboundp()) {
    this->_VectorSNodes = gc::As<Vector_sp>(VectorObjects_O::make( 4, _Nil<T_O>(), clasp_make_fixnum(0)));
  }
  this->pushVectorSNode(snode);
}

List_sp BranchSNode_O::keys() const {
  List_sp keys = _Nil<T_O>();
  for (List_sp cur = this->_SNodePList; cur.consp(); cur = oCddr(cur)) {
    keys = Cons_O::create(oCar(cur), keys);
  }
  return keys;
}

void BranchSNode_O::addAttributeSNode(Symbol_sp name, SNode_sp snode) {
  SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
  this->_SNodePList = Cons_O::create(snode, this->_SNodePList);
  this->_SNodePList = Cons_O::create(name, this->_SNodePList);
}

void BranchSNode_O::addAttribute(Symbol_sp name, T_sp val) {
  SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
  SNode_sp snode = saveArchive->getOrCreateSNodeForObjectIncRefCount(val);
  this->_SNodePList = Cons_O::create(snode, this->_SNodePList);
  this->_SNodePList = Cons_O::create(name, this->_SNodePList);
}

SNode_sp BranchSNode_O::getAttributeSNode(Symbol_sp name, SNode_sp defaultValue) const {
  if (this->_SNodePList.notnilp()) {
    SNode_sp result = gc::As<SNode_sp>(this->_SNodePList.asCons()->getf(name, _Unbound<T_O>()));
    if (!result.unboundp()) {
      return result;
    }
  }
  return defaultValue;
}

SNode_sp BranchSNode_O::getAttributeSNodeOrError(Symbol_sp name) const {
  SNode_sp result = this->getAttributeSNode(name, _Unbound<SNode_O>());
  if (result.unboundp()) {
    SIMPLE_ERROR(BF("Could not find attribute with name %s") % _rep_(name));
  }
  return result;
}

T_sp BranchSNode_O::getAttribute(Symbol_sp name, T_sp defaultValue) const {
  if (this->_SNodePList.notnilp()) {
    SNode_sp result = gc::As<SNode_sp>(this->_SNodePList.asCons()->getf(name, _Unbound<T_O>()));
    if (!result.unboundp()) {
      return result->object();
    }
  }
  return defaultValue;
}

void BranchSNode_O::loadVector(gctools::Vec0<T_sp> &vec) {
  LoadArchive_sp loadArchive = Archive_O::currentLoadArchive();
  vec.clear();
  if (Vector_sp vectorSNodes = this->_VectorSNodes.asOrNull<Vector_O>()) {
    for (int i(0), iEnd(vectorSNodes->length()); i < iEnd; ++i) {
      SNode_sp snode = gc::As<SNode_sp>(vectorSNodes->rowMajorAref(i));
      vec.push_back(snode->object());
    };
  }
  SIMPLE_ERROR(BF("loadVector called when _VectorSNodes was nil"));
}

/*! For each element in the _VectorSNodes, convert it to an object and
      pass it to the function */
void BranchSNode_O::mapVector(std::function<void(T_sp)> const &fn) {
  if (this->_VectorSNodes.unboundp())
    SIMPLE_ERROR(BF("BranchSNode VectorSNodes is unbound"));
  for (int i(0), iEnd(this->_VectorSNodes->length()); i < iEnd; ++i) {
    SNode_sp snode = gc::As<SNode_sp>(this->_VectorSNodes->rowMajorAref(i));
    T_sp obj = snode->object();
    fn(obj);
  };
};

void BranchSNode_O::saveVector(gctools::Vec0<T_sp> const &vec) {
  SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
  this->_VectorSNodes = gc::As<Vector_sp>(VectorObjects_O::make(4,_Nil<T_O>(), clasp_make_fixnum(0)));
  for (auto it = vec.begin(); it != vec.end(); it++) {
    SNode_sp snode = saveArchive->getOrCreateSNodeForObjectIncRefCount(*it);
    this->_VectorSNodes->vectorPushExtend(snode);
  };
}

T_sp BranchSNode_O::createObject(HashTable_sp snodeToObject) {
  DEPRECATED();
};

string BranchSNode_O::__repr__() const {
  stringstream ss;
  ss << "#<" << _rep_(this->__class()->_className()) << " ";
  ss << ":kind " << _rep_(this->_Kind) << " ";
  ss << ":plist " << _rep_(this->_SNodePList) << " ";
  ss << ":vector " << _rep_(this->_VectorSNodes) << ">";
  return ss.str();
}

T_sp BranchSNode_O::getUniqueId() const {
  for (List_sp cur = this->_SNodePList; cur.consp(); cur = oCddr(cur)) {
    Symbol_sp propertyName = gc::As<Symbol_sp>(oCar(cur));
    if (propertyName == kw::_sym__uid) {
      T_sp property = oCadr(cur);
      return property;
    }
  }
  return _Nil<T_O>();
}

SNode_sp BranchSNode_O::childWithUniqueId(Symbol_sp uid) const {
  for (int i(0), iEnd(this->_VectorSNodes->length()); i < iEnd; ++i) {
    SNode_sp child = gc::As<SNode_sp>(this->_VectorSNodes->rowMajorAref(i));
    if (child->getUniqueId() == uid) {
      return child;
    }
  }
  return _Nil<SNode_O>();
}

LeafSNode_sp LeafSNode_O::create(T_sp obj) {
  GC_ALLOCATE(LeafSNode_O, v);
  v->_Object = obj;
  return v;
}





string LeafSNode_O::__repr__() const {
  return _rep_(this->_Object);
}

Archive_O::Archive_O() : _Version(0), _TopNode(_Nil<T_O>()), _NextUniqueId(0){};





string Archive_O::__repr__() const {
  stringstream ss;
  ss << "#<" << _rep_(this->__class()->_className()) << " ";
  ss << _rep_(this->_TopNode) << ">";
  return ss.str();
}





Archive_sp Archive_O::currentArchive() {
  SYMBOL_EXPORT_SC_(CorePkg, STARserializerArchiveSTAR);
  T_sp archive = gc::As<Archive_sp>(_sym_STARserializerArchiveSTAR->symbolValue());
  if (archive.nilp()) {
    SIMPLE_ERROR(BF("You must define core:*serializer-archive*"));
  }
  return gc::As<Archive_sp>(archive);
}

LoadArchive_sp Archive_O::currentLoadArchive() {
  Archive_sp archive = Archive_O::currentArchive();
  if (LoadArchive_sp loadArchive = archive.asOrNull<LoadArchive_O>())
    return loadArchive;
  SIMPLE_ERROR(BF("The value of *serializer-archive* is not a load-archive - its value is: %s") % _rep_(archive));
}

void SaveArchive_O::initialize() {
  this->Base::initialize();
  this->_SNodeForObject = HashTable_O::create(cl::_sym_eq);
  this->_TopNode = BranchSNode_O::create();
}

SaveArchive_sp Archive_O::currentSaveArchive() {
  Archive_sp archive = Archive_O::currentArchive();
  if (SaveArchive_sp saveArchive = archive.asOrNull<SaveArchive_O>())
    return saveArchive;
  SIMPLE_ERROR(BF("The value of *serializer-archive* is not a save-archive"));
}

CL_LISPIFY_NAME("put");
CL_DEFMETHOD void SaveArchive_O::put(Symbol_sp name, T_sp val) {
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  this->_TopNode->addAttribute(name, val);
}

SNode_sp SaveArchive_O::getOrCreateSNodeForObjectIncRefCount(T_sp val) {
  SNode_sp snode = gc::As<SNode_sp>(this->_SNodeForObject->gethash(val, _Unbound<T_O>()));
  if (snode.unboundp()) {
    snode = SNode_O::makeAppropriateSNode(val, this->_SNodeForObject);
  } else {
    snode->incRefCount();
  }
  return snode;
}





void LoadArchive_O::initialize() {
  this->Base::initialize();
  this->_ObjectForSNode = HashTableEq_O::create_default();
  this->_NodesToFinalize = HashTableEq_O::create_default();
}

void LoadArchive_O::addNodeToFinalize(SNode_sp node) {
  IMPLEMENT_ME();
}

T_sp LoadArchive_O::loadObjectDirectly(SNode_sp node) {
  if (LeafSNode_sp leafSNode = node.asOrNull<LeafSNode_O>()) {
    return node->object();
  }
  BranchSNode_sp branchSNode = gc::As<BranchSNode_sp>(node);
  if (node == this->_TopNode) {
    // Don't create an object for the top node - just for it's children
    for (List_sp cur = branchSNode->_SNodePList; cur.consp(); cur = oCddr(cur)) {
      this->loadObjectDirectly(gc::As<SNode_sp>(oCadr(cur)));
    }
    if (!branchSNode->_VectorSNodes.unboundp()) {
      Vector_sp branchSNodeVectorSNodes = gc::As<Vector_sp>(branchSNode->_VectorSNodes);
      for (int i(0), iEnd(branchSNodeVectorSNodes->length()); i < iEnd; ++i) {
        this->loadObjectDirectly(gc::As<SNode_sp>(branchSNodeVectorSNodes->rowMajorAref(i)));
      }
    }
    IMPLEMENT_MEF("Should I return NIL at this point?  The BranchSNode was not completely initialized");
    return _Nil<T_O>();
  } else {
    T_sp findObj = this->_ObjectForSNode->gethash(branchSNode, _Unbound<T_O>());
    if (findObj.unboundp()) {
      findObj = branchSNode->createObject(this->_ObjectForSNode);
      //		this->_ObjectForSNode->hash_table_setf_gethash(branchSNode,findObj);
    }
    return findObj;
  }
  UNREACHABLE();
}

void LoadArchive_O::createContents() {
  this->_ObjectForSNode = HashTable_O::create(cl::_sym_eq);
  this->loadObjectDirectly(this->_TopNode);
}

void LoadArchive_O::needsFinalization(SNode_sp node) {
  this->_NodesToFinalize->setf_gethash(node, _Nil<T_O>());
}

void LoadArchive_O::finalizeObjects() {
  DEPRECATED();
}

CL_LISPIFY_NAME("loadArchive-contains");
CL_DEFMETHOD bool LoadArchive_O::contains(Symbol_sp sym) {
  if (this->_TopNode.unboundp())
    SIMPLE_ERROR(BF("No archive is loaded"));
  BranchSNode_sp bnode = this->_TopNode;
  T_sp property = bnode->_SNodePList.asCons()->getf(sym, _Unbound<T_O>());
  if (property.unboundp()) {
    return false;
  }
  return true;
}

CL_LISPIFY_NAME("loadArchive-get");
CL_DEFMETHOD T_sp LoadArchive_O::get(Symbol_sp sym) {
  if (this->_TopNode.unboundp())
    SIMPLE_ERROR(BF("No archive is loaded"));
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  BranchSNode_sp bnode = this->_TopNode;
  SNode_sp property = gc::As<SNode_sp>(bnode->_SNodePList.asCons()->getf(sym, _Unbound<T_O>()));
  if (property.unboundp()) {
    SIMPLE_ERROR(BF("Could not find property for key: %s") % _rep_(sym));
  }
  return property->object();
}

CL_LISPIFY_NAME("loadArchive-getContents");
CL_DEFMETHOD List_sp LoadArchive_O::getContents() {
  if (this->_TopNode.unboundp())
    SIMPLE_ERROR(BF("No archive is loaded"));
  BranchSNode_sp bnode = this->_TopNode;
  return bnode->_SNodePList;
}

CL_LISPIFY_NAME("loadArchive-keys");
CL_DEFMETHOD List_sp LoadArchive_O::keys() const {
  if (this->_TopNode.unboundp())
    SIMPLE_ERROR(BF("There is no archive loaded"));
  return this->_TopNode->keys();
}
};
