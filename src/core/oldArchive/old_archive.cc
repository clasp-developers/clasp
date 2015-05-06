/*
    File: old_archive.cc
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
#define DEBUG_LEVEL_NONE

#include "core/common.h"
//#i n c l u d e "candoDatabase.h"
#include "lisp.h"
#include "wrappers.h"

#define CURRENT_VERSION 1

namespace core {

string ObjectPathTag = "_ref";
string NilPath = "!nil";
string WeakPointerNull = "WP_ERROR-NULL";
string WeakPointerBroken = "WP_ERROR-BROKEN-LINK";
string RedirectedWeakPointerBroken = "WP_ERROR-BROKEN-REDIRECTED_LINK";

const string Archive_O::UniqueIdTag = "_uid";
const string Archive_O::CoreBuiltInClassAttribute = "_coreClass";

SymbolManager::SymbolManager() {
  this->handleSetExplicit("", EmptyStringHandle);
  this->handleSetExplicit(Archive_O::UniqueIdTag, UniqueIdHandle);
}

void SymbolManager::printDescription(const Lisp_sp &lisp) {
  lisp->print(BF("Describe symbol manager memory usage here"));
}

/*! Return the handle for a symbol
     */
handleType SymbolManager::handle(const string &symbol) {
  handleType handle = IllegalHandle;
  /* If the symbol is a number then
	 * Return the value with the handleNumberFlag bit set
	 */
  const char *cp = symbol.c_str();
  if (*cp >= '0' && *cp <= '9') {
    char *cpRest;
    handle = strtol(cp, &cpRest, 0);
    if (handle < MaxHandle && *cpRest == '\0') {
      // Make it a numeric handle and
      // Return it
      handle |= handleNumberFlag;
      return ((handle));
    }
  }
  handle = IllegalHandle;
  /* Its a symbolic handle so save it in
	 * the symbol map
	 */
  map<string, handleType>::iterator it;
  it = this->_SymbolsToHandles.find(symbol);
  if (it == this->_SymbolsToHandles.end()) {
    handle = this->_Symbols.size();
    if (handle >= MaxHandle) {
      stringstream serr;
      serr << "Exceeded MaxHandle(" << MaxHandle << ") symbols, define handleType as a larger type - " << sizeof(handleType) << " bytes isn't enough";

      THROW_HARD_ERROR(serr.str());
    }
    this->_Symbols.push_back(symbol);
    this->_SymbolsToHandles[symbol] = handle;
  } else {
    handle = it->second;
  }
  return ((handle));
}

void SymbolManager::handleSetExplicit(const string &name, handleType h) {
  handleType hIs = this->handle(name);
  if (hIs != h) {
    stringstream serr;
    serr << "Could not set handle for(" << name << ") explicitly to(" << h << ")";
    THROW_HARD_ERROR(serr.str());
  }
}

/*! Return the handle for a numeric symbol
     */
handleType SymbolManager::handleFromNumber(uint symbol) {
  handleType handle = IllegalHandle;
  if (symbol > MaxHandle) {
    stringstream ss;
    ss << "Illegal numeric symbol(" << symbol << ") it cannot be larger than " << MaxHandle << ".";
    THROW_HARD_ERROR(ss.str());
  }
  handle = symbol | handleNumberFlag;
  return ((handle));
}

bool SymbolManager::isNumericSymbol(const handleType handle) const {
  return (((handle & handleNumberFlag) != 0));
}

handleType SymbolManager::numericSymbol(const handleType handle) const {
  if (!(handle & handleNumberFlag)) {
    stringstream serr;
    serr << "numericSymbolCalled for a non numeric symbol(" << this->textSymbol(handle) << ") handle=" << handle;
    THROW_HARD_ERROR(serr.str());
  }
  return ((handle & handleNumberMask));
}

const string &SymbolManager::textSymbol(const handleType handle) const {
  if (handle & handleNumberFlag) {
    THROW_HARD_ERROR("textSymbol called for numeric symbol");
  }
  HARD_ASSERT(handle < this->_Symbols.size());
  RET_POD((this->_Symbols[handle]));
}

string SymbolManager::anySymbol(const handleType handle) const {
  if (handle & handleNumberFlag) {
    stringstream ss;
    ss << (handle & handleNumberMask);
    return ((ss.str()));
  }
  HARD_ASSERT(handle < this->_Symbols.size());
  return ((this->_Symbols[handle]));
}

void Archive_O::exposeCando(Lisp_sp env) {
  class_<Archive_O>()
      .def("debugEnabled", &Archive_O::debugEnabled)
      .def("enableDebug", &Archive_O::enableDebug)
      .def("disableDebug", &Archive_O::disableDebug)
      .def("loading", &Archive_O::loading)
      .def("isSaveArchive", &Archive_O::isSaveArchive);
}

void Archive_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Archive, "", "", _lisp)
      .def("debugEnabled", &Archive_O::debugEnabled)
      .def("enableDebug", &Archive_O::enableDebug)
      .def("disableDebug", &Archive_O::disableDebug)
      .def("loading", &Archive_O::loading)
      .def("isSaveArchive", &Archive_O::isSaveArchive);
#endif
}

Archive_O::Archive_O() : Base(), _SymbolManagerPtr(NULL) {
  if (!this->isNil() && !this->isUnbound()) {
    this->_SymbolManagerPtr = new SymbolManager();
  }
}

Archive_O::~Archive_O() {
  this->_ArchiveMemoryManager.reset();
  this->_MultiStringBuffer.reset();
  if (this->_HasError) {
    printf("There was an error in the Archive(%s)!!!!!!\n",
           this->_FileName.c_str());
    printf("%s\n", this->_ErrorStream.str().c_str());
  }
  if (this->_SymbolManagerPtr)
    delete this->_SymbolManagerPtr;
};

void Archive_O::initialize() {
  this->Base::initialize();
  this->_NextUniqueId = 1;
  this->_DebugEnabled = false;
  this->_HasError = false;
  this->_Verbosity = 0;
  this->_ErrorStream.str("");
  this->_ArchiveMemoryManager = ArchiveMemoryManager_O::create(this->sharedThis<Archive_O>(), _lisp);
  this->_TopNode = NULL;
  this->_MultiStringBuffer = MultiStringBuffer_O::create();
  this->_NextReferenceHandle = 0;
}

uint Archive_O::nextReferenceHandle() {
  _OF();
  this->_NextReferenceHandle++;
  return ((this->_NextReferenceHandle));
}

void Archive_O::describeMemoryUsage() {
  _G();
  LongLongInt total = 0;
  total += this->getTopNode()->describeMemoryUsage();
  total += this->_MultiStringBuffer->describeMemoryUsage();
  _lisp->print(BF("   Total memory usage of archive = %ld bytes") % total);
}

ArchiveP Archive_O::getTopNode() {
  _G();
  if (this->_TopNode == NULL) {
    this->_TopNode = this->newArchiveNode();
    this->_TopNode->setRawNodeName("Archive");
    this->_TopNode->addAttribute("version", CURRENT_VERSION);
    this->_TopNode->setTextUniqueId("_root_");
  }
  return ((this->_TopNode));
}

void LoadArchive_O::exposeCando(Lisp_sp env) {
  class_<LoadArchive_O>(no_init)
      .def("parse", &LoadArchive_O::parse)
      .def("contains", &LoadArchive_O::contains)
      .def("get", &LoadArchive_O::get);
}

void LoadArchive_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, LoadArchive, "", "", _lisp)
      .def("parse", &LoadArchive_O::parse)
      .def("contains", &LoadArchive_O::contains)
      .def("get", &LoadArchive_O::get);
#endif
}

void LoadArchive_O::initialize() {
  this->Base::initialize();
  this->_NodesToFinalize.clear();
};

void LoadArchive_O::addNodeToFinalize(Dumb_Node *node) {
  _G();
  this->_NodesToFinalize.push_back(node);
}

void LoadArchive_O::finalizeObjects() {
  _G();
  vector<Dumb_Node *>::iterator it;
  bool allDone, oneDone;
  int finalizeTrys;
  T_sp obj;
  if (this->_Verbosity > 0) {
    _lisp->print(BF(" Finalizing archive objects"));
  }
  LOG(BF("There are %d objects to finalize") % this->_NodesToFinalize.size());
  finalizeTrys = 0;
  allDone = false;
  for (it = this->_NodesToFinalize.begin();
       it != this->_NodesToFinalize.end(); it++) {
    (*it)->setFinalized(false);
  }
  while (!allDone) {
    allDone = true;
    for (it = this->_NodesToFinalize.begin();
         it != this->_NodesToFinalize.end(); it++) {
      if ((*it)->isFinalized())
        continue;
      obj = (*it)->getObject();
      if (obj->notNil()) {
        oneDone = (obj->loadFinalize(*it));
        if (oneDone)
          (*it)->setFinalized(true);
        allDone |= oneDone;
        LOG(BF("Called loadFinalize"));
      } else {
        LOG(BF("empty object"));
      }
    }
    finalizeTrys++;
    if (finalizeTrys > 100) {
      //
      // Something refused to finalize, you can dig it out of the _NodesToFinalize list
      //
      SIMPLE_ERROR(BF("Tried to finalize the loaded object 100 times, there must be a circular reference"));
    }
  }
}

void SaveArchive_O::exposeCando(Lisp_sp env) {
  class_<SaveArchive_O>(no_init)
      .def("put", &SaveArchive_O::put)
      //        .def("putCandoDatabase",&SaveArchive_O::putCandoDatabase)
      .def("saveAs", &SaveArchive_O::saveAs)
      .def("asString", &SaveArchive_O::asString)
      .def("numberOfBrokenWeakPointers", &SaveArchive_O::numberOfBrokenWeakPointers);
}
void SaveArchive_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, SaveArchive, "", "", _lisp)
      .def("put", &SaveArchive_O::put)
      .def("saveAs", &SaveArchive_O::saveAs)
      .def("asString", &SaveArchive_O::asString)
      .def("numberOfBrokenWeakPointers", &SaveArchive_O::numberOfBrokenWeakPointers);
#endif
}

void SaveArchive_O::initialize() {
  this->Base::initialize();
}

/*!
 * If the obj pointer is NULL then write WeakPointerNull
 */
void Archive_O::saveWeakPointerInAttribute(ArchiveP node, const string &attr, T_wp obj,
                                           bool suppressNodeForBrokenOrNilWeakPointers,
                                           bool suppressAttributeForNilWeakPointers) {
  _G();
  SaveWeakPointerEntry entry;
  LOG(BF("Saving weak pointer node(%s) attribute(%s)") % node->getNodeName().c_str() % attr.c_str());
  entry._Node = node;
  entry._Attribute = attr;
  entry._SuppressNodeForBrokenOrNilWeakPointers = suppressNodeForBrokenOrNilWeakPointers;
  entry._SuppressAttributeForNilWeakPointers = suppressAttributeForNilWeakPointers;
  if (obj.use_count() == 0) {
    entry._Object = T_O::_nil;
    //	stringstream ss;
    //	ss << "NULL Attribute(" << attr << ")  of node: " << node->description();
  } else if (obj.lock()->isNil()) {
    LOG(BF("The object NULL"));
    entry._Object = T_O::_nil;
  } else {
    LOG(BF("The object was defined"));
    entry._Object = obj.lock();
  }
  this->_WeakObjectReferences.push_back(entry);
}

void Archive_O::saveOnlyObjectOfClassWithinNode(ArchiveP node, Symbol_sp classSymbol, T_sp obj) {
  _G();
  //
  // If the object hasn't been seen yet then create a subNode and archive into it
  //
  ASSERT(obj->notNil());
  ArchiveP child;
  if (this->_ObjectNodes.count(obj.get()) == 0) {
    child = this->newArchiveNode();
    child->setClassNodeName(_lisp->classFromClassSymbol(classSymbol));
    child->setObject(obj);
    //	child->setUniqueId("only");
    node->addChild(child);
    ASSERTNOTNULL(obj);
    obj->archive(child);
    this->_ObjectNodes[obj.get()] = child;
  } else {
    SIMPLE_ERROR(BF("There is already a node with ClassName: %s") %
                 _lisp->classFromClassSymbol(classSymbol)->getPackagedName());
  }
}

void Archive_O::_saveObjectWithinNodeWithUidAndPossiblyKey(ArchiveP node, handleType huid, const string &key, bool useKey, T_sp obj) {
  _G();

  ASSERTNOTNULL(obj);
  //
  // If the object hasn't been seen yet then create a subNode and archive into it
  //
  ArchiveP child;
  if (obj->isNil()) {
    LOG(BF("obj->isNil()==true"));
    child = this->newArchiveNode();
    child->setClassNodeName(obj->_class());
    child->setUniqueIdHandle(huid);
    node->addChild(child);
    child->addAttribute(ObjectPathTag, NilPath);
  } else if (this->_ObjectNodes.count(obj.get()) == 0) {
    child = this->newArchiveNode();
    child->setClassNodeName(obj->_class());
    child->setUniqueIdHandle(huid);
    if (useKey)
      child->addAttribute("_key", key);
    child->setObject(obj);
    node->addChild(child);
    obj->archive(child);
    this->_ObjectNodes[obj.get()] = child;
    //
    // If it has been archived then
    // just create a node to it
  } else {
    ArchiveP existingObjectNode = this->_ObjectNodes[obj.get()];
    existingObjectNode->incrementReferenceCountAndAssignReferenceHandle();
    child = this->newArchiveNode();
    child->setClassNodeName(obj->_class());
    child->setUniqueIdHandle(huid);
    if (useKey) {
      child->addAttribute("_key", key);
    }
    node->addChild(child);
    // Calculate the pathRelativeTo as the last thing
    string pth = existingObjectNode->pathRelativeTo(child);
    child->addAttribute(ObjectPathTag, pth);
  }
}

void Archive_O::saveObjectWithinNodeWithUid(ArchiveP node, handleType huid, T_sp obj) {
  _G();
  this->_saveObjectWithinNodeWithUidAndPossiblyKey(node, huid, "", false, obj);
}

void Archive_O::saveObjectWithinNodeWithUid(ArchiveP node, const string &uid, T_sp obj) {
  _G();
  handleType huid = this->symbolManager().handle(uid);
  this->_saveObjectWithinNodeWithUidAndPossiblyKey(node, huid, "", false, obj);
}

void Archive_O::saveObjectWithinNodeWithUidAndKey(ArchiveP node, handleType huid, const string &key, T_sp obj) {
  _G();
  this->_saveObjectWithinNodeWithUidAndPossiblyKey(node, huid, key, true, obj);
}

void Archive_O::saveObjectWithinNodeWithUidAndKey(ArchiveP node, const string &uid, const string &key, T_sp obj) {
  _G();
  handleType huid = this->symbolManager().handle(uid);
  this->_saveObjectWithinNodeWithUidAndPossiblyKey(node, huid, key, true, obj);
}

void SaveArchive_O::put(const string &uid, T_sp obj) {
  _G();
  handleType huid = this->symbolManager().handle(uid);
  this->_saveObjectWithinNodeWithUidAndPossiblyKey(this->getTopNode(), huid, "", false, obj);
}

#if 0
    void	SaveArchive_O::putCandoDatabase( CandoDatabase_sp obj )
    { _G();
	string uid = "m:*DATABASE*";
	handleType huid = this->symbolManager().handle(uid);
	this->_saveObjectWithinNodeWithUidAndPossiblyKey(this->getTopNode(),huid,"",false,obj);
    }
#endif

/*! Create all of the objects in the top level nodes.
 */
void LoadArchive_O::createContents() {
  _G();
  VectorNodes::iterator ni;
  T_sp obj;
  if (this->_Verbosity > 0) {
    _lisp->print(BF(" Creating archive contents"));
  }
  LOG(BF("Topnode uid(%s) node name(%s)") % this->getTopNode()->getUniqueIdCharacters() % this->getTopNode()->getNodeName().c_str());
  for (ni = this->getTopNode()->begin_Children();
       ni != this->getTopNode()->end_Children(); ni++) {
    LOG(BF("About to load object uid(%s) nodeName(%s)") %
        (*ni)->getUniqueIdCharacters() % (*ni)->getNodeName().c_str());
    obj = this->loadObjectDirectly(*ni);
  }
  this->finalizeObjects();
#if 0
	// 
	// I used to look for m:*DATABASE* in the top level and automatically set the 
	// system database to this.  I won't do this automatically anymore
	//
	if ( this->contains("m:*DATABASE*") )
	{
	    // If there is an object with uid==m:*DATABASE* then load it as
	    // the database
	    T_sp obj = this->get("m:*DATABASE*");
	    // Do something here now that CandoDatabase class is moved into chem
	    IMPLEMENT_ME();
	    if ( obj->isAssignableTo<CandoDatabase_O>() )
	    {
		Lisp_sp lisp = _lisp;
		ASSERTNOTNULL(lisp);
		ASSERTP(lisp->notNil(),"Default Environment is nil and it should never be nil");
		lisp->setCandoDatabase(obj->as<CandoDatabase_O>());
		_lisp->print(BF("Loaded archive with m:*DATABASE*-> replaced system CandoDatabase"));
	    }
	}
#endif
}

/*! Get all of the objects in the database as an ObjectList.
 */
void LoadArchive_O::getContents(ObjectVector &objectList) {
  VectorNodes::iterator ni;
  T_sp obj;
  objectList.clear();
  for (ni = this->getTopNode()->begin_Children();
       ni != this->getTopNode()->end_Children(); ni++) {
    obj = this->loadObjectDirectly(*ni);
    if ((*ni)->getUniqueIdCharacters() != "m:*DATABASE*")
      objectList.push_back(obj);
  }
}

#if 0
    Render_sp	LoadArchive_O::rendered(Cons_sp options)
    {_G();
	ObjectVector		ov;
	ObjectVectorIterator	it;
	DisplayList_sp	render;
	this->getContents(ov);
	render = DisplayList_O::create();
	for ( it=ov.begin(); it!=ov.end(); it++ )
	{
	    if ( (*it)->canRender() )
	    {
		render->append((*it)->rendered(options));
	    }
	}
	return((render));
    }
#endif

/*!	Complete all of the weak object references
 * Count the weakPointer references that we cannot complete.
 */
void SaveArchive_O::completeWeakObjectReferences() {
  _G();
  vector<SaveWeakPointerEntry>::iterator wpi;
  ArchiveP destinationNode;
  T_sp redirectedObject;
  this->_BrokenWeakPointers = 0;
  for (wpi = this->_WeakObjectReferences.begin();
       wpi != this->_WeakObjectReferences.end(); wpi++) {
    ASSERTNOTNULL(wpi->_Object);
    if (wpi->_Object->isNil()) {
      LOG(BF("WEAK_FAIL: NULL link for weak pointer for attribute: %s") % wpi->_Attribute.c_str());
      if (!wpi->_SuppressAttributeForNilWeakPointers) {
        wpi->_Node->addAttribute(wpi->_Attribute, WeakPointerNull);
      }
      if (wpi->_SuppressNodeForBrokenOrNilWeakPointers) {
        wpi->_Node->setSuppressNode(true);
      }
    } else {
      LOG(BF("WEAK_SUCCESS"));
      //
      // Check if the weak pointer points to an object in the Archive
      //
      if (this->_ObjectNodes.count(wpi->_Object.get()) > 0) {
        LOG(BF("WEAK_SUCCESS: Linking weak pointer for attribute: %s") % wpi->_Attribute.c_str());
        destinationNode = this->_ObjectNodes[wpi->_Object.get()];
        wpi->_Node->addAttribute(wpi->_Attribute,
                                 destinationNode->pathRelativeTo(wpi->_Node));
      } else
      //
      // If the weak pointer doesn't point to an object in the archive
      // then it is Broken
      //
      {
        // If we are supposed to ignore this node when its _NecessaryWeakPointer is broken
        // then We check if the _NecessaryWeakPointer is the current weak pointe
        // and since the test above says that it's broken we reset it so that
        // when the node comes up to be written it can be ignored
        // See Archive_ONode::writeXml
        //
        LOG(BF("WEAK_FAIL: NULL link for weak pointer for attribute: %s") % wpi->_Attribute.c_str());
        if (wpi->_SuppressNodeForBrokenOrNilWeakPointers) {
          wpi->_Node->setSuppressNode(true);
          this->_BrokenWeakPointers++;
        } else {
          wpi->_Node->addAttribute(wpi->_Attribute, WeakPointerBroken);
        }
      }
    }
  }
}

/*!
 * 	- Loading a weak pointer
 * 		-# Read the weak pointer tag from the Node
 * 		-# If the pointer tag == NULL then Return an empty pointer.
 * 		-# Identify the Node that corresponds to the weak pointer tag
 * 		-# If the node says the object has already been created
 * 			then Return the object address (tag now swizzled)
 * 		-# Otherwise create the object from the node.
 * 		-# Return the address of the object (tag now swizzled).
 */
void Archive_O::loadWeakPointerInAttribute(ArchiveP node, const string &attr,
                                           T_wp &obj, Symbol_sp expectedClassSymbol, bool errorIfAttributeMissing) {
  _G();
  string attrValue;
  ArchiveP linkedNode;
  attrValue = node->getAttributeStringDefault(attr, "");
  LOG(BF("Looking up link attribute(%s) path(%s)") % attr.c_str() % attrValue.c_str());
  LOG(BF("In node at line number: %d") % node->getLineNumber());
  if (attrValue == WeakPointerNull || attrValue == WeakPointerBroken) {
    LOG(BF("It's not a link I can follow so I'm setting it to Null"));
    obj = T_O::_nil;
    return;
  }
  if (attrValue == "") {
    if (errorIfAttributeMissing) {
      SIMPLE_ERROR(BF("Attribute[%s] is missing") % attr);
    }
    obj = T_O::_nil;
    return;
  }
  linkedNode = node->followLinkPath(attrValue, node);
  if (linkedNode == NULL) {
    stringstream serr;
    serr << "Weak pointer for attr(" << attr << ") must point to a "
         << _lisp->classFromClassSymbol(expectedClassSymbol)->getPackagedName() << endl;
    serr << "I climbed the tree and couldn't find it" << endl;
    SIMPLE_ERROR(BF("%s") % serr.str());
  }

  if (linkedNode->objectHasBeenCreated()) {
    //	IMPLEMENT_ME(); //  Make sure this works with new hierarchy stuff
    if (!linkedNode->getObject()->isAssignableToByClassSymbol(expectedClassSymbol)) {
      LOG(BF("Problem loading weak pointer - expectedClassSymbol(%d) - linkedNode->getObject()->classSymbol()(%d)") % expectedClassSymbol % linkedNode->getObject()->classSymbol());
      stringstream serr;
      serr << "Weak pointer for attr(" << attr << ") must point to a "
           << _lisp->classFromClassSymbol(expectedClassSymbol)->getPackagedName() << endl;
      serr << "Instead it points to an existing object of class: "
           << linkedNode->getObject()->className() << endl;
      serr << "It was defined on line number: "
           << linkedNode->getLineNumber() << endl;
      SIMPLE_ERROR(BF("%s") % serr.str());
    }
    LOG(BF("The object has been created and its the right class, I'll just pull it up"));
    obj = linkedNode->getObject();
    return;
  } else {
    LOG(BF("The object needs to be created or it's not the object we want"));
  }
  // If the linkedNode is not an Object then throw an error
  if (!_lisp->isClassName(linkedNode->getNodeName())) {
    stringstream serr;
    serr << "Weak pointer for attr(" << attr << ") must point to a " << _lisp->classFromClassSymbol(expectedClassSymbol)->getPackagedName() << endl;
    serr << "Instead it points to non-object node: "
         << linkedNode->description() << endl;
    SIMPLE_ERROR(BF("%s") % serr.str());
  } else {
    LOG(BF("Ok, the weak pointer points to an object node: %s") % linkedNode->getNodeName().c_str());
  }
  if (!_lisp->subClassOrder(expectedClassSymbol, _lisp->getClassSymbolForClassName(linkedNode->getNodeName()))) {
    stringstream serr;
    serr << "Weak pointer for attr(" << attr << ") must point to a "
         << _lisp->classFromClassSymbol(expectedClassSymbol)->getPackagedName() << endl;
    serr << "Instead it points to object node: "
         << linkedNode->description() << endl;
    SIMPLE_ERROR(BF("%s") % serr.str());
  }

  // The object hasn't been created yet.
  // So tell the node to create its object.
  linkedNode->createYourObject();
  obj = linkedNode->getObject();
  ASSERTNOTNULL(obj);
}

T_sp Archive_O::loadObjectDirectly(ArchiveP node) {
  _G();
  ArchiveP linkedNode;
  string tagValue;
  T_sp obj;
  LOG(BF("Looking at: %s") % node->description());
  tagValue = node->getAttributeStringDefault(ObjectPathTag, "");
  if (tagValue == NilPath) {
    return ((T_O::_nil));
  }
  LOG(BF("looking at node, %s(%s)") % ObjectPathTag % tagValue);
  if (tagValue != "") {
    LOG(BF("Following path to linked node"));
    linkedNode = node->followLinkPath(tagValue, node);
  } else {
    LOG(BF("Using this node to load object"));
    // If there was no link then this is the data node
    linkedNode = node;
  }
  LOG(BF("Node describing object: %s") % linkedNode->description());
  if (linkedNode->objectHasBeenCreated()) {
    LOG(BF("Object has already been created, I'm just pulling it up"));
    obj = linkedNode->getObject();
    LOG(BF("Got it, object=%s  .use_count=%d") % obj->description() % obj.use_count());
  } else {
    // The object hasn't been created yet.
    // So tell the node to create its object.
    LOG(BF("Object has not been created yet, calling linkedNode->createYourObject"));
    linkedNode->createYourObject();
    obj = linkedNode->getObject();
    LOG(BF("Got created object.use_count()=%d") % obj.use_count());
  }
  return ((obj));
}

/*!
 * 	- Loading
 * 		-# Find the child with the desired nodeName
 * 		-# Extract the shared pointer tag from the Node
 * 		-# Identify the Node that corresponds to the pointer tag
 * 		-# If the node says the object has already been created
 * 			then Return the object address (tag now swizzled)
 * 		-# Otherwise create the object from the node.
 * 		-# Return the address of the object (tag now swizzled).
 */
void Archive_O::loadObjectWithinNodeWithUid(ArchiveP node, const string &uid, T_sp &obj) {
  _G();
  ArchiveP childNode;
  LOG(BF("Looking at node: %s") % node->description());
  handleType huid = this->symbolManager().handle(uid);
  childNode = node->childWithUniqueId(huid);
  childNode->setRecognized(true);
  obj = this->loadObjectDirectly(childNode);
}

void Archive_O::loadOnlyObjectOfClassWithinNode(ArchiveP node, Symbol_sp classSymbol, T_sp &obj) {
  _G();
  ArchiveP childNode;
  LOG(BF("Looking at node: %s") % node->description().c_str());
  childNode = node->childWithUniqueNodeName(_lisp->classFromClassSymbol(classSymbol)->getPackagedName());
  childNode->setRecognized(true);
  obj = this->loadObjectDirectly(childNode);
}

bool LoadArchive_O::contains(const string &uid) {
  _G();
  return ((this->getTopNode()->hasChildWithUniqueId(uid)));
}

T_sp LoadArchive_O::get(const string &uid) {
  _G();
  T_sp obj;
  this->loadObjectWithinNodeWithUid(this->getTopNode(), uid, obj);
  return ((obj));
}

void Archive_O::addError(const string &errorMessage, ArchiveP node) {
  this->_HasError = true;
  this->_ErrorStream << "ERROR:  " << errorMessage << endl;
  this->_ErrorStream << " file(" << this->getFileName() << ")";
  this->_ErrorStream << " line(" << node->getLineNumber() << ")";
  this->_ErrorStream << " nodeName(" << node->getNodeName() << ")\n";
}

EXPOSE_CLASS(core, Archive_O);
EXPOSE_CLASS(core, SaveArchive_O);
EXPOSE_CLASS(core, LoadArchive_O);

}; // namespace core
