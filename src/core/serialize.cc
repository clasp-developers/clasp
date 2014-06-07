#define	DEBUG_LEVEL_FULL

#include "foundation.h"
#include "object.h"
#include "hashTable.h"
#include "str.h"
#include "arguments.h"
#include "hashTableEq.h"
#include "symbolTable.h"
#include "serialize.h"

#include "wrappers.h"


namespace core 
{

    SNode_sp getOrCreateSNodeForObjectIncRefCount(T_sp val)
    {
	SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
	return saveArchive->getOrCreateSNodeForObjectIncRefCount(val);
    }

    SYMBOL_EXPORT_SC_(KeywordPkg,podSymbolMap);

    EXPOSE_CLASS(core,SNode_O);

    SNode_sp SNode_O::makeAppropriateSNode(T_sp val, HashTable_sp objToSNodeMap)
    {_G();
	if ( val.nilp() ||
	     val.isA<Fixnum_O>() ||
	     val.isA<Number_O>() ||
	     val.isA<Str_O>() ||
	     val.isA<Symbol_O>() ) {
	    LeafSNode_sp lnode = LeafSNode_O::create(val);
	    lnode->incRefCount();
	    objToSNodeMap->hash_table_setf_gethash(val,lnode);
	    return lnode;
	}
	BranchSNode_sp branchSNode = BranchSNode_O::create();
	branchSNode->_Kind = val->_instanceClass()->className();
	branchSNode->incRefCount();
	objToSNodeMap->hash_table_setf_gethash(val,branchSNode);
	val->archiveBase(branchSNode);
	return branchSNode;
    }

    void SNode_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<SNode_O>()
	    .def("setKind",&SNode_O::setKind)
	    .def("getKind",&SNode_O::getKind)
	    .def("getVectorSNodes",&SNode_O::getVectorSNodes)
	    .def("setVectorSNodes",&SNode_O::setVectorSNodesUnsafe)
	    .def("setAttributes",&SNode_O::setAttributesUnsafe)
	    .def("getAttributes",&SNode_O::getAttributes)
	    .def("getUniqueId",&SNode_O::getUniqueId)
	    .def("childWithUniqueId",&SNode_O::childWithUniqueId)
	    .def("keys",&SNode_O::keys)
	    .def("object",&SNode_O::object)
	    ;
    }
    void SNode_O::exposePython(Lisp_sp lisp)
    {_G();
    }


    SNode_sp SNode_O::createBranchSNode(Symbol_sp kind)
    {
	SNode_sp snode = BranchSNode_O::create(kind,_Nil<Cons_O>(),_Nil<Vector_O>());
	return snode;
    }


    bool SNode_O::loading() const
    {
	return Archive_O::currentArchive()->loading();
    }


    void SNode_O::needsFinalization() const
    {
	if ( this->loading() ) {
	    LoadArchive_sp loadArchive = Archive_O::currentLoadArchive();
	    loadArchive->needsFinalization(this->asSmartPtr());
	}
    }
	    



    BranchSNode_sp BranchSNode_O::create()
    {_G();
	GC_ALLOCATE(BranchSNode_O,v);
	return v;
    }


    BranchSNode_sp BranchSNode_O::create(Symbol_sp kind, Cons_sp plist, Vector_sp data)
    {_G();
	GC_ALLOCATE(BranchSNode_O,v);
	v->_Kind = kind;
	v->_SNodePList = plist;
	v->_VectorSNodes = data;
	return v;
    }

    
    EXPOSE_CLASS(core,BranchSNode_O);

    void BranchSNode_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<BranchSNode_O>("make-branch-snode")
	    ;
    }
    void BranchSNode_O::exposePython(Lisp_sp lisp)
    {_G();
    }


    T_sp BranchSNode_O::object() const
    {
	LoadArchive_sp archive = Archive_O::currentLoadArchive();
	BranchSNode_sp me = this->asSmartPtr();
	T_sp obj = archive->loadObjectDirectly(me);
	return obj;
    }

    /*! Push the SNode into the _VectorSNodes */
    void BranchSNode_O::pushVectorSNode(SNode_sp snode)
    {
	SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
	if ( this->_VectorSNodes.nilp() ) {
	    this->_VectorSNodes = VectorObjectsWithFillPtr_O::make(_Nil<T_O>(),_Nil<Cons_O>(),0,0,true);
	}
	this->_VectorSNodes->vectorPushExtend(snode);
    }


    /*! Convert the object into an SNode and push it into _VectorSNodes */
    void BranchSNode_O::pushVector(T_sp obj)
    {
	SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
	SNode_sp snode = saveArchive->getOrCreateSNodeForObjectIncRefCount(obj);
	if ( this->_VectorSNodes.nilp() ) {
	    this->_VectorSNodes = VectorObjectsWithFillPtr_O::make(_Nil<T_O>(),_Nil<Cons_O>(),0,0,true);
	}
	this->pushVectorSNode(snode);
    }
	
    Cons_sp BranchSNode_O::keys() const
    {
	Cons_sp keys = _Nil<Cons_O>();
	for ( Cons_sp cur = this->_SNodePList;cur.notnilp();cur=cCddr(cur))
	{
	    keys = Cons_O::create(oCar(cur),keys);
	}
	return keys;
    }

    void BranchSNode_O::addAttributeSNode(Symbol_sp name, SNode_sp snode)
    {
	SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
	this->_SNodePList = Cons_O::create(snode,this->_SNodePList);
	this->_SNodePList = Cons_O::create(name,this->_SNodePList);
    }

    void BranchSNode_O::addAttribute(Symbol_sp name, T_sp val)
    {
	SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
	SNode_sp snode = saveArchive->getOrCreateSNodeForObjectIncRefCount(val);
	this->_SNodePList = Cons_O::create(snode,this->_SNodePList);
	this->_SNodePList = Cons_O::create(name,this->_SNodePList);
    }



    SNode_sp BranchSNode_O::getAttributeSNode(Symbol_sp name, SNode_sp defaultValue) const
    {
	SNode_sp result = this->_SNodePList->getf(name,_Unbound<T_O>()).as<SNode_O>();
	if ( result.unboundp() ) {
	    return defaultValue;
	}
	return result;
    }

    SNode_sp BranchSNode_O::getAttributeSNodeOrError(Symbol_sp name) const
    {
	SNode_sp result = this->getAttributeSNode(name,_Unbound<SNode_O>());
	if (result.unboundp()) {
	    SIMPLE_ERROR(BF("Could not find attribute with name %s") % _rep_(name));
	}
	return result;
    }

    T_sp BranchSNode_O::getAttribute(Symbol_sp name, T_sp defaultValue) const
    {
	SNode_sp result = this->_SNodePList->getf(name,_Unbound<T_O>()).as<SNode_O>();
	if ( result.unboundp() ) {
	    return defaultValue;
	}
	return result->object();
    }

    void BranchSNode_O::loadVector(gctools::Vec0<T_sp>& vec)
    {
	LoadArchive_sp loadArchive = Archive_O::currentLoadArchive();
	vec.clear();
	for (int i(0), iEnd(this->_VectorSNodes->length()); i<iEnd; ++i ) {
	    SNode_sp snode = this->_VectorSNodes->elt(i).as<SNode_O>();
	    vec.push_back(snode->object());
	};
    }

    /*! For each element in the _VectorSNodes, convert it to an object and
      pass it to the function */
    void BranchSNode_O::mapVector(std::function<void(T_sp)> const& fn)
    {
        if ( this->_VectorSNodes.notnilp() )
        {
            for (int i(0), iEnd(this->_VectorSNodes->length()); i<iEnd; ++i ) {
                SNode_sp snode = this->_VectorSNodes->elt(i).as<SNode_O>();
                T_sp obj = snode->object();
                fn(obj);
            };
        }
    };	


    void BranchSNode_O::saveVector(gctools::Vec0<T_sp> const& vec)
    {
	SaveArchive_sp saveArchive = Archive_O::currentSaveArchive();
	this->_VectorSNodes = VectorObjectsWithFillPtr_O::make(_Nil<T_O>(),_Nil<Cons_O>(),0,0,true);
	for ( auto it=vec.begin(); it!=vec.end(); it++ ) {
	    SNode_sp snode = saveArchive->getOrCreateSNodeForObjectIncRefCount(*it);
	    this->_VectorSNodes->vectorPushExtend(snode);
	};
    }




    T_sp BranchSNode_O::createObject(HashTable_sp snodeToObject)
    {_G();
	SYMBOL_EXPORT_SC_(CorePkg,serialize);
	Class_sp cl = af_findClass(this->_Kind);
	BranchSNode_sp me = this->asSmartPtr();
	T_sp obj = cl->make_instance();
	snodeToObject->hash_table_setf_gethash(me,obj);
	obj->archiveBase(me);
	return obj;
    };



    string BranchSNode_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << _rep_(this->__class()->className()) << " ";
	ss << ":kind " << _rep_(this->_Kind) << " ";
	ss << ":plist " << _rep_(this->_SNodePList) << " ";
	ss << ":vector " << _rep_(this->_VectorSNodes) << ">";
	return ss.str();
    }


#if 0
    Vector_sp BranchSNode_O::getVector()
    {
	VectorObjects_sp result = VectorObjects_O::create(_Nil<T_O>(),this->_VectorSNodes->length(),cl::_sym_T_O);
	for (int i(0),iEnd(this->_VectorSNodes->length());i<iEnd;++i)
	{
	    result->setf_elt(i,this->_VectorSNodes->elt(i).as<SNode_O>()->object());
	}
	return result;
    }
#endif


    T_sp BranchSNode_O::getUniqueId() const
    {
	for (Cons_sp cur=this->_SNodePList; cur.notnilp(); cur=cCddr(cur) )
	{
	    Symbol_sp propertyName = oCar(cur).as<Symbol_O>();
	    if ( propertyName == kw::_sym__uid ) {
		T_sp property = oCadr(cur);
		return property;
	    }
	}
	return _Nil<T_O>();
    }

    SNode_sp BranchSNode_O::childWithUniqueId(Symbol_sp uid) const
    {
	for (int i(0),iEnd(this->_VectorSNodes->length());i<iEnd;++i)
	{
	    SNode_sp child = this->_VectorSNodes->elt(i).as<SNode_O>();
	    if ( child->getUniqueId() == uid ) {
		return child;
	    }
	}
	return _Nil<SNode_O>();
    }


    LeafSNode_sp LeafSNode_O::create(T_sp obj)
    {_G();
	GC_ALLOCATE(LeafSNode_O,v);
	v->_Object = obj;
	return v;
    }



    EXPOSE_CLASS(core,LeafSNode_O);

    void LeafSNode_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<LeafSNode_O>()
	    ;
    }
    void LeafSNode_O::exposePython(Lisp_sp lisp)
    {_G();
    }


    string LeafSNode_O::__repr__() const
    {
	return _rep_(this->_Object);
    }




    Archive_O::Archive_O() : _Version(0), _TopNode(_Nil<BranchSNode_O>()), _NextUniqueId(0) {};
    Archive_O::~Archive_O() {};

    EXPOSE_CLASS(core,Archive_O);

    void Archive_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<Archive_O>()
	    ;
    }
    void Archive_O::exposePython(Lisp_sp lisp)
    {_G();
    }

    string Archive_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << _rep_(this->__class()->className()) << " ";
	ss << _rep_(this->_TopNode) << ">";	
	return ss.str();
    }


    EXPOSE_CLASS(core,SaveArchive_O);

    void SaveArchive_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<SaveArchive_O>("make-save-archive")
	    .def("put",&SaveArchive_O::put)
	    ;
    }
    void SaveArchive_O::exposePython(Lisp_sp lisp)
    {_G();
    }


    Archive_sp Archive_O::currentArchive()
    {
	SYMBOL_EXPORT_SC_(CorePkg,STARserializerArchiveSTAR);
	Archive_sp archive = _sym_STARserializerArchiveSTAR->symbolValue().as<Archive_O>();
	if ( archive.nilp() ) {
	    SIMPLE_ERROR(BF("You must define core:*serializer-archive*"));
	}
	return archive;
    }

    LoadArchive_sp Archive_O::currentLoadArchive()
    {
	Archive_sp archive = Archive_O::currentArchive();
	if ( LoadArchive_sp loadArchive = archive.asOrNull<LoadArchive_O>() ) return loadArchive;
	SIMPLE_ERROR(BF("The value of *serializer-archive* is not a load-archive - its value is: %s") % _rep_(archive));
    }


    SaveArchive_O::SaveArchive_O() 
    {
	this->_SNodeForObject = HashTable_O::create(cl::_sym_eq);
	this->_TopNode = BranchSNode_O::create();
    }


    SaveArchive_sp Archive_O::currentSaveArchive()
    {
	Archive_sp archive = Archive_O::currentArchive();
	if ( SaveArchive_sp saveArchive = archive.asOrNull<SaveArchive_O>() ) return saveArchive;
	SIMPLE_ERROR(BF("The value of *serializer-archive* is not a save-archive"));
    }


    void SaveArchive_O::put(Symbol_sp name, T_sp val)
    {
	DynamicScopeManager scope(_sym_STARserializerArchiveSTAR,this->asSmartPtr());
	this->_TopNode->addAttribute(name,val);
    }




    SNode_sp SaveArchive_O::getOrCreateSNodeForObjectIncRefCount(T_sp val)
    {
	SNode_sp snode = this->_SNodeForObject->gethash(val,_Unbound<T_O>()).as<SNode_O>();
	if (snode.unboundp()) {
	    snode = SNode_O::makeAppropriateSNode(val,this->_SNodeForObject);
	} else {
	    snode->incRefCount();
	}
	return snode;
    }



    EXPOSE_CLASS(core,LoadArchive_O);

    void LoadArchive_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<LoadArchive_O>()
	    .def("keys",&LoadArchive_O::keys)
	    .def("get",&LoadArchive_O::get)
	    .def("contains",&LoadArchive_O::contains)
	    .def("getContents",&LoadArchive_O::getContents)
	    ;
    }
    void LoadArchive_O::exposePython(Lisp_sp lisp)
    {_G();
    }

    void LoadArchive_O::initialize()
    {
        this->Base::initialize();
        this->_NodesToFinalize = HashTableEq_O::create_default();
    }

    void LoadArchive_O::addNodeToFinalize(SNode_sp node)
    {_G();
	IMPLEMENT_ME();
    }

    T_sp LoadArchive_O::loadObjectDirectly(SNode_sp node)
    {
	if ( LeafSNode_sp leafSNode = node.asOrNull<LeafSNode_O>() ) {
	    return node->object();
	}
	BranchSNode_sp branchSNode = node.as<BranchSNode_O>();
	if ( node == this->_TopNode ) {
	    // Don't create an object for the top node - just for it's children
	    for ( Cons_sp cur = branchSNode->_SNodePList; cur.notnilp(); cur=cCddr(cur) ) {
		this->loadObjectDirectly(oCadr(cur).as<SNode_O>());
	    }
	    if ( branchSNode->_VectorSNodes.notnilp()) {
		for ( int i(0),iEnd(branchSNode->_VectorSNodes->length());i<iEnd;++i) {
		    this->loadObjectDirectly(branchSNode->_VectorSNodes->elt(i).as<SNode_O>());
		}
	    }
	    return _Nil<T_O>();
	} else {
	    T_sp findObj = this->_ObjectForSNode->gethash(branchSNode,_Unbound<T_O>());
	    if ( findObj.unboundp() ) {
		findObj = branchSNode->createObject(this->_ObjectForSNode);
//		this->_ObjectForSNode->hash_table_setf_gethash(branchSNode,findObj);
	    }
	    return findObj;
	}
	UNREACHABLE();
    }
	
    void LoadArchive_O::createContents()
    {_G();
	this->_ObjectForSNode = HashTable_O::create(cl::_sym_eq);
	this->loadObjectDirectly(this->_TopNode);
    }


    void LoadArchive_O::needsFinalization(SNode_sp node)
    {
	this->_NodesToFinalize->setf_gethash(node,_Nil<T_O>());
    }

    void LoadArchive_O::finalizeObjects()
    {_G();
        T_sp obj;
        this->_NodesToFinalize->mapHash( [&obj] (T_sp node, T_sp dummy) {
                node.as<SNode_O>()->object()->loadFinalize(node.as<SNode_O>());
            } );
#if 0
	this->_NodesToFinalize.map( [] (gctools::smart_ptr<SNode_O> node)  {
		T_sp obj = node->object();
		obj->loadFinalize(node);
	    });
#endif
    }

    bool LoadArchive_O::contains(Symbol_sp sym)
    {_G();
	if ( this->_TopNode.nilp() ) SIMPLE_ERROR(BF("No archive is loaded"));
	if ( BranchSNode_sp bnode = this->_TopNode.asOrNull<BranchSNode_O>()) {
	    T_sp property = bnode->_SNodePList->getf(sym,_Unbound<T_O>());
	    if (property.unboundp()) {
		return false;
	    }
	    return true;
	}
	return false;
    }

    T_sp LoadArchive_O::get(Symbol_sp sym)
    {_G();
	if ( this->_TopNode.nilp() ) SIMPLE_ERROR(BF("No archive is loaded"));
	DynamicScopeManager scope(_sym_STARserializerArchiveSTAR,this->asSmartPtr());
	if ( BranchSNode_sp bnode = this->_TopNode.asOrNull<BranchSNode_O>()) {
	    SNode_sp property = bnode->_SNodePList->getf(sym,_Unbound<T_O>()).as<SNode_O>();
	    if (property.unboundp()) {
		SIMPLE_ERROR(BF("Could not find property for key: %s") % _rep_(sym));
	    }
	    return property->object();
	}
	SIMPLE_ERROR(BF("Could not find property for key: %s") % _rep_(sym));
    }

    Cons_sp LoadArchive_O::getContents()
    {_G();
	if ( this->_TopNode.nilp() ) SIMPLE_ERROR(BF("No archive is loaded"));
	if ( BranchSNode_sp bnode = this->_TopNode.asOrNull<BranchSNode_O>()) {
	    return bnode->_SNodePList;
	}
	return _Nil<Cons_O>();
    }

    Cons_sp LoadArchive_O::keys() const
    {
	if ( this->_TopNode.nilp() ) {
	    SIMPLE_ERROR(BF("There is no archive loaded"));
	}
	return this->_TopNode->keys();
    }



};
