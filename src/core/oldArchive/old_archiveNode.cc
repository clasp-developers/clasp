/*
    File: old_archiveNode.cc
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
#define	DEBUG_LEVEL_NONE

#include "foundation.h"
#include "core/corePackage.h"
#include "object.h"
#include "lisp.h"
#include <string.h>
#include "archiveNode.h"
#include "symbolTable.h"
#include "builtInClass.h"
#include "standardClass.h"
//#i n c l u d e "color.h"
#include "ql.h"
#include "archive.h"
#include "standardObject.h"
#include "evaluator.h"
#include "primitives.h"
#include "forwardReferencedClass.h"
#include "wrappers.h"

#define EPSILON		1.0e-20
#define	BUFFER_MAX	1024

namespace core {

const string&	AttributeMap::name(const Dumb_Node* node, AttributeMap::iterator it)
{
    RET_POD((node->archive_const().symbolManager().textSymbol(it->first)));

}

const string&	AttributeMap::name_const(const Dumb_Node* node, AttributeMap::const_iterator it) const
{
    RET_POD((node->archive_const().symbolManager().textSymbol(it->first)));
}

const char*	AttributeMap::value(const Dumb_Node* node, AttributeMap::iterator it) const
{
    return((node->archive().multiStringBuffer()->getCharacters(it->second)));
}

const char*	AttributeMap::value(const Dumb_Node* node, AttributeMap::const_iterator it) const
{
    return((node->archive().multiStringBuffer()->getCharacters(it->second)));
}



const char* AttributeMap::get(Dumb_Node* node, const string& name)
{
    handleType h = node->archive().symbolManager().handle(name);
    for (AttributeMap::iterator ii=this->_Entries.begin(); ii!=this->_Entries.end(); ii++ )
    {
	if ( ii->first == h ) 
	{
	    return((node->archive().multiStringBuffer()->getCharacters(ii->second)));
	}
    }
    RET_POD((NULL));
}

bool AttributeMap::contains(Dumb_Node* node, const string& name)
{
    return((this->get(node,name)!=NULL));
}

void AttributeMap::set(Dumb_Node* node, const string& name, const string& val)
{_G();
    handleType h = node->archive().symbolManager().handle(name);
    if ( h == UniqueIdHandle ) 
    {
	SIMPLE_ERROR(BF("I was asked to set the UniqueId attribute but we don't do that anymore.  UniqueId attributes are set at the Node directly"));
    }
#if !PRODUCTION_CODE
    	//
	// This is a test to avoid adding duplicate attributes
	// In the production code we should never have duplicate attributes
	// currently there are a few in constitution.cc and topology.cc fragment.cc frame.cc frameRecognizer.cc plug.cc
    for (AttributeMap::iterator ii=this->_Entries.begin(); ii!=this->_Entries.end(); ii++ )
    {
	if ( ii->first == h ) return;
    }
#endif
    uint valueIndex = node->archive().multiStringBuffer()->addString(val);
    this->_Entries.push_back(pair<handleType,uint>(h,valueIndex));
}

void AttributeMap::setFromXml(Dumb_Node* node, const string& name, const string& val)
{
    handleType h = node->archive().symbolManager().handle(name);
    if ( h == UniqueIdHandle ) 
    {
	node->setTextUniqueId(val);
	return;
    }
    uint valueIndex = node->archive().multiStringBuffer()->addString(val);
    this->_Entries.push_back(pair<handleType,uint>(h,valueIndex));
}



class MemoryUsageTracker
{
public:
	uint	nodeCount;
	uint 	nodeNames;
	uint	characterStructure;
	uint	pointers;
	uint	lineNumbersFlags;
	uint	attributesStructure;
	uint	attributesKeys;
	uint	attributesText;
	uint	uidMap;
	uint	focus;
	uint	countAttributes;
	LongLongInt totalBytes;
public:
	void reset();
	LongLongInt describeMemoryUsage(const Lisp_sp& lisp);
	void sumUsageForNode(Dumb_Node* node);
};


void MemoryUsageTracker::reset()
{
    this->nodeCount = 0;
    this->nodeNames=0;
    this->characterStructure=0;
    this->pointers=0;
    this->lineNumbersFlags=0;
    this->attributesStructure=0;
    this->attributesText=0;
    this->attributesKeys=0;
    this->uidMap=0;
    this->focus=0;
    this->countAttributes=0;
    this->totalBytes=0;
}


LongLongInt MemoryUsageTracker::describeMemoryUsage(const Lisp_sp& lisp)
{
    LongLongInt total = 0;
    lisp->print(BF("       nodeNames   = %u") %  nodeNames );
    total += nodeNames;
    lisp->print(BF("characterStructures= %u") %  characterStructure );
    total += characterStructure;
    lisp->print(BF("          pointers = %u") %  pointers );
    total += pointers;
    lisp->print(BF("  lineNumbersFlags = %u") %  lineNumbersFlags );
    total += lineNumbersFlags;
    lisp->print(BF("attributesStructure= %u") %  attributesStructure );
    total += attributesStructure;
    lisp->print(BF("     attributesKeys= %u") %  attributesKeys );
    total += attributesKeys;
    lisp->print(BF("     attributesText= %u") %  attributesText );
    total += attributesText;
    lisp->print(BF("          uidMap   = %u") %  uidMap );
    total += uidMap;
    lisp->print(BF("           focus   = %u") %  focus );
    total += focus;
    lisp->print(BF("           TOTAL   = %u") %  total );
    lisp->print(BF(" countAttributes   = %u") %  countAttributes );
    RET_POD((total));
}


//
// Figure out some ways to optimize storage
//
// First limit strings to necessary length
//
void MemoryUsageTracker::sumUsageForNode(Dumb_Node* node)
{
    this->nodeCount++;
    this->focus = sizeof(node->_Attributes);
    this->pointers += sizeof(node->_PArchive)+sizeof(node->_Parent)+sizeof(node->_Object);
    this->lineNumbersFlags += sizeof(node->_LineNumber)+sizeof(node->_Flags);
    this->pointers += sizeof(node->_PChildren);
    this->nodeNames += sizeof(node->_NodeHandle);
    this->characterStructure += sizeof(node->_CharactersIndex);
    VectorNodes::iterator	ci;
    for ( ci=node->begin_Children(); ci!=node->end_Children(); ci++ ) 
    {
	this->sumUsageForNode(*ci);
	this->pointers += sizeof((*ci));
    }

	//
    	// Add up the attributes
	//
    AttributeMap::iterator attr;
    this->countAttributes += node->_Attributes.size();
    this->attributesStructure += sizeof(node->_Attributes);
    for ( attr=node->_Attributes.begin(); attr!=node->_Attributes.end(); attr++ )
    {
	this->attributesKeys += sizeof(attr->first);
	this->attributesText += sizeof(attr->second);
    }
    map<string,ArchiveP>::iterator	uidi;
}




ChildHolder::ChildHolder()
{
    this->_Children.clear();
    this->_ChildrenByUid.clear();
}

ChildHolder::~ChildHolder()
{    // Do nothing
}

void	ChildHolder::addChild(Dumb_Node* parent, ArchiveP node)
{_G();
    uint nextIdx = this->_Children.size();
    this->_Children.push_back(node);
    if ( node->hasUniqueId() )
    {
	LOG(BF("Child node has a uid(%s)")% node->getUniqueIdCharacters());
	handleType uidHandle = node->getUniqueIdHandle();
	this->_ChildrenByUid[uidHandle] = nextIdx;
    } else
    {
	LOG(BF("Child node did not have uid"));
    }
}

bool	ChildHolder::hasChildWithUniqueId(Dumb_Node* parent, handleType huid )
{
    return((( this->_ChildrenByUid.count(huid) > 0 )));
}

ArchiveP	ChildHolder::childWithUniqueIdOrNull(Dumb_Node* parent, handleType huid )
{
    ArchiveP child = NULL;
    if ( this->_ChildrenByUid.count(huid) == 0 ) return((child));
    uint idx = this->_ChildrenByUid[huid];
    return((this->_Children[idx]));
}


ArchiveP	ChildHolder::childWithUniqueNodeName(Dumb_Node* parent,  const string& nodeName )
{_G();
    ArchiveP			child = NULL;
    VectorNodes::iterator	ci;
    int				cnt;
    cnt = 0;
    handleType hname = parent->archive().symbolManager().handle(nodeName);
    for ( ci=this->begin_Children(parent); ci!=this->end_Children(parent); ci++ ) 
    {
	if ( (*ci)->isNamed(hname) )
	{
	    cnt++;
	    child = (*ci);
	}
    }
    if ( cnt != 1 ) {
	if ( cnt == 0 ) {
	    SIMPLE_ERROR(BF("Could not find child with name: %s")%nodeName);
	}
	SIMPLE_ERROR(BF("There can be only one child with name: ")%nodeName);
    }
    return((child));
}

bool ChildHolder::hasChildWithUniqueNodeName(Dumb_Node* parent,  const string& nodeName )
{
    VectorNodes::iterator	ci;
    handleType hname = parent->archive().symbolManager().handle(nodeName);
    for ( ci=this->begin_Children(parent); ci!=this->end_Children(parent); ci++ ) 
    {
	if ( (*ci)->isNamed(hname) ) return((true));
    }
    return((false));
}


/*! Return the child with the requested node name and key*/
ArchiveP	ChildHolder::childWithNodeNameAndKey(Dumb_Node* parent,  const string& nodeName,
					const string& key )
{_G();
    VectorNodes::iterator	ci;
    string			childKey;
    handleType hname = parent->archive().symbolManager().handle(nodeName);
    for ( ci=this->begin_Children(parent); ci!=this->end_Children(parent); ci++ ) 
    {
	if ( (*ci)->isNamed(hname) )
	{
	    childKey = (*ci)->getAttributeStringDefault("_key","");
	    if ( childKey == key )
	    {
	        return(((*ci)));
	    }
	}
    }
    SIMPLE_ERROR(BF("Could not find child with NodeName(%s) and key (%s)") % nodeName % key);
}

bool	ChildHolder::hasChildrenWithName(Dumb_Node* parent,  const string& name )
{_G();
    VectorNodes::iterator	ci;
    int				cnt;
    bool			ret;
    cnt = 0;
    handleType hname = parent->archive().symbolManager().handle(name);
    for ( ci=this->begin_Children(parent); ci!=this->end_Children(parent); ci++ ) 
    {
	if ( (*ci)->isNamed(hname) )
	{
	    cnt++;
	}
    }
    ret = cnt>0;
    return((ret));
}

uint	ChildHolder::numberOfChildren(Dumb_Node* parent )
{
    return((this->_Children.size()));
}


		// Used for iterating when there are no children
vector<ArchiveP>	_EmptyChildren;






Dumb_Node::Dumb_Node(Archive_O* archive)
{
    this->_PArchive = archive;	// This has to be initialized first!!!!!
    this->_Huid = EmptyStringHandle;
    this->_Object = T_O::_nil;
    this->_Parent = NULL;
    this->_Flags = 0;
    this->setSuppressNode(false);
    this->_PChildren = NULL;
    this->_CharactersIndex = UndefinedUnsignedInt;
    this->_ReferenceCount = 1;
    this->_ReferenceHandle = 0;
}

Dumb_Node::~Dumb_Node()
{
    if ( this->_PChildren != NULL )
    {
		// explicitly call dtor
	this->_PChildren->~ChildHolder();
    }
}

void Dumb_Node::incrementReferenceCountAndAssignReferenceHandle()
{
    this->_ReferenceCount++;
    this->_ReferenceHandle = this->archive().nextReferenceHandle();
}

Lisp_sp Dumb_Node::lisp() const
{
    return((this->archive().lisp()));
}

Lisp_sp Dumb_Node::lisp()
{
    return((this->archive().lisp()));
}

uint	Dumb_Node::numberOfChildren()
{
    if ( this->_PChildren == NULL ) return((0));
    return((this->_PChildren->numberOfChildren(this)));
}

vector<ArchiveP>::iterator	Dumb_Node::begin_Children()
{
    if ( this->_PChildren == NULL ) RET_POD((_EmptyChildren.begin()));
    RET_POD((this->_PChildren->begin_Children(this)));
};

vector<ArchiveP>::iterator	Dumb_Node::end_Children()
{
    if ( this->_PChildren == NULL ) RET_POD((_EmptyChildren.end()));
    RET_POD((this->_PChildren->end_Children(this)));
};

Archive_sp	Dumb_Node::getArchive() const
{_OF();
    ASSERT(this->_PArchive!=NULL);
    return((this->_PArchive->sharedThis<Archive_O>()));
}


Bignum Dumb_Node::getAttributeBignum(const string& at)
    {_G();
	string val = this->getAttributeString(at);
	LOG(BF("Converting string(%s)") % val );
	Bignum bn(val);
	return((bn));
    }





    LongLongInt Dumb_Node::getAttributeLongLongInt(const string& at)
    {_G();
	string val = this->getAttributeString(at);
	LOG(BF("Converting string(%s)") % val );
	LongLongInt lli =  atoll(val.c_str());
	LOG(BF("Converted to long long int=%d") % lli );
	return((lli));
    }


    LongLongInt Dumb_Node::getAttributeLongLongIntDefault(const string& at,LongLongInt& df)
    {_G();
	if ( this->_Attributes.contains(this,at) )
	{
	    string val = this->getAttributeString(at);
	    LOG(BF("Converting string(%s)") % val );
	    return((atoll(val.c_str())));
	} else
	{
	    return((df));
	}
    };





ArchiveP Dumb_Node::newArchiveNode()
{
    return((this->getArchive()->newArchiveNode()));
}

void Dumb_Node::loadOnlyObjectOfClassWithinNode( Symbol_sp classSymbol, T_sp& obj )
{
    Archive_sp arch;
    arch = this->getArchive();
    arch->loadOnlyObjectOfClassWithinNode(this, classSymbol, obj );
}

bool	Dumb_Node::isNamed(const string& name)
{
    uint h = this->getArchive()->symbolManager().handle(name);
    return((h == this->_NodeHandle));
}

void Dumb_Node::setClassNodeName(Class_sp metaClass)
{
    this->setRawNodeName(metaClass->className());
    if (metaClass->isAssignableTo<StandardClass_O>() )
    {
	StandardClass_sp cc = metaClass->as<StandardClass_O>();
	IMPLEMENT_ME(); // I commented out the line below because I needed to eliminate getInstanceCoreClass 
	// Figure out a better mechanism
	// this->addAttribute(Archive_O::CoreBuiltInClassAttribute,cc->getInstanceCoreClass()->className());
    }
};

void Dumb_Node::setClasslessNodeName(const string& nm)
{_OF();
    ASSERTP(!_lisp->isClassName(nm),
	    "setClasslessNodeName node name("+nm+") is a class!!");
    this->setRawNodeName(nm);
};

bool Dumb_Node::hasChildWithNodeNameMatchingSubClassOf(Symbol_sp baseId)
{
    if ( this->numberOfChildren() < 1 ) return((false));
    vector<ArchiveP>::iterator	ci;
//    int classSymbol;
    Class_sp baseClass = _lisp->classFromClassSymbol(baseId);
    for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ )
    {
	Symbol_sp sym = _lisp->intern((*ci)->getNodeName());
	T_sp obj = sym->symbolValue();
	if ( obj->metaClassP() ) 
	{
	    Class_sp mc = obj->as<Class_O>();
	    if ( mc->isSubClassOf(baseClass) )
	    {
		return((true));
	    }
	}
    }
    return((false));
}


    void Dumb_Node::attributeSymbolIfNotNil(const string& key, Symbol_sp& sym)
    {_G();
	    if ( this->saving())
	    {
	    	if ( sym->notNil() )
		{
                    this->addAttribute(key,sym);
		}
	    } else
	    {
		sym = this->getAttributeSymbolDefaultNil(key);
	    }
    }






bool	Dumb_Node::isNamed(handleType hname)
{
    return((hname == this->_NodeHandle));
}

const string& Dumb_Node::getNodeName() const
{
    const string& name = this->getArchive()->symbolManager().textSymbol(this->_NodeHandle);
    RET_POD((name));
}

void	Dumb_Node::setRawNodeName(const string& nm)
{
    uint h = this->getArchive()->symbolManager().handle(nm);
    this->_NodeHandle = h;
}



const char* Dumb_Node::characters()
{
    return((this->archive().multiStringBuffer()->getCharacters(this->_CharactersIndex)));
}


#if 0
string Dumb_Node::oneLineOfCharacters(const char** cur)
{
    stringstream ss;
    while ( **cur && **cur != '\n' )
    {
	ss << **cur;
	(*cur)++;
    }
    if ( **cur ) (*cur)++;
    return((ss.str()));
}
#endif


void Dumb_Node::setCharacters(const string& buffer)
{_G();
    LOG(BF("setCharactersstr=(%s) len()=%d")% buffer % buffer.size() );
    uint index = this->archive().multiStringBuffer()->addCharacters(buffer.c_str());
    this->_CharactersIndex = index;
#ifdef	DEBUG_ON
    const char* wrote = this->archive().multiStringBuffer()->getCharacters(index);
    LOG(BF("Wrote str(%s) to MultiStringBuffer")% wrote );
#endif
}






void Dumb_Node::saveOnlyObjectOfClassWithinNode( Symbol_sp classSymbol, T_sp obj )
{
    this->getArchive()->saveOnlyObjectOfClassWithinNode(this, classSymbol, obj );
}

void Dumb_Node::loadObjectWithinNodeWithUid( const string& uid, T_sp& obj )
{
    this->getArchive()->loadObjectWithinNodeWithUid(this, uid, obj );
}


void Dumb_Node::saveObjectWithinNodeWithUid( const string& uid, T_sp obj )
{
    this->getArchive()->saveObjectWithinNodeWithUid(this, uid, obj );
}

void Dumb_Node::saveObjectWithinNodeWithUidAndKey(const string& uid, const string& key, T_sp obj)
{
    this->getArchive()->saveObjectWithinNodeWithUidAndKey(this,uid,key,obj);
}


T_sp Dumb_Node::loadObjectDirectly(ArchiveP node)
{
    return((this->getArchive()->loadObjectDirectly(node)));
}

void 	Dumb_Node::loadWeakPointerInAttribute( ArchiveP node, const string& attribute, T_wp& obj, Symbol_sp expectedClassSymbol, bool mustBeDefined )
{
    this->getArchive()->loadWeakPointerInAttribute(node,attribute,obj,expectedClassSymbol,mustBeDefined);
}


uint	Dumb_Node::leafCount()
{
    uint cnt = 0;
    VectorNodes::iterator	ci;
    if ( this->numberOfChildren() == 0 ) return((1));
    for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ ) {
	cnt += (*ci)->leafCount();
    }
    return((cnt));
}


uint	Dumb_Node::nodeCount()
{
    uint cnt = 1; // this node
    VectorNodes::iterator	ci;
    for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ ) {
	cnt += (*ci)->nodeCount();
    }
    return((cnt));
}



LongLongInt Dumb_Node::describeMemoryUsage()
{
    MemoryUsageTracker track;
    track.reset();
    track.sumUsageForNode(this);
    _lisp->print(BF(" Estimated memory usage broken down" ));
    _lisp->print(BF(" Number of symbols in SymbolManager: %d") %  this->archive().symbolManager().numberOfSymbols() );
    this->archive().symbolManager().printDescription(_lisp);
    LongLongInt totalBytes = track.describeMemoryUsage(_lisp);
#define DP(n) {_lisp->print(BF("    %20s = %u bytes") %  #n % sizeof(n) ); }
    _lisp->print(BF("   Memory usage of top node broken down by parts" ));
    DP(this->_PArchive);
    DP(this->_Parent);
    DP(this->_Object);
    DP(this->_NodeHandle);
    DP(this->_LineNumber);
    DP(this->_Flags);
    DP(this->_PChildren);
    DP(this->_Attributes);
    return((totalBytes));
}



void Dumb_Node::saveWeakPointerInAttribute( const string& attribute, 
				T_wp obj, 
    				bool suppressNodeForBrokenOrNilWeakPointers,
				bool suppressAttributeForNilWeakPointers )
{
    this->getArchive()->saveWeakPointerInAttribute(this,attribute,
    				obj, suppressNodeForBrokenOrNilWeakPointers,
				suppressAttributeForNilWeakPointers );
}

uint Dumb_Node::nextUniqueId()
{
    return((this->archive().nextUniqueId()));
}

string Dumb_Node::nextUniqueIdCharacters()
{
    stringstream ss;
    ss << this->archive().nextUniqueId();
    return((ss.str()));
}




void	Dumb_Node::needsFinalization()
{_G();
    if ( this->loading() ) {
        this->getArchive()->addNodeToFinalize(this);
    }
}


void Dumb_Node::addAttributeFloat(const string& at, float val )
{
    ostringstream temp;
    if ( fabs(val)>0.001 && fabs(val) < 10000.0 ) {
	temp << setprecision(8)
	    << setiosflags(ios::fixed)
	    << val;
    } else if ( fabs(val) < EPSILON )
    {
	temp << "0";
    } else {
	temp << setprecision(8)
	    << setiosflags(ios::scientific)
	    << val;
    }
    this->addAttribute(at,temp.str());
};

void Dumb_Node::addAttribute(const string& at, stringstream& val)
{
    this->_Attributes.set(this,at,val.str());
};


    void Dumb_Node::addAttribute(const string& at, Symbol_sp sym)
    {
	if ( sym->isNil() )
	{
	    this->_Attributes.set(this,at,"");
	    return;
	}
	this->_Attributes.set(this,at,sym->__repr__());
    }


#if 0
void Dumb_Node::xml_fwrite( ostream& sout, const string& str)
{_G();
const char*	cp;
    cp = str.c_str();
    while ( *cp )
    {
	switch (*cp)
	{
	    case '<':
	        sout << "&lt;";
	        break;
	    case '>':
	        sout << "&gt;";
		break;
	    case '&':
	        sout << "&amp;";
		break;
	    default:
	        sout << *cp;
		break;
	}
	cp++;
    }
}
#endif


void Dumb_Node::setRecognized(bool b)
{
    if ( b ) this->_Flags |= NODE_Recognized;
    else this->_Flags &= ~(NODE_Recognized);
}

void Dumb_Node::setFinalized(bool b)
{
    if ( b ) this->_Flags |= NODE_Finalized;
    else this->_Flags &= ~(NODE_Finalized);
}

void Dumb_Node::setSuppressNode(bool b)
{
    if ( b ) this->_Flags |= NODE_Suppressed;
    else this->_Flags &= ~(NODE_Suppressed);
}



void Dumb_Node::addAttribute(const string& at, const string& val)
{
    this->_Attributes.set(this,at,val);
};

void Dumb_Node::addAttribute(const string& at, double val ) {
    this->addAttributeDouble(at,val);
};

void Dumb_Node::addAttribute(const string& at, float val ) {
    this->addAttributeFloat(at,val);
};

void Dumb_Node::addAttribute(const string& at, int val ) {
    this->addAttributeInt(at,val);
};

void Dumb_Node::addAttribute(const string& at, LongLongInt val ) {
    this->addAttributeLongLongInt(at,val);
};

void Dumb_Node::addAttribute(const string& at, Bignum& val)
{
    stringstream ss;
    ss << val;
    this->addAttributeString(at,ss.str());
}

void Dumb_Node::addAttribute(const string& at, unsigned int val ) {
    this->addAttributeUInt(at,val);
};

void Dumb_Node::addAttribute(const string& at, unsigned long val ) {
    this->addAttributeULong(at,val);
};

void Dumb_Node::addAttribute(const string& at, bool val ) {
    this->addAttributeBool(at,val);
};


void Dumb_Node::addAttributeString(const string& at, const string& val) {
	    this->addAttribute(at,val);
};
void Dumb_Node::addAttributeDouble(const string& at, double val ) {
    ostringstream temp;
    if ( fabs(val) < EPSILON /* epsilon value */ )
    {
	temp << setiosflags(ios::scientific)
	    << setprecision(6)
	    << val;
    } else if ( fabs(val)>0.00001 && fabs(val) < 10000.0 ) {
	temp << setiosflags(ios::fixed)
	    << setprecision(6)
	    << val;
    } else {
	temp << setiosflags(ios::scientific)
	    << setprecision(8)
	    << val;
    }
    this->addAttribute(at,temp.str());
};


void Dumb_Node::addAttributeInt(const string& at, int val) {
    ostringstream temp;
    temp << std::dec << val;
    this->addAttribute(at,temp.str());
};

void Dumb_Node::addAttributeLongLongInt(const string& at, LongLongInt val) {
    ostringstream temp;
    temp << std::dec << val;
    this->addAttribute(at,temp.str());
};

void Dumb_Node::addAttributeUInt(const string& at, unsigned int val) {
    ostringstream temp;
    if ( val == UndefinedUnsignedInt )
    {
	temp << "undef";
    } else {
        temp << std::dec << val;
    }
    this->addAttribute(at,temp.str());
};


void Dumb_Node::addAttributeULong(const string& at, unsigned long val) {
    ostringstream temp;
    temp << std::dec << val;
    this->addAttribute(at,temp.str());
};

void Dumb_Node::addAttributeHex(const string& at, int val) {
    ostringstream temp;
    temp << std::hex << val << std::dec;
    this->addAttribute(at,temp.str());
};

void Dumb_Node::addAttributeBool(const string& at, bool b ) {
    ostringstream temp;
    if ( b ) {
	    temp << "yes";
    } else {
	    temp << "no";
    }
    this->addAttribute(at,temp.str());
};

string	Dumb_Node::getFileName()
{
    return((this->getArchive()->getFileName()));
}



void	Dumb_Node::setTextUniqueId(const string& uid)
{ _G();
    Dumb_Node* parent;
    handleType huid = this->archive().symbolManager().handle(uid);
    if ( this->hasParent() ) 
    {
        parent = this->getParent();
        if ( parent->hasChildWithUniqueId(huid) ) {
	    SIMPLE_ERROR(BF("Parent node already has uid(%s)") % uid);
        }
    }
    this->_Huid = huid;
}


void	Dumb_Node::setNumericUniqueId(const int uid)
{ _G();
    handleType huid = this->archive().symbolManager().handleFromNumber(uid);
    this->_Huid = huid;
}

void	Dumb_Node::setUniqueIdHandle(handleType huid)
{ _G();
    this->_Huid = huid;
}

void	Dumb_Node::setUniqueIdAutomatically()
{ _G();
    int uid = this->archive().nextUniqueId();
    this->setNumericUniqueId(uid);
}

ArchiveP	Dumb_Node::followLinkPathUpTreeForClass(const string& linkPath,
			ArchiveP startNode)
{_G();
    ArchiveP curNode;
    string lookForClassName = linkPath.substr(1,linkPath.size());
    curNode = startNode;
    while ( 1 )
    {
	if ( curNode->isNamed(lookForClassName) )
	{
	    return((curNode));
	}
		// If there is no parent then Return an empty node
	if ( !curNode->hasParent() ) {
	    SIMPLE_ERROR(BF("Could not find node name(%s)") % lookForClassName);
	}
        curNode = curNode->getParent();
    }
}


uint Dumb_Node::getUniqueIdNumeric()
{
    return((this->archive().symbolManager().numericSymbol(this->_Huid)));
}

string Dumb_Node::getUniqueIdCharactersIfMissingThrow()
{_OF();
    handleType huid = this->getUniqueIdHandle();
    if ( huid == EmptyStringHandle )
    {
	SIMPLE_ERROR(BF("There is an empty UniqueId and there shouldn't be"));
    }
    return((this->archive().symbolManager().anySymbol(huid)));
}

string Dumb_Node::getUniqueIdCharacters()
{
    handleType huid = this->getUniqueIdHandle();
    return((this->archive().symbolManager().anySymbol(huid)));
}


ArchiveP	Dumb_Node::followLinkPathUpTreeForUid(const string& linkPath, ArchiveP startNode)
{_G();
    ArchiveP curNode;
    string lookForUid=linkPath.substr(1,linkPath.size());
    handleType hLookForUid = this->archive().symbolManager().handle(lookForUid);
    curNode = startNode;
    while ( 1 )
    {
	if ( curNode == NULL )
	{
	    SIMPLE_ERROR(BF("Couldn't find uid: "+lookForUid));
	}
	if ( hLookForUid == curNode->getUniqueIdHandle() ) 
	{
	    return((curNode));
	}
		// If there is no parent then Return an empty node
	if ( curNode->getParent() == NULL ) {
	    curNode = NULL;
	}
        curNode = curNode->getParent();
    }
}

/*!
 * There are different ways to represent nodes along a path
 * /identifier1/identifier2/identifier3 - absolute path
 * ../		- Climb one level up the tree
 * ^[uid]	- Climb up the tree until you find the uid
 * %[className] - climb up the tree looking for the class name
 * $[className]([keyName])
 * @[className] - Unique className at current level
 *
 * Also used but not parsed here is: 
 * !nil - represents a nil object (handled in O_PArchive::loadObjectDirectly before followLinkPath is called).
 */
ArchiveP	Dumb_Node::followLinkPath( const string& linkPath, ArchiveP startNode )
{_G();
ArchiveP		curNode;
vector<string>	parts;
vector<string>	mapParts;
stringstream	ss;
string		key;
int		pathIndex;
    tokenize(linkPath,parts,"/");
    pathIndex = 0;
    curNode = startNode;
    if ( parts[0] == "" ) {
	curNode = this->getArchive()->getTopNode();
	pathIndex++;
    } else if ( parts[0][0] == '^' )
    {
	curNode = this->followLinkPathUpTreeForUid(parts[0],curNode);
	pathIndex++;
    } else if ( linkPath[0] == '%' )
    {
	curNode = this->followLinkPathUpTreeForClass(parts[0],curNode);
	pathIndex++;
    }
    	// Do we start at the root?
    for ( uint parti=pathIndex; parti<parts.size(); parti++ ) {
	key = parts[parti];
	if ( key == ".." )
	{
	    curNode = curNode->getParent();
	} else if ( key == "." )
	{
	    	// Do nothing
	} else if ( key[0] == '@' )
	{
	    string nodeName = key.substr(1,9999);
	    curNode = curNode->childWithUniqueNodeName(nodeName);
	} else if ( key[0] == '$' )
	{
		// Parse a map entry
	    tokenize(key,mapParts,"$()");
	    if ( mapParts.size() != 2 )
	    {
	        stringstream ess;
		ess << "Map links must have NodeName(key) format, instead we got: ";
		ess << key << " with " << mapParts.size() << " parts"<<endl;
		for ( uint zi=0; zi< mapParts.size(); zi++ )
		{
		    ess << "Part"<<zi<<"("<<mapParts[zi]<<"), ";
		}
	        SIMPLE_ERROR(BF("%s") % ess.str() );
	    }
	    curNode = curNode->childWithNodeNameAndKey(mapParts[0],mapParts[1]);
	} else {
	    curNode = curNode->childWithUniqueId(key);
	}
    }
    return((curNode));
}


void Dumb_Node::pathFromNodeToRoot(ArchiveP node, vector<Dumb_Node*>& path)
{
    Dumb_Node*	cur;
    path.clear();
    cur = node;
    path.push_back(cur);
    while ( cur->hasParent() ) {
	cur = cur->getParent();
        path.push_back(cur);
    }
}




ArchiveP	Dumb_Node::createChildNode( const string& name ) 
{
ArchiveP	child;
    child = this->getArchive()->newArchiveNode();
    this->addChild(child);
    child->setRawNodeName(name);
    return((child));
}


bool	Dumb_Node::saving()
{_OF();
    ASSERT(this->_PArchive!=NULL);
    return((this->_PArchive->isSaveArchive()));
};


bool	Dumb_Node::hasChildWithUniqueId(const string& uid)
{_G();
    if ( this->_PChildren == NULL ) return((false));
    handleType huid = this->archive().symbolManager().handle(uid);
    return((this->_PChildren->hasChildWithUniqueId(this,huid)));
}

bool	Dumb_Node::hasChildWithUniqueId(handleType huid)
{_G();
    if ( this->_PChildren == NULL ) return((false));
    return((this->_PChildren->hasChildWithUniqueId(this,huid)));
}

ArchiveP	Dumb_Node::childWithUniqueIdOrNull(handleType huid)
{_G();
    ASSERT(this->_PChildren!=NULL);
    return((this->_PChildren->childWithUniqueIdOrNull(this,huid)));
}

ArchiveP	Dumb_Node::childWithUniqueIdOrNull(const string& uid)
{_G();
    ASSERT(this->_PChildren!=NULL);
    handleType huid = this->archive().symbolManager().handle(uid);
    return((this->_PChildren->childWithUniqueIdOrNull(this,huid)));
}

ArchiveP Dumb_Node::childWithUniqueId(const string& uid)
{_G();
    handleType huid = this->archive().symbolManager().handle(uid);
    if (this->_PChildren == NULL)
    {
	SIMPLE_ERROR(BF("Could not find child with uid(%s) This node doesn't have children") % uid);
    }
    ArchiveP child = this->_PChildren->childWithUniqueIdOrNull(this,huid);
    if ( child == NULL )
    {
	SIMPLE_ERROR(BF("Could not find child with uid(%s) This node has children but none with that uid") % uid );
    }
    return((child));
}

ArchiveP Dumb_Node::childWithUniqueId(handleType huid)
{_G();
    if (this->_PChildren == NULL)
    {
	stringstream ss;
	ss << "Could not find child with uid(" << this->archive().symbolManager().anySymbol(huid) << " This node doesn't have children";
	SIMPLE_ERROR(BF("%s") % ss.str() );
    }
    ArchiveP child = this->_PChildren->childWithUniqueIdOrNull(this,huid);
    if ( child == NULL )
    {
	stringstream ss;
	ss << "Could not find child with uid(" << this->archive().symbolManager().anySymbol(huid) << " This node has children but none with that uid";
	SIMPLE_ERROR(BF("%s") % ss.str() );
    }
    return((child));
}

void	Dumb_Node::addChild( ArchiveP	node)
{
    if ( this->_PChildren == NULL )
    {
	this->_PChildren = this->getArchive()->newChildHolder();
    }
    this->_PChildren->addChild(this,node);
    node->setParent(this);
}


bool	Dumb_Node::hasChildrenWithName( const string& name )
{_G();
    if ( this->_PChildren==NULL ) return((false));
    return((this->_PChildren->hasChildrenWithName(this,name)));
}

bool Dumb_Node::hasChildWithUniqueNodeName( const string& nodeName )
{
    if ( this->_PChildren == NULL ) return((false));
    return((this->_PChildren->hasChildWithUniqueNodeName(this,nodeName)));
}


ArchiveP	Dumb_Node::childWithUniqueNodeName( const string& nodeName )
{_OF();
    ASSERT(this->_PChildren!=NULL);
    return((this->_PChildren->childWithUniqueNodeName(this,nodeName)));
}



/*! Return the child with the requested node name and key*/
ArchiveP	Dumb_Node::childWithNodeNameAndKey( const string& nodeName,
					const string& key )
{_OF();
    ASSERT(this->_PChildren!=NULL);
    return((this->_PChildren->childWithNodeNameAndKey(this,nodeName,key)));
}


bool	Dumb_Node::doesNodeDescribeObject()
{
    const string& className = this->getNodeName();
    return((_lisp->isClassName(className)));
}


void Dumb_Node::createYourSymbol()
{
    string symname = this->getAttributeString("_sym");
    Symbol_sp sym = _lisp->intern(symname);
    this->_Object = sym;
}

#if 0
void Dumb_Node::createYourColor()
{
    this->_Object = Color_O::getOrDefineSystemColor(this);
}
#endif

void	Dumb_Node::createYourObject()
{ _G();
    T_sp	obj;
    string className = this->getNodeName();
    {_BLOCK_TRACEF(BF("Determining if node name(%s) is known") % this->getNodeName() );
	if ( !_lisp->isClassName(className) )
	{
	    LOG(BF("Trying to create an object for unknown class name: %s") % className );
	    LOG(BF("Known class names: %s")% _lisp->allClassNames()->asString() );
	    Symbol_sp newForwardClassSymbol = _lisp->intern(className);
	    LOG(BF("I'm going to create a ForwardReferencedClass for the new class name(%s)")% newForwardClassSymbol->fullName());
	    IMPLEMENT_MEF(BF("Call make_instance the proper way"));
#if 0
	    ForwardReferencedClass_sp newForwardClass = eval::apply(_sym_make_instance,
								    (ql::list(_lisp)
								     << _sym_forward_referenced_class
								     << kw::_sym_name
								     << newForwardClassSymbol),_lisp)->as<ForwardReferencedClass_O>();
	    string coreClassName = this->getAttributeStringDefault(CoreBuiltInClassAttribute,"StandardObject");
	    if ( !_lisp->isClassName(coreClassName) )
	    {
		SIMPLE_ERROR(BF("Trying to create object with CoreBuiltInClass(%s) but that class doesn't exist")%coreClassName);
	    }
	    Class_sp coreBuiltInClass = _lisp->classFromClassName(coreClassName);
	    newForwardClass->setInstanceCoreClass(coreBuiltInClass->as<BuiltInClass_O>());
	    ArchiveP binderNode = this->childWithUniqueIdOrNull("slots");
	    newForwardClass->defineYourSlotsFromBinderArchiveNode(binderNode);
		    //
		    // By default set the base class to StandardObject
		    //
	    newForwardClass->addInstanceBaseClass(coreBuiltInClass->instanceClassSymbol()); //coreBuiltInClass);
#endif
	} else 
	{
	    LOG(BF("The class with name(%s) is known - creating object of that class")%className );
	}
    }
    {_BLOCK_TRACEF(BF("Creating the object with className(%s)") % className );
	Symbol_sp classSymbol = _lisp->findSymbol(className);
	if ( classSymbol == cl::_sym_symbol )
	{
	    // Symbols are handled in a special way
	    // because they have to be interned and they
	    // may already be in the system
	    this->createYourSymbol();
	    return;
#if 0
	} else if ( classSymbol == _sym_color )
	{
	    // Colors are handled in a special way
	    // because they are stored in a color database
	    // and may already be in the system
	    this->createYourColor();
	    return;
#endif
	}
	LOG(BF("Class symbol = %s") % classSymbol->fullName() );
	T_sp mc = eval::funcall(cl::_sym_findClass,classSymbol);
//	Class_sp mc = classSymbol->symbolValue()->as<Class_O>();
	LOG(BF("Class name(%s)")%mc->getPackagedName());
	obj = _lisp->createObjectOfClass(mc);
	this->_Object = obj;
	LOG(BF("Created object of class(%s)") % obj->className() );
    }
    {_BLOCK_TRACE("Filling in the contents of object by calling obj->archive");
	vector<ArchiveP>::iterator	ci;
	for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ )
	{
	    (*ci)->setRecognized(false);
	}
	this->setRecognized(true);
	obj->archive(this);
    }
    { _BLOCK_TRACE("Checking to make sure all child nodes are accounted for");
	vector<ArchiveP>::iterator	ci;
	for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ )
	{
	    if ( !(*ci)->isRecognized() ) 
	    {
		stringstream ss;
		ss << "Left over - unrecognized node uid(" << (*ci)->getUniqueIdCharacters() << ")";
		this->getArchive()->addError( ss.str(), (*ci) );
	    } else
	    {
		T_sp child = (*ci)->getObject();
		LOG(BF("Setting owner of child object(%p) to object(%p)") % child.get() % obj.get() );
		IMPLEMENT_MEF(BF("Removed _InitializationOwner"));
#if 0
		child->setOwner(obj);
#endif
	    }
	}
    }
}









//
//	dataIsAllWhiteSpace
//
//	Return true if the data is all white space
bool	Dumb_Node::dataIsAllWhiteSpace()
{
string::iterator	it;
    string val = this->characters();
    for (it=val.begin();it!=val.end();it++){
	if (!isspace(*it)) {
	    return((false));
	}
    }
    return((true));
}


int	Dumb_Node::dataCountNewLines()
{
int			newLines;
    newLines = 0;
    const string& val = this->characters();
    string::const_iterator	it;
    for (it=val.begin();it!=val.end();it++)
    {
	if (*it == '\n' ) {
	    newLines++;
	}
    }
    return((newLines));
}



#if 0
void	Dumb_Node::writeXml( ostream& out )
{ _G();
vector<ArchiveP>::iterator	it;
AttributeMap::iterator	attr;
AttributeMap::iterator	abegin;
AttributeMap::iterator	aend;
bool				dataWhiteSpace;
int				dataNewLines;
bool				usePrefix;

    if ( this->isSuppressNode() ) return;
    out << "<" << this->getNodeName();
	out.flush();
    if ( this->_Huid != EmptyStringHandle )
    {
	out << " " << UniqueIdTag << "=\"";
	out << this->archive().symbolManager().anySymbol(this->_Huid) << "\"";
    }
    abegin = this->_Attributes.begin();
    aend = this->_Attributes.end();
    for ( attr=abegin; attr!=aend; attr++ ) 
    {
	this->xml_fwrite(out," ");
	this->xml_fwrite(out,this->_Attributes.name(this,attr));
	this->xml_fwrite(out,"=\"");
	this->xml_fwrite(out,this->_Attributes.value(this,attr));
	this->xml_fwrite(out,"\"");
//	out << " " << this->_Attributes.name(this,attr) << "=\"" << this->_Attributes.value(this,attr)  << "\"";
	out.flush();
    }
    dataWhiteSpace = this->dataIsAllWhiteSpace();
    dataNewLines = 0;
    if ( !dataWhiteSpace ) 
    {
	dataNewLines = this->dataCountNewLines();
    }
    if ( this->begin_Children() == this->end_Children() && dataWhiteSpace ) 
    {
	out << "/>\n";
	out.flush();
    } else 
    {
	out << ">";
		//
		// If there are _Children then start them
		// on the next line
		//
	usePrefix = false;
	if ( this->begin_Children() != this->end_Children() ) 
	{
	    out << endl;
	    usePrefix = true;
	}
	out.flush();
	for ( it = this->begin_Children(); it!=this->end_Children(); it++ ) 
	{
	    if ( (*it) == NULL )
	    {
		SIMPLE_ERROR(BF("Bad child"));
	    }
	    (*it)->writeXml(out);
	}
	out.flush();
	if ( !dataWhiteSpace ) 
	{
	    if ( dataNewLines > 0 ) 
	    {
		out << endl;
		usePrefix = true;
	    }
	    this->xml_fwrite(out,this->characters());
	    out.flush();
	}
	out << "</" << this->getNodeName() << ">\n";
	out.flush();
    }
}
#endif


#if 0
void	Dumb_Node::writeCandoFormat( ostream& out, bool debug )
{ _G();
vector<ArchiveP>::iterator	it;
AttributeMap::iterator	attr;
AttributeMap::iterator	abegin;
AttributeMap::iterator	aend;
bool				dataWhiteSpace;
int				dataNewLines;
bool				usePrefix;

    if ( this->isSuppressNode() ) return;
    out << "n" << this->getNodeName() << endl;
    if ( this->_Huid != EmptyStringHandle )
    {
	out << "u" << this->archive().symbolManager().anySymbol(this->_Huid);
	if ( debug )
	{
	    out << " # raw Huid = " << this->_Huid;
	}
	out << endl;
    }
    abegin = this->_Attributes.begin();
    aend = this->_Attributes.end();
    for ( attr=abegin; attr!=aend; attr++ ) 
    {
	out << "a" << this->_Attributes.name(this,attr) << ":" << this->_Attributes.value(this,attr)<< endl;
    }
    dataWhiteSpace = this->dataIsAllWhiteSpace();
    dataNewLines = 0;
    if ( !dataWhiteSpace ) 
    {
	const char* data = this->characters();
	while ( *data )
	{
	    out << "d" << this->oneLineOfCharacters(&data) << endl;
	}
    }
    for ( it = this->begin_Children(); it!=this->end_Children(); it++ ) 
    {
	if ( (*it) == NULL )
	{
	    SIMPLE_ERROR(BF("Bad child"));
	}
	(*it)->writeCandoFormat(out,debug);
    }
    out << "e" <<endl;
    out.flush();
}
#endif



string	Dumb_Node::description() const
{
AttributeMap::const_iterator	attr, abegin, aend;
    stringstream	ss;
    ss.str("");
    ss << "Node@";
    ss << std::hex << this << std::dec;
    ss << "[" << this->getNodeName()<<" ";
    ss << " line#"<<this->_LineNumber << " ";
    abegin = this->_Attributes.begin();
    aend = this->_Attributes.end();
    for ( attr=abegin; attr!=aend; attr++ ) 
    {
	ss << "attr("<< this->_Attributes.name_const(this,attr) ;
	ss << ":" << this->_Attributes.value(this,attr) << ") ";
    }
    ss<<"]";
    return((ss.str()));
}



void	Dumb_Node::dumpChildrenNames()
{
    VectorNodes::iterator	ci;
    _lisp->print(BF( "Children names: "));
    for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ ) {
	_lisp->prin1(BF( " %s") % (*ci)->getNodeName().c_str() );
    }
    _lisp->print(BF(""));
}


void	Dumb_Node::archiveVectorStrings( const string& uid, VectorStrings& vec )
{_G();
    if ( this->saving() ) {
    	if ( vec.size() > 0 ) {
	    ArchiveP newNode;
	    stringstream ss;
	    for ( vector<string>::iterator si=vec.begin(); si!=vec.end(); si++ ) {
		ss << escapeWhiteSpace(*si) << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("VectorString");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsVectorOfStrings(vec);
	    childNode->setRecognized(true);
	} else {
	    vec.clear();
	}
    }
}

void	Dumb_Node::archiveVectorSymbols( const string& uid, vector<Symbol_sp>& vec )
{_G();
    if ( this->saving() ) {
    	if ( vec.size() > 0 ) {
	    ArchiveP newNode;
	    stringstream ss;
	    for ( vector<Symbol_sp>::iterator si=vec.begin(); si!=vec.end(); si++ ) {
		ss << (*si)->__repr__() << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("VectorSymbols");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsVectorOfSymbols(vec);
	    childNode->setRecognized(true);
	} else {
	    vec.clear();
	}
    }
}


void	Dumb_Node::archiveVectorDouble( const string& uid, vector<double>& vec )
{_G();
    if ( this->saving() ) {
    	if ( vec.size() > 0 ) {
	    ArchiveP newNode;
	    stringstream ss;
	    for ( vector<double>::iterator si=vec.begin(); si!=vec.end(); si++ ) {
		ss << *si << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("VectorDouble");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    vec.clear();
	    childNode->getDataAsVectorOfDoubles(vec);
	    childNode->setRecognized(true);
	} else {
	    vec.clear();
	}
    }
}


void	Dumb_Node::archiveVectorInt( const string& uid, vector<int>& vec )
{_G();
    if ( this->saving() ) {
    	if ( vec.size() > 0 ) {
	    ArchiveP newNode;
	    stringstream ss;
	    for ( vector<int>::iterator si=vec.begin(); si!=vec.end(); si++ ) {
		ss << *si << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("VectorInt");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    vec.clear();
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsVectorOfInts(vec);
	    childNode->setRecognized(true);
	} else {
	    vec.clear();
	}
    }
}
void	Dumb_Node::archiveVectorUInt( const string& uid, vector<uint>& vec )
{_G();
    if ( this->saving() ) {
    	if ( vec.size() > 0 ) {
	    ArchiveP newNode;
	    stringstream ss;
	    for ( vector<uint>::iterator si=vec.begin(); si!=vec.end(); si++ ) {
		ss << *si << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("VectorUInt");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    vec.clear();
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsVectorOfUnsignedInts(vec);
	    childNode->setRecognized(true);
	} else {
	    vec.clear();
	}
    }
}


void	Dumb_Node::archiveMapSymbolInt( const string& uid, map<Symbol_sp,int>& vec )
{_G();
    if ( this->saving() )
    {
    	if ( vec.size() > 0 )
	{
	    ArchiveP newNode;
	    stringstream ss;
	    for ( map<Symbol_sp,int>::iterator si=vec.begin(); si!=vec.end(); si++ )
	    {
		ss << si->first->__repr__() << " " << si->second << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("MapSymbolInt");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else
    {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) )
	{
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsMapSymbolInts(_lisp,vec);
	    childNode->setRecognized(true);
	} else
	{
	    vec.clear();
	}
    }
}

void	Dumb_Node::archiveMapSymbolUnsignedInt( const string& uid, map<Symbol_sp,uint>& vec )
{_G();
    if ( this->saving() )
    {
    	if ( vec.size() > 0 )
	{
	    ArchiveP newNode;
	    stringstream ss;
	    for ( map<Symbol_sp,uint>::iterator si=vec.begin(); si!=vec.end(); si++ )
	    {
		ss << si->first->__repr__() << " " << si->second << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("MapSymbolUInt");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else
    {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) )
	{
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsMapSymbolUnsignedInts(_lisp,vec);
	    childNode->setRecognized(true);
	} else
	{
	    vec.clear();
	}
    }
}


void	Dumb_Node::archiveMultiMapSymbolUnsignedInts( const string& uid, multimap<Symbol_sp,uint>& vec )
{_G();
    if ( this->saving() )
    {
    	if ( vec.size() > 0 )
	{
	    ArchiveP newNode;
	    stringstream ss;
	    for ( multimap<Symbol_sp,uint>::iterator si=vec.begin(); si!=vec.end(); si++ )
	    {
		ss << si->first->__repr__() << " " << si->second << " ";
	    }
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("MapSymbolUInt");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(ss.str());
	    this->addChild(newNode);
	}
    } else
    {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) )
	{
	    childNode = this->childWithUniqueId(uid);
	    childNode->getDataAsMultiMapSymbolUnsignedInts(_lisp,vec);
	    childNode->setRecognized(true);
	} else
	{
	    vec.clear();
	}
    }
}





#if 0
StringSet_sp	Dumb_Node::getDataAsStringSet()
{
StringSet_sp	ss;
unsigned int	cur;
int		start, stop;
    ss = StringSet_O::create();
    const string& s = this->characters();
    cur = 0;
    while (cur <s.size()) 
    {
	if ( isspace(s[cur]) || s[cur]<' ' ) {
	    cur++;
	    continue;
	}
	start = cur;
	while (cur<s.size() && !isspace(s[cur])) {
	    cur++;
	}
	stop = cur-1;
	ss->insert(s.substr(start,stop-start+1));
    }
    return((ss));
}
#endif



void Dumb_Node::getDataAsVectorOfStrings(vector<string>&vs)
{
    unsigned int	cur;
    int		start, stop;
    vs.clear();
    const string& s = this->characters();
    cur = 0;
    while (cur <s.size()) 
    {
	if ( isspace(s[cur]) || s[cur]<' ' ) {
	    cur++;
	    continue;
	}
	start = cur;
	while (cur<s.size() && !isspace(s[cur])) {
	    cur++;
	}
	stop = cur-1;
	vs.push_back(unEscapeWhiteSpace(s.substr(start,stop-start+1)));
    }
}



void Dumb_Node::getDataAsVectorOfSymbols(vector<Symbol_sp>&vs)
{
    unsigned int	cur;
    int		start, stop;
    vs.clear();
    const string& s = this->characters();
    cur = 0;
    while (cur <s.size()) 
    {
	if ( isspace(s[cur]) || s[cur]<' ' ) {
	    cur++;
	    continue;
	}
	start = cur;
	while (cur<s.size() && !isspace(s[cur])) {
	    cur++;
	}
	stop = cur-1;
	vs.push_back(_lisp->intern(s.substr(start,stop-start+1)));
    }
}

void Dumb_Node::getDataAsVectorOfDoubles(vector<double>&vs)
{
    unsigned int	cur;
    int		start, stop;
    vs.clear();
    const string& s = this->characters();
    cur = 0;
    while (cur <s.size()) {
	if ( isspace(s[cur]) || s[cur]<' ' ) {
	    cur++;
	    continue;
	}
	start = cur;
	while (cur<s.size() && !isspace(s[cur])) {
	    cur++;
	}
	stop = cur-1;
	string dbl = s.substr(start,stop-start+1);
	vs.push_back(atof(dbl.c_str()));
    }
}


void Dumb_Node::getDataAsVectorOfInts(vector<int>& vs)
{
    unsigned int	cur;
    int		start, stop;
    vs.clear();
    const string& s = this->characters();
    cur = 0;
    while (cur <s.size()) {
	if ( isspace(s[cur]) || s[cur]<' ' ) {
	    cur++;
	    continue;
	}
	start = cur;
	while (cur<s.size() && !isspace(s[cur])) {
	    cur++;
	}
	stop = cur-1;
	string istr = s.substr(start,stop-start+1);
	vs.push_back(atoi(istr.c_str()));
    }
}


void Dumb_Node::getDataAsVectorOfUnsignedInts(vector<uint>& vs)
{
    unsigned int	cur;
    int		start, stop;
    vs.clear();
    const string& s = this->characters();
    cur = 0;
    while (cur <s.size()) {
	if ( isspace(s[cur]) || s[cur]<' ' )
	{
	    cur++;
	    continue;
	}
	start = cur;
	while (cur<s.size() && !isspace(s[cur]))
	{
	    cur++;
	}
	stop = cur-1;
	string istr = s.substr(start,stop-start+1);
	vs.push_back(strtoul(istr.c_str(),NULL,10));   // atoi(istr.c_str()));
    }
}





bool	Dumb_Node::getWord(const char*& cur, char buffer[], uint bufferMax)
{_G();
    uint numChars = 0;
    char* wr = buffer;
    while (*cur && isspace((int)(*cur))) cur++;
    if ( (*cur) == '\0' ) return((false));
    while (*cur && !isspace((int)(*cur)))
    {
	(*wr) = *cur;
	wr++;
	cur++;
	numChars++;
	if ( numChars >= bufferMax )
	{
	    THROW_HARD_ERROR(BF("Increase the size of buffers when converting data to strings"));
	}
    }
    if ( (*cur) == '\0' && numChars == 0 ) return((false));
    (*wr) = '\0';
    return((true));
}


void Dumb_Node::getDataAsMapSymbolInts(Lisp_sp lisp,map<Symbol_sp,int>& vs )
{_G();
    vs.clear();
    char buffer[BUFFER_MAX];
    const char* cur;
    cur = this->characters();
    LOG(BF("Interpreting MapSymbolInts for characters: %s") % cur );
    while (this->getWord(cur,buffer,BUFFER_MAX))
    {
	string sstr = buffer;
	LOG(BF("Found symbol(%s)") % buffer);
	if ( !this->getWord(cur,buffer,BUFFER_MAX))
	{
	    THROW_HARD_ERROR(BF("Bad data in Symbol,Fixnum xml line(%d) buffer=%s") % this->getLineNumber() % buffer  );
	}
	string istr = buffer;
	LOG(BF("Found int(%s)") % buffer);
	Symbol_sp sym = lisp->intern(sstr);
	vs[sym] = atoi(buffer);
    }
}




void Dumb_Node::getDataAsMapSymbolUnsignedInts(Lisp_sp lisp,map<Symbol_sp,uint>& vs )
{_G();
    vs.clear();
    char buffer[BUFFER_MAX];
    const char* cur;
    cur = this->characters();
    LOG(BF("Interpreting MapSymbolUnsignedInts for characters: %s") % cur );
    while (this->getWord(cur,buffer,BUFFER_MAX))
    {
	string sstr = buffer;
	LOG(BF("Found symbol(%s)") % buffer);
	if ( !this->getWord(cur,buffer,BUFFER_MAX))
	{
	    THROW_HARD_ERROR(BF("Bad data in Symbol,Fixnum xml line(%d) buffer=%s") % this->getLineNumber() % buffer  );
	}
	string istr = buffer;
	LOG(BF("Found int(%s)") % buffer);
	Symbol_sp sym = lisp->intern(sstr);
	vs[sym] = strtoul(buffer,NULL,10);
    }
}



void Dumb_Node::getDataAsMultiMapSymbolUnsignedInts(Lisp_sp lisp,multimap<Symbol_sp,uint>& vs )
{_G();
    vs.clear();
    char buffer[BUFFER_MAX];
    const char* cur;
    cur = this->characters();
    LOG(BF("Interpreting MultiMapSymbolUnsignedInts for characters: %s") % cur );
    while (this->getWord(cur,buffer,BUFFER_MAX))
    {
	string sstr = buffer;
	LOG(BF("Found symbol(%s)") % buffer);
	if ( !this->getWord(cur,buffer,BUFFER_MAX))
	{
	    THROW_HARD_ERROR(BF("Bad data in Symbol,Fixnum xml line(%d) buffer=%s") % this->getLineNumber() % buffer  );
	}
	string istr = buffer;
	LOG(BF("Found int(%s)") % buffer);
	Symbol_sp sym = lisp->intern(sstr);
	vs.insert(pair<Symbol_sp,uint>(sym,strtoul(buffer,NULL,10)));
    }
}




void	Dumb_Node::archiveString( const string& uid, string& vec )
{_G();
    if ( this->saving() ) {
	ArchiveP newNode;
	newNode = this->getArchive()->newArchiveNode();
	newNode->setClasslessNodeName("podString");
	newNode->setTextUniqueId(uid);
	newNode->setCharacters(vec);
	this->addChild(newNode);
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    ASSERT(childNode->isNamed("podString"));
	    vec = childNode->characters();
	    childNode->setRecognized(true);
	} else {
	    vec = "";
	}
    }
}

void	Dumb_Node::archiveStringIfNotDefault( const string& uid, string& vec, const string& defaultString )
{_G();
    if ( this->saving() ) {
        if ( vec != defaultString )
	{
	    ArchiveP newNode;
	    newNode = this->getArchive()->newArchiveNode();
	    newNode->setClasslessNodeName("podString");
	    newNode->setTextUniqueId(uid);
	    newNode->setCharacters(vec);
	    this->addChild(newNode);
	}
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    ASSERT(childNode->isNamed("podString"));
	    vec = childNode->characters();
	    childNode->setRecognized(true);
	} else {
	    vec = defaultString;
	}
    }
}


void	Dumb_Node::archiveStringStream( const string& uid, stringstream& vec )
{_OF();
    if ( this->saving() ) {
	ArchiveP newNode;
	newNode = this->getArchive()->newArchiveNode();
	newNode->setClasslessNodeName("podStream");
	newNode->setTextUniqueId(uid);
	newNode->setCharacters(vec.str());
	this->addChild(newNode);
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    ASSERT(childNode->isNamed("Stream"));
	    vec.str(childNode->characters());
	    childNode->setRecognized(true);
	} else {
	    vec.str("");
	}
    }
}


void	Dumb_Node::archiveStringStreamIfDefined( const string& uid, stringstream& vec )
{_OF();
    if ( this->saving() ) {
        if ( vec.str() == "" ) return;
	ArchiveP newNode;
	newNode = this->getArchive()->newArchiveNode();
	newNode->setClasslessNodeName("podStream");
	newNode->setTextUniqueId(uid);
	newNode->setCharacters(vec.str());
	this->addChild(newNode);
    } else {
        ArchiveP childNode;
    	if ( this->hasChildWithUniqueId(uid) ) {
	    childNode = this->childWithUniqueId(uid);
	    ASSERTP(childNode->isNamed("podStream"),"Expected node with name(podStream) but found name("+childNode->getNodeName()+")");
	    vec.str(childNode->characters());
	    childNode->setRecognized(true);
	} else {
	    vec.str("");
	}
    }
}








}; // namespace core
