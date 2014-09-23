/*
    File: old_archiveNode.h
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
#ifndef	ARCHIVE_NODE_H //[
#define	ARCHIVE_NODE_H

#include <map>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <vector>
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"
#include "lispStream.fwd.h"
#include "symbolToEnumConverter.h"
#include "holder.h"


namespace core {

class MemoryUsageTracker;


typedef	vector<T_sp>		ObjectVector;
typedef	vector<T_sp>::iterator	ObjectVectorIterator;


class ChildHolder
{
    friend class ArchiveMemoryManager_O;
protected:
    void initialize();
private:
    vector<ArchiveP>		_Children;
    map<handleType,uint>	_ChildrenByUid;
public:
	vector<ArchiveP>::iterator begin_Children(Dumb_Node* parent) { return this->_Children.begin();};
	vector<ArchiveP>::iterator end_Children(Dumb_Node* parent) { return this->_Children.end();};

	void addChild(Dumb_Node* parent,ArchiveP);

    bool	hasChildWithUniqueId(Dumb_Node* parent, handleType uid );
    ArchiveP 	childWithUniqueIdOrNull(Dumb_Node* parent, handleType uid );
    ArchiveP 	childWithUniqueId(Dumb_Node* parent, handleType uid );
    bool	hasChildWithUniqueId(Dumb_Node* parent, const string& uid );
    ArchiveP 	childWithUniqueIdOrNull(Dumb_Node* parent, const string& uid );
    ArchiveP 	childWithUniqueId(Dumb_Node* parent, const string& uid );
    ArchiveP	childWithUniqueNodeName(Dumb_Node* parent,  const string& nodeName );
    bool	hasChildWithUniqueNodeName(Dumb_Node* parent,  const string& nodeName );
    ArchiveP	childWithNodeNameAndKey(Dumb_Node* parent,  const string& nodeName, const string& key );

    bool		hasChildrenWithName(Dumb_Node* parent, const string& nm);
    uint		numberOfChildren(Dumb_Node* parent);
public:
    ChildHolder();
    virtual ~ChildHolder();
private:	// Non-copyable object
    ChildHolder(const ChildHolder& c) {};
};


SMART(Archive);

class	Dumb_Node;
typedef	Dumb_Node* ArchiveP;
typedef	Dumb_Node* ArchiveP;




class AttributeMap
{
public:
	vector<pair<handleType,uint> >	_Entries;
	typedef vector<pair<handleType,uint> >::iterator iterator;
	typedef vector<pair<handleType,uint> >::const_iterator const_iterator;
public:
	const string& name(const Dumb_Node*, iterator);
	const string& name_const(const Dumb_Node*, const_iterator) const;
	const char* value(const Dumb_Node*, iterator) const;
	const char* value(const Dumb_Node*, const_iterator) const;
	bool contains(Dumb_Node*,const string& name);
	const char* get(Dumb_Node*, const string& );
		/*! Set the attribute that was loaded from an XML file
		 * This checks if the key is the UniqueId key and if it is
		 * it throws an exception
		 */
	void set(Dumb_Node*, const string& key, const string& val);
		/*! Set the attribute that was loaded from an XML file
		 * This checks if the key is the UniqueId key and if it is
		 * sets the UniqueId of the node
		 */
	void setFromXml(Dumb_Node*, const string& key, const string& val);
	uint size() { return this->_Entries.size();};
	iterator begin() { return this->_Entries.begin(); };
	iterator end() { return this->_Entries.end(); };
	const_iterator begin() const { return this->_Entries.begin(); };
	const_iterator end() const { return this->_Entries.end(); };

};



typedef	vector<ArchiveP>	VectorNodes;
typedef VectorNodes		VectorArchiveNodes;

#define	NODE_Recognized 0x01  //!< Used to identify nodes that were not asked for and may have incorrect names
#define	NODE_Finalized  0x02
#define	NODE_Suppressed 0x04 //!< Set to true if this node should be suppressed


/*! An Node can have attributes
 */
class	Dumb_Node {
    friend class Archive_O;
    friend void XmlSaveArchive_writeXml(Dumb_Node* node, Stream_sp out);
    friend void CandoFormat_writeCandoFormat(Dumb_Node* node, Stream_sp out, bool debug);

    friend class MemoryUsageTracker;
    friend class LoadArchive_O;
    friend class SaveArchive_O;
    friend class ArchiveMemoryManager_O;
protected:
    void initialize(Archive_O* archive);
public:
	Lisp_sp lisp();
	Lisp_sp lisp() const;
protected:
	/*! Stores the unique id for this node
	 */
    Symbol_sp 		_Kind;
    T_sp		_Object;
    Cons_sp 		_Attributes;
    Vector_sp 		_Data;

//protected:	//! Signals if a node should be written only if a weak pointer is valid
//    bool	_IfTrueIgnoreNodeIfNecessaryWeakPointerIsNull_IfFalseAlwaysWriteNode;

private:
    void pathFromNodeToRoot(ArchiveP node,vector<Dumb_Node*>& path);

public:	// mirror functions in Archive_O so I don't have to expose it here
    void loadOnlyObjectOfClassWithinNode( Symbol_sp classSymbol, T_sp& obj );
    void saveOnlyObjectOfClassWithinNode( Symbol_sp classSymbol, T_sp obj );
    void loadObjectWithinNodeWithUid( const string& uid, T_sp& obj );
    void saveObjectWithinNodeWithUid( const string& uid, T_sp obj );
    void saveObjectWithinNodeWithUidAndKey( const string& suid, const string& key, T_sp obj);
    T_sp loadObjectDirectly(ArchiveP node);
    void loadWeakPointerInAttribute( ArchiveP node, const string& attribute, T_wp& obj, Symbol_sp expectedClassSymbol, bool mustBeDefined );
    void saveWeakPointerInAttribute( const string& attribute, T_wp obj, 
    				bool suppressNodeForBrokenOrNilWeakPointers,
				bool suppressAttributeForNilWeakPointers );
    uint nextUniqueId();
    string nextUniqueIdCharacters();

public:
    Dumb_Node(Archive_O* archive);
    virtual ~Dumb_Node();
private:	// Non-copyable object
    Dumb_Node(const Dumb_Node& c) {};

public:
	ArchiveP newArchiveNode();

public:
//	void xml_fwrite( ostream& sout, const string& str);

public:
    static ArchiveP create(Archive_sp archive);
public:
    bool	isRecognized() { return (this->_Flags&NODE_Recognized)>0;};
    void	setRecognized(bool b);

    bool	isFinalized() { return (this->_Flags&NODE_Finalized)>0;};
    void	setFinalized(bool b);

		/*! Estimate the memory used by everything and
		 * print a report
		 */
    LongLongInt describeMemoryUsage();

		/*! Do your best to estimate the number of nodes
		 * in this node (recursively including child nodes )
		 */
    uint	nodeCount();
		/*! Do your best to estimate the number of nodes under
		 * this node that have no children (recursively including child nodes )
		 */
    uint	leafCount();

    bool	doesNodeDescribeObject();
    bool	isSuppressNode() { return (this->_Flags&NODE_Suppressed)>0;};
    void	setSuppressNode(bool b);

	/*! Define a weak pointer that must be valid if this node is to be written out
	 * by defaul all nodes are written out
	 */
//    void	setNecessaryValidWeakPointer(T_wp obj);

    Archive_sp   getArchive() const;
    Archive_O&	archive() { HARD_ASSERT(this->_PArchive!=NULL); return *(this->_PArchive);};
    Archive_O&	archive() const { HARD_ASSERT(this->_PArchive!=NULL); return *(this->_PArchive);};
    const Archive_O&	archive_const() const { HARD_ASSERT(this->_PArchive!=NULL); return *(this->_PArchive);};

    bool	saving();
    bool	loading() { return !this->saving();};

    bool	objectHasBeenCreated() { return this->_Object->notNil(); };
    T_sp	getObject() { return this->_Object; };
    void	setObject(T_sp obj) { this->_Object = obj; };
    void	createYourObject();
    void	createYourSymbol();
//    void	createYourColor();

    void	needsFinalization();

    bool	hasParent()	{ return this->_Parent != NULL; };
    Dumb_Node*	getParent()	{_OF();ASSERT(this->_Parent!=NULL);return this->_Parent;};
    void	setParent(Dumb_Node* par)	{this->_Parent = par;};

	//! Return the Node attained by following the path
    ArchiveP	followLinkPath(const string& linkStr, ArchiveP startNode );
	/*! Return the Node attained by following the path looking for a class
	 * with the name "%ClassName"
	 */
    ArchiveP	followLinkPathUpTreeForClass(const string& linkStr, ArchiveP startNode );
	/*! Return the Node attained by following the path looking for a class
	 * with the name "^uid"
	 */
    ArchiveP	followLinkPathUpTreeForUid(const string& linkStr, ArchiveP startNode );
    	//! Return a path to get from node to this
    string	pathRelativeTo( ArchiveP fromNode );

    string	getFileName();
    bool	isNamed(const string& nm);
    bool	isNamed(handleType nm);
    const string& 	getNodeName() const;
    void	setRawNodeName(const string& nm);
    void	setClassNodeName(Class_sp mc);
    void	setClasslessNodeName(const string& nm);
    void	setLineNumber(int line)		{this->_LineNumber = line;};
    int		getLineNumber()			{return this->_LineNumber;};

    bool	hasChildWithUniqueId(const string& uid );
    ArchiveP 	childWithUniqueIdOrNull(const string& uid );
    ArchiveP 	childWithUniqueId(const string& uid );

    bool	hasChildWithUniqueId(handleType huid);
    ArchiveP 	childWithUniqueIdOrNull(handleType huid);
    ArchiveP 	childWithUniqueId(handleType huid);

    ArchiveP	childWithUniqueNodeName( const string& nodeName );
    bool	hasChildWithUniqueNodeName( const string& nodeName );
    ArchiveP	childWithNodeNameAndKey( const string& nodeName, const string& key );

    bool		hasChildrenWithName(const string& nm);
    int			countChildrenWithName(const string& nm);
    ArchiveP		onlyChild();


//    VectorNodes	getChildrenWithName(const string& nm);
//    VectorNodes	getChildrenWithNameAndRemoveThem(const string& nm);
//    VectorNodes	getChildren()	{return this->_Children;};
#ifdef	USEBOOSTPYTHON
    boost::python::list		python__ChildrenWithName(const string& nm);
    boost::python::list		python_getChildren();
    boost::python::list		python_gatherSubNodesWithName(const string& nm);
    boost::python::long_	python_hasChildrenWithName(const string& nm);
#endif

    uint		numberOfChildren();

    bool	hasUniqueId() { return this->_Huid != EmptyStringHandle; };
    string getUniqueIdCharactersIfMissingThrow();
    string getUniqueIdCharacters();
    handleType  getUniqueIdHandle() { return this->_Huid;};
    uint getUniqueIdNumeric();
    void throwErrorForChildren();
    void throwErrorForChildrenWithoutName(string name);
//    int	getNodeIndex() { return this->_NodeIndex; };
//    void setNodeIndex(int i) { this->_NodeIndex = i; };

//    void	writeXml( ostream& out );
//    void	writeCandoFormat( ostream& out, bool debug );
    void	appendToFileName( string fileName );
    ostream&	dumpToStream(ostream& o);
    void	dump();
    void	dumpChildrenNames();
    string	description() const;


    bool	hasAttribute(const string& at ) 
    {
	return this->_Attributes.contains(this,at);
    };

    string	getAttributeStringDefault(const string& at,const string& df) {
	if ( this->_Attributes.contains(this,at) ) {
	    return this->_Attributes.get(this,at);
	} else {
	    return df;	// return the default value
	}
    };
    string	getAttributeString(const string& at) 
    {_OF();
	if ( this->_Attributes.contains(this,at) ) {
	    return this->_Attributes.get(this,at);
	}
	SIMPLE_ERROR(BF("Missing attribute: %s")%at);
    };

    Symbol_sp getAttributeSymbol(const string& at) 
    {_OF();
	if ( this->_Attributes.contains(this,at) ) 
	{
	    return lisp_intern(this->_Attributes.get(this,at));
	} else 
	{
	    SIMPLE_ERROR(BF("Missing attribute: %s")%at);
	}
    };

    Symbol_sp getAttributeSymbolDefaultNil(const string& at) 
    {_OF();
	if ( this->_Attributes.contains(this,at) ) 
	{
	    return lisp_intern(this->_Attributes.get(this,at));
	}
	return lisp_symbolNil(_lisp);
    };


	//
	// Overloaded versions of getAttribute
	//
    void getAttribute(const string& name, Symbol_sp& sym)
    {
	sym = this->getAttributeSymbol(name);
    }

    void getAttribute(const string& name, string& v) {
	v = this->getAttributeString(name);
    }
    void getAttribute(const string& name, string& v, string defval) {
	v = this->getAttributeStringDefault(name,defval);
    }

    void getAttribute(const string& name, stringstream& v) {
        string ss;
	ss = this->getAttributeString(name);
	v.str(ss);
    }
    void getAttribute(const string& name, unsigned char& v) {
	v = this->getAttributeUChar(name);
    }
    void getAttribute(const string& name, unsigned short& v) {
	v = this->getAttributeUShort(name);
    }

    void getAttribute(const string& name, unsigned int& v) {
	v = this->getAttributeUInt(name);
    }
    void getAttribute(const string& name, unsigned int& v, unsigned int dv) {
	v = this->getAttributeUIntDefault(name,dv);
    }

    void getAttribute(const string& name, unsigned long& v) {
	v = this->getAttributeULong(name);
    }
    void getAttribute(const string& name, unsigned long& v, unsigned long& dv) {
	v = this->getAttributeULongDefault(name,dv);
    }
    void getAttribute(const string& name, LongLongInt& v) {
	v = this->getAttributeLongLongInt(name);
    }
    void getAttribute(const string& name, Bignum& v)
    {
	v = this->getAttributeBignum(name);
    }
    void getAttribute(const string& name, LongLongInt& v, LongLongInt& df) {
	v = this->getAttributeLongLongIntDefault(name,df);
    }
    void getAttribute(const string& name, int& v) {
	v = this->getAttributeInt(name);
    }
    void getAttribute(const string& name, int& v, int defval) {
	v = this->getAttributeIntDefault(name,defval);
    }

    void getAttribute(const string& name, double& v) {
	v = this->getAttributeDouble(name);
    }
    void getAttribute(const string& name, double& v, double defval) {
	v = this->getAttributeDoubleDefault(name,defval);
    }

    void getAttribute(const string& name, float& v) {
	v = this->getAttributeFloat(name);
    }
    void getAttribute(const string& name, float& v, float defval) {
	v = this->getAttributeFloatDefault(name,defval);
    }

    void getAttribute(const string& name, bool& v ) {
	v = this->getAttributeBool(name);
    }
    void getAttribute(const string& name, bool& v, bool defval) {
	v = this->getAttributeBoolDefault(name,defval);
    }




		//
		// Attribute type conversion routines.
		// These routines could throw exceptions if they
		// found invalid data
		//
    double	getAttributeDouble(const string& at) {
		return atof(this->getAttributeString(at).c_str());
    };
    double	getAttributeFloat(const string& at) {
		return atof(this->getAttributeString(at).c_str());
    };
    double	getAttributeDoubleDefault(const string& at,double df) {
			if ( this->_Attributes.contains(this,at) ) {
			    return atof(this->getAttributeString(at).c_str());
			} else {
			    return df;
			}
    };
    float	getAttributeFloatDefault(const string& at,float df) {
			if ( this->_Attributes.contains(this,at) ) {
			    return atof(this->getAttributeString(at).c_str());
			} else {
			    return df;
			}
    };
    unsigned char getAttributeUChar(const string& at) {
	string val = this->getAttributeString(at);
	return (unsigned char)(atoi(val.c_str()));
    };
    unsigned short getAttributeUShort(const string& at) {
	string val = this->getAttributeString(at);
	return (unsigned short)(atoi(val.c_str()));
    };
    uint getAttributeUInt(const string& at) {
	string val = this->getAttributeString(at);
	if ( val == "undef" )
	{
	    return UndefinedUnsignedInt;
	}
	return atol(val.c_str());
    };
    uint getAttributeUIntDefault(const string& at, unsigned int dv) {
	if ( this->_Attributes.contains(this,at) )
	{
	    string val = this->getAttributeString(at);
	    if ( val == "undef" )
	    {
		return UndefinedUnsignedInt;
	    }
	    return atol(val.c_str());
	} else
	{
	    return dv;
	}
    };
    unsigned long getAttributeULong(const string& at) {
			return atol(this->getAttributeString(at).c_str());
    };
    unsigned long getAttributeULongDefault(const string& at,unsigned long df) {
	if ( this->_Attributes.contains(this,at) ) {
	    return atol(this->getAttributeString(at).c_str());
	} else {
	    return df;
	}
    };
    Bignum getAttributeBignum(const string& at);
    LongLongInt getAttributeLongLongInt(const string& at);
    LongLongInt getAttributeLongLongIntDefault(const string& at,LongLongInt& df);

    int		getAttributeInt(const string& at) {
			return atoi(this->getAttributeString(at).c_str());
    };
    int		getAttributeIntDefault(const string& at,int df) {
			if ( this->_Attributes.contains(this,at) ) {
			    return atoi(this->getAttributeString(at).c_str());
			} else {
			    return df;
			}
    };
    bool	getAttributeBool(const string& at) 
    {_OF();
	string val;
	val = this->getAttributeString(at);
	if ( val== "true") return true;
	if ( val== "True") return true;
	if ( val== "TRUE") return true;
	if ( val== "yes") return true;
	if ( val== "Yes") return true;
	if ( val== "YES") return true;
	if ( val== "1" ) return true;
	if ( val== "false") return false;
	if ( val== "False") return false;
	if ( val== "FALSE") return false;
	if ( val== "no") return false;
	if ( val== "No") return false;
	if ( val== "NO") return false;
	if ( val== "0" ) return false;
	SIMPLE_ERROR(BF("Illegal boolean value: %s")%val);
    };

    bool	getAttributeBoolDefault(const string& at,bool d) {
		string val;
		if ( this->hasAttribute(at) ) {
		    val = this->getAttributeString(at);
		    if ( val== "true") return true;
		    if ( val== "True") return true;
		    if ( val== "TRUE") return true;
		    if ( val== "yes") return true;
		    if ( val== "Yes") return true;
		    if ( val== "YES") return true;
		    if ( val== "1" ) return true;
		    if ( val== "false") return false;
		    if ( val== "False") return false;
		    if ( val== "FALSE") return false;
		    if ( val== "no") return false;
		    if ( val== "No") return false;
		    if ( val== "NO") return false;
		    if ( val== "0" ) return false;
		}
		return d;
    };

    template <typename TYPE>
    void addAttribute(const string& at, TYPE val);
    void addAttributeFloat(const string& at, float val );
    void addAttribute(const string& at, Bignum& val);
    void addAttribute(const string& at, stringstream& val);
    void addAttribute(const string& at, Symbol_sp sym);
    void addAttribute(const string& at, const string& val);
    void addAttribute(const string& at, double val );
    void addAttribute(const string& at, float val );
    void addAttribute(const string& at, int val );
    void addAttribute(const string& at, unsigned int val );
    void addAttribute(const string& at, unsigned long val );
    void addAttribute(const string& at, LongLongInt val );
    void addAttribute(const string& at, bool val );
    void addAttributeString(const string& at, const string& val);
    void addAttributeDouble(const string& at, double val );
    void addAttributeInt(const string& at, int val);
    void addAttributeLongLongInt(const string& at, LongLongInt val);
    void addAttributeUInt(const string& at, unsigned int val);
    void addAttributeULong(const string& at, unsigned long val);
    void addAttributeHex(const string& at, int val);
    void addAttributeBool(const string& at, bool b );

    string asString();

  /*! Serialize an enumerated type
   */
    template <typename SymbolEnumType>
        void attributeSymbolEnumIfNotDefault( const string& key, SymbolEnumType& val, SymbolToEnumConverter_sp converter, SymbolEnumType defVal )
    {_OF();
	    if ( this->saving()) 
	    {
		if ( val != defVal )
		{
		    converter->throwIfUnrecognizedEnum<SymbolEnumType>(val);
		    string enumStr = converter->symbolForEnum<SymbolEnumType>(val)->fullName();
		    this->addAttributeString(key,enumStr);
		}
	    } else 
	    {
		if ( this->hasAttribute(key) )
		{
		    string enumStr;
		    this->getAttribute(key,enumStr);
		    Symbol_sp sym = _lisp->intern(enumStr);
		    if ( !converter->recognizesSymbol(sym))
		    {
			SIMPLE_ERROR(BF("Loading: did not recognize enumerated value: %s") % sym->__repr__());
		    }
		    val = converter->enumForSymbol<SymbolEnumType>(sym);
		} else
		{
		    val = defVal;
		}
	    }
	}


    template <typename SymbolEnumType>
        void attributeSymbolEnum( const string& key, SymbolEnumType& val, SymbolToEnumConverter_sp converter )
    {_OF();
	    if ( this->saving()) 
	    {
		converter->throwIfUnrecognizedEnum<SymbolEnumType>(val);
		string enumStr = converter->symbolForEnum<SymbolEnumType>(val)->fullName();
		this->addAttributeString(key,enumStr);
	    } else 
	    {
		string enumStr;
		this->getAttribute(key,enumStr);
		Symbol_sp enumSym = _lisp->intern(enumStr);
		if ( !converter->recognizesSymbol(enumSym) )
		{
		    SIMPLE_ERROR(BF("Loading: did not recognize enumerated value: %s ")% enumSym->__repr__());
		}
		val = converter->enumForSymbol<SymbolEnumType>(enumSym);
	    }
	}




  /*! Serialize an enumerated type that has symbols associated with it in a SymbolToEnumConverter stored
    in the HiddenBinder. If the attribute is not defined then return the default value or if val == defVal then don't
    writing anything to the archive
   */
    template <typename SymbolEnumType>
        void attributeSymbolEnumHiddenConverterIfNotDefault( const string& key, SymbolEnumType& val, Symbol_sp hiddenConverterSymbol, SymbolEnumType defVal )
    {_OF();
	SymbolToEnumConverter_sp converter = lisp_hiddenBinderLookup(_lisp,hiddenConverterSymbol)->as<SymbolToEnumConverter_O>();
	if ( this->saving()) 
	{
	    if ( val != defVal )
	    {
		converter->throwIfUnrecognizedEnum<SymbolEnumType>(val);
		string enumStr = converter->symbolForEnum<SymbolEnumType>(val)->fullName();
		this->addAttributeString(key,enumStr);
	    }
	} else 
	{
	    if ( this->hasAttribute(key) )
	    {
		string enumStr;
		this->getAttribute(key,enumStr);
		Symbol_sp enumSym = _lisp->intern(enumStr);
		if ( !converter->recognizesSymbol(enumSym) )
		{
		    SIMPLE_ERROR(BF("Loading: did not recognize enumerated value: %s") % enumSym->fullName());
		}
		val = converter->enumForSymbol<SymbolEnumType>(enumSym);
	    } else
	    {
		val = defVal;
	    }
	}
    }


    template <typename SymbolEnumType>
        void attributeSymbolEnumHiddenConverter( const string& key, SymbolEnumType& val, Symbol_sp hiddenConverterSymbol )
    {_OF();
	SymbolToEnumConverter_sp converter = lisp_hiddenBinderLookup(_lisp,hiddenConverterSymbol)->as<SymbolToEnumConverter_O>();
	if ( this->saving()) 
	{
	    converter->throwIfUnrecognizedEnum<SymbolEnumType>(val);
	    string enumStr = converter->symbolForEnum<SymbolEnumType>(val)->fullName();
	    this->addAttributeString(key,enumStr);
	} else 
	{
	    string enumStr;
	    this->getAttribute(key,enumStr);
	    Symbol_sp enumSym = _lisp->intern(enumStr);
	    if ( !converter->recognizesSymbol(enumSym))
	    {
		SIMPLE_ERROR(BF("Loading: did not recognize enumerated value: %s ")% enumStr);
	    }
	    val = converter->enumForSymbol<SymbolEnumType>(enumSym);
	}
    }







  /*! Serialize an enumerated type
   */
    template <typename EnumType>
        void attributeEnumIfNotDefault( const string& key, EnumType& val, NullTerminatedEnumAssociation kvp[], EnumType defVal )
    {_OF();
	if ( this->saving())
	{
	    if ( val != defVal )
	    {
		T_sp sobj = translate::to_object<EnumType>::convert(val);
		core::Symbol_sp sym = sobj->as<core::Symbol_O>();
		string symStr = sym->fullName();
		this->addAttributeString(key,symStr);
	    }
	} else {
	    if ( this->hasAttribute(key) )
	    {
		string str;
		this->getAttribute(key,str);
		core::Symbol_sp sym = _lisp->intern(str);
		val = translate::from_object<EnumType>::convert(sym);
	    } else
	    {
		val = defVal;
	    }
	}
    }


  /*! Serialize an enumerated type
   */
    template <typename EnumType>
        void attributeEnum( const string& key, EnumType& val, NullTerminatedEnumAssociation kvp[] )
    {_OF();
	    if ( this->saving()) {
		string	enumStr;
		int i;
		for ( i=0; kvp[i]._Key!=""; i++ ) {
		    if ( val == kvp[i]._Enum ) break;
		}
		if ( kvp[i]._Key == "" ) {
		    stringstream ss;
		    ss << val;
		    SIMPLE_ERROR(BF("Could not save enumerated value: ")%ss.str());
		}
		enumStr = kvp[i]._Key;
		this->addAttributeString(key,enumStr);
	    } else {
		string enumStr;
		this->getAttribute(key,enumStr);
		int i;
		for ( i=0; kvp[i]._Key!=""; i++ ) {
		    if ( enumStr == kvp[i]._Key ) break;
		}
		if ( kvp[i]._Key=="" ) {
		    SIMPLE_ERROR(BF("Loading: did not recognize enumerated value: %s")%enumStr);
		}
		val = (EnumType)(kvp[i]._Enum);
	    }
	}

  //! Read/write the attribute
    template <typename Type>
        void attribute(const string& key, Type& val )
	{
	    if ( this->saving()) {
		this->addAttribute(key,val);
	    } else {
		this->getAttribute(key,val);
	    }
	}
    void attributeSymbolIfNotNil(const string& key, Symbol_sp& sym);

  /*! Read/write the attribute,
   * if it is zero or equivelent then don't write it
   * If it isn't in the serialization stream then set it to zero.
   */
    template <typename Type>
        void attributeIfNotDefault(const string& key, Type& val, Type defaultVal )
    {_G();
	    if ( this->saving()) {
	    	if ( val != defaultVal ) {
                    this->addAttribute(key,val);
		}
	    } else {
		this->getAttribute(key,val,defaultVal);
	    }
	}

  /*! Read/write the attribute if its defined
   */
    template <typename Type>
        void attributeIfDefined(const string& key, Type& val, bool& defined)
    {_G();
	    if ( this->saving()) {
	    	if ( defined ) {
                    this->addAttribute(key,val);
		}
	    } else {
	        defined = false;
	        if ( this->hasAttribute(key) ) {
		    defined = true;
		    this->getAttribute(key,val);
		}
	    }
	}


    void	dumpChild( const string& prefix);
    void	dumpChildToStream( ostream& o, const string& prefix);


    void	fillVectorNodesIfNameIs(int depth, ArchiveP me, VectorNodes& vnodes, const string& name);
    void	setUniqueIdAutomatically();
    void	setTextUniqueId(const string& uid);
    void	setNumericUniqueId(const int uid);
    void	setUniqueIdHandle(handleType huid);

public:
    void	addChild(ArchiveP child );
public:

    vector<ArchiveP>::iterator begin_Children();
    vector<ArchiveP>::iterator end_Children();

    virtual bool isNode() { return true; };

    template<class SimpleClass>
    void archiveSharedObjectRaw( const string& uid, const string& nodeName, gctools::smart_ptr<SimpleClass>& plainObject, bool mustBeDefined )
    { _G();
            if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		if ( !plainObject )
		{
		    if ( mustBeDefined )
		    {
		        SIMPLE_ERROR(BF("Tried to archive a shared object that is undefined"));
		    } else
		    {
		        return;
		    }
		}
		ArchiveP  plainChild;
		plainChild = this->newArchiveNode();
		plainChild->setTextUniqueId(uid);
		plainChild->setClasslessNodeName(nodeName);
		this->addChild(plainChild);
		plainObject->archive(plainChild);
	    } else
	    { _BLOCK_TRACE("Loading");
		ArchiveP plainNode = NULL;
		if ( !this->hasChildWithUniqueId(uid) )
		{
		    if ( !mustBeDefined )
		    {
		        plainObject.reset();
			return;
		    }
		    SIMPLE_ERROR(BF("Expecting nodeName(%s) got (%s)") % nodeName % plainNode->getNodeName() );
		}
		plainNode = this->childWithUniqueId(uid);
		ASSERT(plainNode!=NULL);
		plainNode->setRecognized(true);
		plainObject = SimpleClass::create();
		ASSERTNOTNULL(plainObject);
		plainObject->archive(plainNode);
	    }
        }

    template<class SimpleClass>
    void archiveSharedObject( const string& uid, const string& nodeName, gctools::smart_ptr<SimpleClass>& plainObject )
    { _G();
	    this->archiveSharedObjectRaw(uid,nodeName,plainObject,true);
	}

    template<class SimpleClass>
    void archiveSharedObjectIfDefined( const string& uid, const string& nodeName, gctools::smart_ptr<SimpleClass>& plainObject )
    { _G();
	    this->archiveSharedObjectRaw(uid,nodeName,plainObject,false);
	}



	/*! Archive any POD class.  All it neds is a archive method
	 * void archive(ArchiveP node);
	 */
    template<class SimpleClass>
	void archivePlainObject( const string& uid, const string& nodeName, SimpleClass& plainObject )
    { _G();
            if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		ArchiveP  plainChild;
		plainChild = this->newArchiveNode();
		plainChild->setTextUniqueId(uid);
		plainChild->setClasslessNodeName(nodeName);
		this->addChild(plainChild);
		plainObject.archive(plainChild);
	    } else
	    { _BLOCK_TRACE("Loading");
		ArchiveP plainNode;
		plainNode = this->childWithUniqueId(uid);
		if ( !plainNode->isNamed(nodeName) ) {
		    SIMPLE_ERROR(BF("Expecting nodeName(%s) got(%s)") % nodeName % plainNode->getNodeName());
		}
		plainNode->setRecognized(true);
		plainObject.archive(plainNode);
	    }
        }

    template <typename pType>
        void archiveMapKeyStringValuePOD( const string& uid, map<string,pType>& v )
    { _G();
	    ArchiveP	listNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		listNode = this->newArchiveNode();
		this->addChild(listNode);
		ArchiveP plainChild;
		listNode->setClasslessNodeName("podMapKeyStringValuePOD");
		listNode->setTextUniqueId(uid);
		typename map<string,pType>::iterator oi;
		stringstream suid;
		for ( oi=v.begin(); oi!=v.end(); oi++ ) 
		{
		    suid.str("");
		    suid << oi->first;
		    plainChild = this->newArchiveNode();
		    plainChild->setTextUniqueId(suid.str());
		    plainChild->setClasslessNodeName("mapPOD");
		    plainChild->attribute("v",oi->second);
		    listNode->addChild(plainChild);
		}
	    } else
	    { _BLOCK_TRACE("Loading");
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    pType pod = pType();
		    listNode = this->childWithUniqueId(uid);
		    listNode->setRecognized(true);
		    VectorNodes::iterator in;
		    for ( in=listNode->begin_Children(); in!= listNode->end_Children(); in++ ) 
		    {
			(*in)->attribute("v",pod);
			v[(*in)->getUniqueIdCharactersIfMissingThrow()] = pod;
		    }
		} else
		{
		    SIMPLE_ERROR(BF("Missing MapKeyStringValuePOD with uid("+uid+")"));
		}
	    }
	}

    template <typename firstType, typename secondType, typename compareType>
        void archive_map( const string& uid, map<firstType,secondType,compareType>& m )
    { _G();
	    ArchiveP	listNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		listNode = this->newArchiveNode();
		this->addChild(listNode);
		ArchiveP elementChild, keyChild, valueChild;
		listNode->setClasslessNodeName("mapGen");
		listNode->setTextUniqueId(uid);
		typename map<firstType,secondType,compareType>::iterator oi;
		stringstream suid;
		uint idx=0;
		for ( oi=m.begin(); oi!=m.end(); oi++,idx++ )
		{
		    suid.str("");
		    suid << idx;
		    elementChild = this->newArchiveNode();
		    elementChild->setTextUniqueId(suid.str());
		    elementChild->setClasslessNodeName("MapElement");
		    listNode->addChild(elementChild);
		    keyChild = this->newArchiveNode();
		    valueChild = this->newArchiveNode();
		    elementChild->addChild(keyChild);
		    elementChild->addChild(valueChild);
		    keyChild->saveObjectWithinNodeWithUid("key",oi->first);
		    valueChild->saveObjectWithinNodeWithUid("val",oi->second);
		}
	    } else
	    { _BLOCK_TRACE("Loading");
		m.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    firstType	keyObj;
		    secondType	valueObj;
		    listNode = this->childWithUniqueId(uid);
		    listNode->setRecognized(true);
		    VectorNodes::iterator in;
		    for ( in=listNode->begin_Children(); in!= listNode->end_Children(); in++ ) 
		    {
			(*in)->setRecognized(true);
			keyObj = RP_Create<typename firstType::element_type>(_lisp);
			valueObj = RP_Create<typename secondType::element_type>(_lisp);
			(*in)->archiveObject("key",keyObj);
			(*in)->archiveObject("val",valueObj);
			m.insert(pair<firstType,secondType>(keyObj,valueObj));
		    }
		} else
		{
		    SIMPLE_ERROR(BF("Missing map with uid("+uid+")"));
		}
	    }
	}





    template <typename pType>
        void archiveVectorPlainObjects( const string& uid, vector<pType>& v )
    { _G();
	    ArchiveP	listNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		listNode = this->newArchiveNode();
		this->addChild(listNode);
		ArchiveP plainChild;
		listNode->setClasslessNodeName("podList");
		listNode->setTextUniqueId(uid);
		typename vector<pType>::iterator oi;
		stringstream suid;
		for ( oi=v.begin(); oi!=v.end(); oi++ ) {
		    suid.str("");
		    suid << oi-v.begin();
		    plainChild = this->newArchiveNode();
		    plainChild->setTextUniqueId(suid.str());
		    plainChild->setClasslessNodeName("VectorPOD");
		    listNode->addChild(plainChild);
		    (*oi).archive(plainChild);
		}
	    } else
	    { _BLOCK_TRACE("Loading");
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    pType pod(_lisp);
		    listNode = this->childWithUniqueId(uid);
		    listNode->setRecognized(true);
		    VectorNodes::iterator in;
		    for ( in=listNode->begin_Children(); in!= listNode->end_Children(); in++ ) {
		        pod.archive(*in);
			v.push_back(pod);
		    }
		} else
		{
		    SIMPLE_ERROR(BF("Missing VectorPOD with uid("+uid+")"));
		}
	    }
	}
    template<class SimpleClass>
	void archivePlainObjectIfDefined( const string& uid, const string& nodeName,
					bool isDefined, SimpleClass& plainObject )
    { _G();
            if ( this->saving() )
	    { _BLOCK_TRACE("saving");
	    	if ( isDefined )
		{
		    ArchiveP  plainChild;
	            plainChild = this->newArchiveNode();
		    plainChild->setClasslessNodeName(nodeName);
		    plainChild->setTextUniqueId(uid);
	            this->addChild(plainChild);
	            plainObject.archive(plainChild);
		}
	    } else
	    { _BLOCK_TRACEF(BF("loading and looking for uid(%s) in node: %s") % uid % this->description() );
	    		// If there is a node with this name then
			// extract the data from it
	        if ( this->hasChildWithUniqueId(uid) )
		{
		    ArchiveP plainNode;
		    plainNode = this->childWithUniqueId(uid);
		    if ( !plainNode->isNamed(nodeName) )
		    {
			SIMPLE_ERROR(BF("Expecting nodeName("+nodeName+") got ("+plainNode->getNodeName()+")"));
		    }
		    plainNode->setRecognized(true);
		    plainObject.archive(plainNode);
		} else {
		    plainObject.setIsDefined(false);
		}
	    }
        }

    template <class oclass>
    void archiveOnlyObjectOfClass( gctools::smart_ptr<oclass>& obj )
    { _G();
	    if ( this->loading() )
	    { _BLOCK_TRACE("Loading");
		T_sp rawObj = T_O::_nil;
		this->loadOnlyObjectOfClassWithinNode(oclass::static_classSymbol(), rawObj );
		if ( rawObj->isNil() ) {
		    obj = oclass::_nil;
		} else {
		    obj = downcast<oclass>(rawObj);
		}
	    } else
	    { _BLOCK_TRACE("Saving");
		this->saveOnlyObjectOfClassWithinNode( oclass::static_classSymbol(), obj );
	    }
	}
/*!
 * I'm copying the boost serialization library handling of pointers
 * 	- Loading
 * 		-# Extract the shared pointer tag from the Node
 * 		-# Identify the Node that corresponds to the pointer tag
 * 		-# If the node says the object has already been created
 * 			then return the object address (tag now swizzled)
 * 		-# Otherwise create the object from the node.
 * 		-# Return the address of the object (tag now swizzled).
 *	- Saving
 *		-# Check if the archive recognizes the object.
 *		-# If its found then add a node that references it.
 *		-# If it isn't found then archive it here.
 */
    template <class oclass>
    void archiveObject( const string& uid, gctools::smart_ptr<oclass>& obj )
    { _G();
	    if ( this->loading() )
	    { _BLOCK_TRACE("Loading");
		T_sp rawObj = T_O::_nil;
		this->loadObjectWithinNodeWithUid( uid, rawObj );
		obj = downcast<oclass>(rawObj);
	    } else
	    { _BLOCK_TRACE("Saving");
		this->saveObjectWithinNodeWithUid( uid, obj );
	    }
	}

    template <class oclass>
    void archiveObjectIfDefined( const string& uid, gctools::smart_ptr<oclass>& obj )
    { _G();
	    if ( this->loading() )
	    { _BLOCK_TRACEF(BF("Loading uid(%s) if it exists")%uid.c_str() );
	        if ( this->hasChildWithUniqueId( uid ) ) {
		    this->archiveObject( uid, obj );
		} else {
		    obj = oclass::_nil;
		}
	    } else
	      { _BLOCK_TRACEF(BF("Saving uid(%s)")%uid.c_str() );
	        if ( obj->notNil() )
		{
		    this->saveObjectWithinNodeWithUid( uid, obj );
		}
	    }
	}

/*!	Archive a Collection Object
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename OType>
    void archiveContainerIfNotEmpty( const string& uid, gctools::smart_ptr<OType>& v )
    { _G();
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
	        if ( v->notEmpty() )
		{
		    this->archiveObject(uid,v);
		}
	    } else
	    { _BLOCK_TRACE("Loading");
//		printf( "archiveVector0>>Looking for children with uid(%s)\n", uid.c_str() );
	        if ( this->hasChildWithUniqueId(uid) ) {
		    LOG(BF("Found child with uid=%s")% uid );
		    	// If the object exists then load it
		    this->archiveObject(uid,v);
		    LOG(BF("Loaded restraints number = %d") % v->size() );
		} else
		{
		    LOG(BF("Could not find child with uid=%s") % uid );
		    	// If the entry doesn't exist then create an empty one
		    v = OType::create();
		}
	    }
	}

/*!	Archive a List Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename OType>
        void archiveVector0Raw( const string& uid, Vector0<OType>& v, bool failIfMissing )
    { _G();
	    ArchiveP	listNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
//		    printf( "archiveVector0>> Archive start object\n" );
		listNode = this->newArchiveNode();
		this->addChild(listNode);
		listNode->setClasslessNodeName("podList");
		listNode->setTextUniqueId(uid);
		typename Vector0<OType>::iterator oi;
		stringstream suid;
		for ( oi=v.begin(); oi!=v.end(); oi++ ) {
		    suid.str("");
		    suid << oi-v.begin();
		    listNode->archiveObject(suid.str(), *oi );
		}
//		printf( "archiveVector0>> Done saving\n" );
	    } else
	    { _BLOCK_TRACE("Loading");
//		printf( "archiveVector0>>Looking for children with uid(%s)\n", uid.c_str() );
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
                    gctools::smart_ptr<OType> object;
		    listNode = this->childWithUniqueId(uid);
		    listNode->setRecognized(true);
		    VectorNodes::iterator in;
		    for ( in=listNode->begin_Children(); in!= listNode->end_Children(); in++ ) {
			object = downcast<OType>(this->loadObjectDirectly(*in));
			ASSERTNOTNULL(object);
			v.append(object);
		    }
		} else
		{
		    if ( failIfMissing )
		    {
		        SIMPLE_ERROR(BF("Missing List node with uid(%s)")%uid);
		    }
		}
	    }
	}

    template <typename OType>
        void archiveVector0( const string& uid, Vector0<OType>& v )
	{
	    this->archiveVector0Raw(uid,v,true);
	}

    template <typename OType>
        void archiveVector0IfDefined( const string& uid, Vector0<OType>& v )
	{
	    this->archiveVector0Raw(uid,v,false);
	}

/*!	Archive a Set Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename OType>
        void archiveSetRaw( const string& uid, Set<OType>& v, bool failIfMissing )
    { _G();
	    ArchiveP	listNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
//		    printf( "archiveSet>> Archive start object\n" );
		listNode = this->newArchiveNode();
		this->addChild(listNode);
		listNode->setClasslessNodeName("podSet");
		listNode->setTextUniqueId(uid);
		typename Set<OType>::iterator oi;
		stringstream suid;
		uint uidi = 0;
		for ( oi=v.begin(); oi!=v.end(); oi++, uidi++ ) {
		    suid.str("");
		    suid << uidi;
		    T_sp obj = *oi;
		    listNode->archiveObject(suid.str(), obj );
		}
//		printf( "archiveSet>> Done saving\n" );
	    } else
	    { _BLOCK_TRACE("Loading");
//		printf( "archiveSet>>Looking for children with uid(%s)\n", uid.c_str() );
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
                    gctools::smart_ptr<OType> object;
		    listNode = this->childWithUniqueId(uid);
		    listNode->setRecognized(true);
		    VectorNodes::iterator in;
		    for ( in=listNode->begin_Children(); in!= listNode->end_Children(); in++ ) {
			object = downcast<OType>(this->loadObjectDirectly(*in));
			ASSERTNOTNULL(object);
			v.insert(object);
		    }
		} else
		{
		    if ( failIfMissing )
		    {
		        SIMPLE_ERROR(BF("Missing Set node with uid(%s)")%uid);
		    }
		}
	    }
	}

    template <typename OType>
        void archiveSet( const string& uid, Set<OType>& v )
	{
	    this->archiveSetRaw(uid,v,true);
	}

    template <typename OType>
        void archiveSetIfDefined( const string& uid, Set<OType>& v )
	{
	    this->archiveSetRaw(uid,v,false);
	}

/*!	Archive a OrderedSet Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename OType>
        void archiveOrderedSetRaw( const string& uid, OrderedSet<OType>& v, bool failIfMissing )
    { _G();
	    ArchiveP	setNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
//		    printf( "archiveOrderedSet>> Archive start object\n" );
		setNode = this->newArchiveNode();
		this->addChild(setNode);
		setNode->setClasslessNodeName("OrderedSet");
		setNode->setTextUniqueId(uid);
		typename OrderedSet<OType>::iterator oi;
                gctools::smart_ptr<OType> obj;
		stringstream suid;
		int suidi = 1;
		for ( oi=v.begin(); oi!=v.end(); oi++ ) {
		    suid.str("");
		    suid << suidi++;
		    obj = *oi;
		    setNode->archiveObject(suid.str(), obj );
		}
//		printf( "archiveOrderedSet>> Done saving\n" );
	    } else
	    { _BLOCK_TRACE("Loading");
//		printf( "archiveOrderedSet>>Looking for children with uid(%s)\n", uid.c_str() );
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
                    gctools::smart_ptr<OType> object;
		    setNode = this->childWithUniqueId(uid);
		    setNode->setRecognized(true);
		    VectorNodes::iterator in;
		    for ( in=setNode->begin_Children(); in!= setNode->end_Children(); in++ ) {
			object = downcast<OType>(this->loadObjectDirectly(*in));
			ASSERTNOTNULL(object);
			v.insert(object);
		    }
		} else
		{
		    if ( failIfMissing ) {
		        SIMPLE_ERROR(BF("Missing OrderedSet node with uid(%s)") % uid );
		    }
		}
	    }
	}

    template <typename OType>
        void archiveOrderedSet( const string& uid, OrderedSet<OType>& v )
	{
	    this->archiveOrderedSetRaw(uid,v,true);
	}

    template <typename OType>
        void archiveOrderedSetIfDefined( const string& uid, OrderedSet<OType>& v )
	{
	    this->archiveOrderedSetRaw(uid,v,false);
	}
















/*!	Archive a map of strings->Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <class OType>
        void archiveSymbolMapRaw( const string& uid, SymbolMap<OType>& v, bool ignoreIfMissing )
    { _G();
	    ArchiveP	mapNode;
	    if ( this->saving() )
	    { _BLOCK_TRACEF(BF("Saving %d entries") % v.size() );
		if ( v.size() == 0 && ignoreIfMissing ) return;
		typename SymbolMap<OType>::iterator	oi;
		mapNode = this->newArchiveNode();
		this->addChild(mapNode);
		mapNode->setClasslessNodeName("podSymbolMap");
		mapNode->setTextUniqueId(uid);
		for ( oi=v.begin(); oi!=v.end(); oi++ )
		{ _BLOCK_TRACEF(BF("Archiving map entry with name(%s)") %oi->first );
		    mapNode->archiveObject(oi->first->fullName(),oi->second);
		}
	    } else
	      { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid);
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    mapNode = this->childWithUniqueId(uid);
		    mapNode->setRecognized(true);
		    VectorNodes::iterator	ci;
		    string	key;
                    gctools::smart_ptr<OType>  object;
		    for ( ci=mapNode->begin_Children(); ci!=mapNode->end_Children(); ci++ )
		    {
			object = downcast<OType>(mapNode->loadObjectDirectly(*ci));
			ASSERTNOTNULL(object);
			key = (*ci)->getUniqueIdCharactersIfMissingThrow();
			Symbol_sp skey = lisp_intern(key);
			v.set(skey,object);
		    }
		} else
		{
	            if ( !ignoreIfMissing )
		    {
			SIMPLE_ERROR(BF("SymbolMap with uid(%s) is missing") % uid );
		    }
		}
	    }
	}

    template <class OType>
        void archiveSymbolMap(const string& uid, SymbolMap<OType>& v )
	{
	    this->archiveSymbolMapRaw(uid,v,false);
	}

    template <class OType>
        void archiveSymbolMapIfDefined(const string& uid, SymbolMap<OType>& v )
	{
	    this->archiveSymbolMapRaw(uid,v,true);
	}










/*!	Archive a map of strings->Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <class OType>
        void archiveWeakSymbolMapRaw( const string& uid, WeakSymbolMap<OType>& v, bool failIfMissing )
    { _G();
	    ArchiveP	mapNode;
	    if ( this->saving() )
	    { _BLOCK_TRACEF(BF("Saving %d entries") % v.size() );
		typename SymbolMap<OType>::iterator	oi;
		mapNode = this->newArchiveNode();
		this->addChild(mapNode);
		mapNode->setClasslessNodeName("podWeakSymbolMap");
		mapNode->setTextUniqueId(uid);
		for ( oi=v.begin(); oi!=v.end(); oi++ )
		{ _BLOCK_TRACEF(BF("Archiving map entry with name(%s)") %oi->first );
		    mapNode->archiveWeakPointer("val",oi->second);
		}
	    } else
	      { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid);
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    mapNode = this->childWithUniqueId(uid);
		    mapNode->setRecognized(true);
		    VectorNodes::iterator	ci;
		    string	key;
                    gctools::smart_ptr<OType>  object;
		    for ( ci=mapNode->begin_Children(); ci!=mapNode->end_Children(); ci++ )
		    {
			this->loadWeakPointerInAttribute( *ci, "val", object, OType::static_classSymbol(), true );
			ASSERTNOTNULL(object);
			key = (*ci)->getUniqueIdCharactersIfMissingThrow();
			Symbol_sp skey = lisp_intern(key);
			v.set(skey,weak_downcast<OType>(object,_lisp));
		    }
		} else
		{
	            if ( failIfMissing )
		    {
			SIMPLE_ERROR(BF("WeakSymbolMultiMap with uid(%s) is missing") % uid);
		    }
		}
	    }
	}

    template <class OType>
        void archiveWeakSymbolMap(const string& uid, WeakSymbolMap<OType>& v )
	{
	    this->archiveWeakSymbolMapRaw(uid,v,true);
	}

    template <class OType>
        void archiveWeakSymbolMapIfDefined(const string& uid, WeakSymbolMap<OType>& v )
	{
	    this->archiveWeakSymbolMapRaw(uid,v,false);
	}






/*!	Archive a map of strings->Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */

    template <class OType>
        void archiveWeakSymbolMultiMapRaw( const string& uid, WeakSymbolMultiMap<OType>& v, bool failIfMissing, bool ignoreBrokenWeakPointers )
    { _G();
	    ArchiveP	mapNode, childNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		typename WeakSymbolMultiMap<OType>::iterator	oi;
		mapNode = this->newArchiveNode();
		this->addChild(mapNode);
		mapNode->setClasslessNodeName("WeakSymbolMultiMap");
		mapNode->setTextUniqueId(uid);
		for ( oi=v.begin(); oi!=v.end(); oi++ )
		{
		    childNode = mapNode->createChildNode("Entry");
		    childNode->addAttribute("_mkey",oi->first);
		    childNode->archiveWeakPointerSuppressNodeIfBrokenOrNil("val",oi->second);
		}
	    } else
	    { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid.c_str());
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    mapNode = this->childWithUniqueId(uid);
		    mapNode->setRecognized(true);
		    VectorNodes::iterator	ci;
		    T_wp  weakObject;
		    Symbol_sp key;
		    for ( ci=mapNode->begin_Children(); ci!=mapNode->end_Children(); ci++ )
		    {
			this->loadWeakPointerInAttribute( *ci, "val", weakObject, OType::static_classSymbol(), false );
			ASSERT(weakObject.use_count()!=0);
			key = (*ci)->getAttributeSymbol("_mkey");
			if ( weakObject.lock()->notNil() )
			{
			    v.insert2(key, weak_downcast<OType>(weakObject,_lisp));
			} else
			{
			    v.insert2(key,OType::_nil);
			}
		    }
		} else
		{
		    if ( failIfMissing )
		    {
			SIMPLE_ERROR(BF("WeakSymbolMultiMap with uid(%s) is missing")%uid);
		    }
		}
	    }
	}

    template <class OType>
        void archiveWeakSymbolMultiMap(const string& uid, WeakSymbolMultiMap<OType>& v )
	{
	    this->archiveWeakSymbolMultiMapRaw(uid,v,true,false);
	}

    template <class OType>
        void archiveWeakSymbolMultiMapIfDefined(const string& uid, WeakSymbolMultiMap<OType>& v )
	{
	    this->archiveWeakSymbolMultiMapRaw(uid,v,false,false);
	}


















/*!	Archive a map of strings->Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <class OType>
        void archiveMapRaw( const string& uid, StringMap<OType>& v, bool failIfMissing )
    { _G();
	    ArchiveP	mapNode;
	    if ( this->saving() )
	    { _BLOCK_TRACEF(BF("Saving %d entries") % v.size() );
		typename StringMap<OType>::iterator	oi;
		mapNode = this->newArchiveNode();
		this->addChild(mapNode);
		mapNode->setClasslessNodeName("podMap");
		mapNode->setTextUniqueId(uid);
		for ( oi=v.begin(); oi!=v.end(); oi++ )
		{ _BLOCK_TRACEF(BF("Archiving map entry with name(%s)") % oi->first );
		    mapNode->archiveObject(oi->first,oi->second);
		}
	    } else
	      { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid.c_str());
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
		    mapNode = this->childWithUniqueId(uid);
		    mapNode->setRecognized(true);
		    VectorNodes::iterator	ci;
		    string	key;
                    gctools::smart_ptr<OType>  object;
		    for ( ci=mapNode->begin_Children(); ci!=mapNode->end_Children(); ci++ )
		    {
			object = downcast<OType>(mapNode->loadObjectDirectly(*ci));
			ASSERTNOTNULL(object);
			key = (*ci)->getUniqueIdCharactersIfMissingThrow();
			v.set(key,object);
		    }
		} else
		{
	            if ( failIfMissing )
		    {
			SIMPLE_ERROR(BF("Map with uid(%s) is missing") % uid);
		    }
		}
	    }
	}

    template <class OType>
        void archiveMap(const string& uid, StringMap<OType>& v )
	{
	    this->archiveMapRaw(uid,v,true);
	}

    template <class OType>
        void archiveMapIfDefined(const string& uid, StringMap<OType>& v )
	{
	    this->archiveMapRaw(uid,v,false);
	}



/*!	Archive a map of strings->WeakObjects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <class OType>
        void archiveWeakMap( const string& uid, WeakMap<OType>& v ) { _G();
	    ArchiveP	mapNode, childNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		typename WeakMap<OType>::iterator	oi;
		mapNode = this->newArchiveNode();
		this->addChild(mapNode);
		mapNode->setClasslessNodeName("podMap");
		mapNode->setTextUniqueId(uid);
		string	ss;
		for ( oi=v.begin(); oi!=v.end(); oi++ )
		{
		    childNode = mapNode->createChildNode("Entry");
		    childNode->setTextUniqueId(oi->first);
		    childNode->archiveWeakPointer("val",oi->second);
		}
	    } else
	    { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid.c_str());
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
//		    printf( "archiveWeakMap>>found the node with uid(%s)\n", uid.c_str() );
		    mapNode = this->childWithUniqueId(uid);
		    mapNode->setRecognized(true);
		    VectorNodes::iterator	ci;
		    T_wp  object;
		    string key;
		    for ( ci=mapNode->begin_Children(); ci!=mapNode->end_Children(); ci++ )
		    {
			this->loadWeakPointerInAttribute( *ci, "val", object, OType::static_classSymbol(), true );
			key = (*ci)->getUniqueIdCharactersIfMissingThrow();
			ASSERTNOTNULL(object);
			v.set(key, weak_downcast<OType>(object,_lisp));
		    }
		}
	    }
	}


    template <class OType>
        void archiveWeakMultiStringMapRaw( const string& uid, WeakMultiStringMap<OType>& v, bool failIfMissing, bool ignoreBrokenWeakPointers )
    { _G();
	    ArchiveP	mapNode, childNode;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
		typename WeakMultiStringMap<OType>::iterator	oi;
		mapNode = this->newArchiveNode();
		this->addChild(mapNode);
		mapNode->setClasslessNodeName("WeakMultiStringMap");
		mapNode->setTextUniqueId(uid);
		string	ss;
		for ( oi=v.begin(); oi!=v.end(); oi++ )
		{
		    childNode = mapNode->createChildNode("Entry");
		    childNode->addAttribute("_mkey",oi->first);
//		    childNode->setNecessaryValidWeakPointer(oi->second);
		    childNode->archiveWeakPointerSuppressNodeIfBrokenOrNil("val",oi->second);
		}
	    } else
	    { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid.c_str());
		v.clear();
	        if ( this->hasChildWithUniqueId(uid) ) {
//		    printf( "archiveWeakMultiStringMap>>found the node with uid(%s)\n", uid.c_str() );
		    mapNode = this->childWithUniqueId(uid);
		    mapNode->setRecognized(true);
		    VectorNodes::iterator	ci;
		    T_wp  weakObject;
		    string key;
		    for ( ci=mapNode->begin_Children(); ci!=mapNode->end_Children(); ci++ )
		    {
			this->loadWeakPointerInAttribute( *ci, "val", weakObject, OType::static_classSymbol(), false );
			(*ci)->getAttribute("_mkey",key);
			ASSERT(weakObject.use_count()!=0);
			if ( weakObject.lock()->notNil() )
			{
			    v.insert2(key, weak_downcast<OType>(weakObject,_lisp));
			} else
			{
			    v.insert2(key,OType::_nil);
			}
		    }
		} else
		{
		    if ( failIfMissing )
		    {
			SIMPLE_ERROR(BF("Map with uid(%s) is missing") % uid);
		    }
		}
	    }
	}

    template <class OType>
        void archiveWeakMultiStringMapIfDefined(const string& uid, WeakMultiStringMap<OType>&v)
	{
	    this->archiveWeakMultiStringMapRaw(uid,v,false,false);
	}

    template <class OType>
        void archiveWeakMultiStringMap(const string& uid, WeakMultiStringMap<OType>&v)
	{
	    this->archiveWeakMultiStringMapRaw(uid,v,true,false);
	}

    template <class OType>
        void archiveWeakMultiStringMapIgnoreBrokenWeakPointers(const string& uid, WeakMultiStringMap<OType>&v)
	{
	    this->archiveWeakMultiStringMapRaw(uid,v,true,true);
	}


/*!	Archive a Vector of Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename MyType>
    void archiveObjectVector( const string& uid, const string& nodeName, vector<gctools::smart_ptr<MyType> >& v )
    { _G();
//	    DEPRECIATED("Use archiveVector0 instead");
	    ArchiveP	childMatter;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
	        if ( v.size() != 0 ) {
//		    printf( "archiveObjectVector>> Archive start object\n" );
		    typename vector<gctools::smart_ptr<MyType> >::iterator	oi;
		    childMatter = this->newArchiveNode();
		    childMatter->setClasslessNodeName(nodeName);
		    childMatter->setTextUniqueId(uid);
		    this->addChild(childMatter);
                    gctools::smart_ptr<MyType> object;
		    for ( oi=v.begin(); oi!=v.end(); oi++ ) {
//			printf( "archiveObjectVector>> Archive object\n" );
			object = *oi;
			childMatter->archiveObject<MyType>(this->nextUniqueIdCharacters(),object);
		    }
		}
//		printf( "archiveObjectVector>> Done saving\n" );
	    } else
	    { _BLOCK_TRACE("Loading");
//		printf( "archiveObjectVector>>Looking for children with uid(%s)\n", uid.c_str() );
	        if ( this->hasChildWithUniqueId(uid) ) {
//		    printf( "archiveObjectVector>>found the node with uid(%s)\n", uid.c_str() );
		    childMatter = this->childWithUniqueId(uid);
		    childMatter->setRecognized(true);
		    VectorNodes::iterator	ci;
                    gctools::smart_ptr<MyType> object;
		    v.clear();
		    for ( ci=childMatter->begin_Children();
				    ci!=childMatter->end_Children(); ci++ ) {
			object = downcast<MyType>(this->loadObjectDirectly((*ci)));
			ASSERTNOTNULL(object);
			v.push_back(object);
		    }
		} else {
//		    printf( "archiveObjectVector>>could not find the node with name(%s)\n", nodeName.c_str() );
//		    this->dumpChildrenNames();
		    v.clear();
		}
	    }
	}


	bool hasChildWithNodeNameMatchingSubClassOf(Symbol_sp baseId);


		/*!
		 * This will drop the contents of List directly into the node.
		 * When saving, it makes sure that all of the objects are of the specified class.
		 */
	template <class aClass>
	void archiveVector0OfObjectsSubClassOf( Vector0<aClass>& objects )
    { _G();
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
	    		// First make sure that there are no children of
			// this subclass in the node
		if ( this->hasChildWithNodeNameMatchingSubClassOf(aClass::static_classSymbol()) ) {
		    SIMPLE_ERROR(BF("You are trying to put objects of a subclass into a node that already contains them"));
		}
		typename Vector0<aClass>::iterator oi;
		stringstream suid;
		int nextUniqueId = this->numberOfChildren()+1;
		for ( oi=objects.begin(); oi!=objects.end(); oi++ ) {
		    suid.str("");
		    suid << nextUniqueId;
		    this->saveObjectWithinNodeWithUid(suid.str(), *oi);
		    nextUniqueId++;
		}
	    } else
	    { _BLOCK_TRACE("Loading");
		Symbol_sp curId;
		vector<ArchiveP>::iterator ci;
                gctools::smart_ptr<aClass> object;
		objects.clear();
		for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ )
		{
		    if ( lisp_isClassName(_lisp, (*ci)->getNodeName()) )
		    {
		        curId = lisp_getClassSymbolForClassName(_lisp,(*ci)->getNodeName());
			if ( lisp_subClassOrder(_lisp,aClass::static_classSymbol(),curId) )
			{
			    object = downcast<aClass>(this->loadObjectDirectly(*ci));
			    objects.push_back(object);
			    (*ci)->setRecognized(true);
			}
		    }
		}
	    }
	}


	template <class aClass>
	void archiveMapOfObjectsSubClassOf( StringMap<aClass>& objects )
    { _G();
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
	    		// First make sure that there are no children of
			// this subclass in the node
		if ( this->hasChildWithNodeNameMatchingSubClassOf(aClass::static_classSymbol()) ) {
		    SIMPLE_ERROR(BF("You are trying to put objects of a subclass into a node that already contains them"));
		}
		typename StringMap<aClass>::iterator oi;
		stringstream suid;
		int nextUniqueId = this->numberOfChildren()+1;
		for ( oi=objects.begin(); oi!=objects.end(); oi++ ) {
		    suid.str("");
		    suid << nextUniqueId;
		    this->saveObjectWithinNodeWithUidAndKey(suid.str(), oi->first, oi->second);
		    nextUniqueId++;
		}
	    } else
	    { _BLOCK_TRACE("Loading");
		Symbol_sp curId;
		string key;
		vector<ArchiveP>::iterator ci;
                gctools::smart_ptr<aClass> object;
		objects.clear();
		for ( ci=this->begin_Children(); ci!=this->end_Children(); ci++ )
		{
		    if ( lisp_isClassName(_lisp, (*ci)->getNodeName()) )
		    {
		        curId = lisp_getClassSymbolForClassName(_lisp, (*ci)->getNodeName());
			if ( lisp_subClassOrder(_lisp, aClass::static_classSymbol(),curId) )
			{
			    object = downcast<aClass>(this->loadObjectDirectly(*ci));
			    key = (*ci)->getAttributeString("_key");
			    if ( objects.contains(key) )
			    {
				SIMPLE_ERROR("The Map of "+aClass::static_className()+" already contains the key: "+key);
			    }
			    objects.set(key,object);
			    (*ci)->setRecognized(true);
			}
		    }
		}
	    }
	}









/*!	Archive a map of strings->Objects
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename MyType>
    void archiveMapStringObject( const string& uid, const string& nodeName, map<string,gctools::smart_ptr<MyType> >& v ) { _G();
	    DEPRECIATEDP("Use archiveMap instead");
	    ArchiveP	childMatter;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
	        if ( v.size() != 0 ) {
//		    printf( "archiveObjectVector>> Archive start object\n" );
		    typename map<string,gctools::smart_ptr<MyType> >::iterator	oi;
		    childMatter = this->newArchiveNode();
		    childMatter->setClasslessNodeName(nodeName);
		    childMatter->setTextUniqueId(uid);
		    this->addChild(childMatter);
                    gctools::smart_ptr<MyType> object;
		    for ( oi=v.begin(); oi!=v.end(); oi++ ) {
//			printf( "archiveObjectVector>> Archive object\n" );
			object = oi->second;
			childMatter->archiveObject<MyType>(oi->first,object);
		    }
		}
//		printf( "archiveObjectVector>> Done saving\n" );
	    } else
	    { _BLOCK_TRACEF(BF("Loading uid(%s)") %uid.c_str());
//		printf( "archiveObjectVector>>Looking for children with uid(%s)\n", uid.c_str() );
	        if ( this->hasChildWithUniqueId(uid) ) {
//		    printf( "archiveObjectVector>>found the node with uid(%s)\n", uid.c_str() );
		    childMatter = this->childWithUniqueId(uid);
		    VectorNodes::iterator	ci;
                    gctools::smart_ptr<MyType> object;
		    v.clear();
		    for ( ci=childMatter->begin_Children();
				    ci!=childMatter->end_Children(); ci++ ) {
			object = downcast<MyType>(this->loadObjectDirectly((*ci)));
			ASSERTNOTNULL(object);
			v[(*ci)->getUniqueIdCharacters()] = (object);
		    }
		} else {
//		    printf( "archiveObjectVector>>could not find the node with name(%s)\n", nodeName.c_str() );
//		    this->dumpChildrenNames();
		    v.clear();
		}
	    }
	}


#if 0 // {
/*!	Archive a map of strings->pods (plain old datas)
 * 	- Loading
 * 		-# Identify the node with the requested name and archive each of its children.
 * 		-# If there is no node then clear the vector
 *	- Saving
 *		-# Create a node with the requested name and archive each of its contents.
 *		-# If the vector is empty then don't create contents.
 */
    template <typename MyPod>
        void archiveMapStringPod( const string& uid, const string& nodeName, map<string,MyPod >& v ) {
	_G();
	    ArchiveP	childMatter, child;
	    if ( this->saving() )
	    { _BLOCK_TRACE("Saving");
	        if ( v.size() != 0 ) {
//		    printf( "archiveStringPodMap>> Archive start object\n" );
		    typename map<string,MyPod >::iterator	oi;
		    childMatter = this->newArchiveNode();
		    childMatter->setClasslessNodeName(nodeName);
		    childMatter->setUniqueId(uid);
		    this->addChild(childMatter);
		    for ( oi=v.begin(); oi!=v.end(); oi++ ) {
//			printf( "archiveObjectVector>> Archive object\n" );
			child = childMatter->createChildNode("E");
			child->setUniqueId(oi->first);
			child->attribute("val",oi->second );
		    }
		}
//		printf( "archiveObjectVector>> Done saving\n" );
	    } else
	    { _BLOCK_TRACE("Loading");
//		printf( "archiveObjectVector>>Looking for children with uid(%s)\n", uid.c_str() );
	        if ( this->hasChildWithUniqueId(uid) )
		{
//		    printf( "archiveObjectVector>>found the node with uid(%s)\n", uid.c_str() );
		    childMatter = this->childWithUniqueId(uid);
		    VectorNodes::iterator	ci;
		    v.clear();
		    for ( ci=childMatter->begin_Children();
				    ci!=childMatter->end_Children(); ci++ ) {
		        MyPod object;
			(*ci)->attribute("val",object);
			v[(*ci)->getUniqueId()] = object;
		    }
		    childMatter->setRecognized(true);
		} else
		{
//		    printf( "archiveObjectVector>>could not find the node with name(%s)\n", nodeName.c_str() );
//		    this->dumpChildrenNames();
		    v.clear();
		}
	    }
	}
#endif // }
    bool getWord(const char*&cur, char buffer[], uint bufferMax);

			/*! Archive a VectorString to a node with name nodeName
			 */
	void	archiveVectorStrings( const string& nodeName, VectorStrings& vec );
	void	archiveVectorSymbols( const string& nodeName, vector<Symbol_sp>& vec );
	void	archiveVectorDouble( const string& nodeName, vector<double>& vec );
	void	archiveVectorFloat( const string& nodeName, vector<float>& vec );
	void	archiveVectorInt( const string& nodeName, vector<int>& vec );
	void	archiveVectorUInt( const string& nodeName, vector<uint>& vec );
	void	archiveMapSymbolInt( const string& nodeName, map<Symbol_sp,int>& vec );
	void	archiveMapSymbolUnsignedInt( const string& nodeName, map<Symbol_sp,uint>& vec );
	void	archiveMultiMapSymbolInt( const string& nodeName, multimap<Symbol_sp,int>& vec );
	void	archiveMultiMapSymbolUnsignedInts( const string& nodeName, multimap<Symbol_sp,uint>& vec );
	void	archiveString( const string& nodeName, string& str );
	void	archiveStringIfNotDefault( const string& nodeName, string& str, const string& defaultString );
	void	archiveStringStream( const string& nodeName, stringstream& str );
	void	archiveStringStreamIfDefined( const string& nodeName, stringstream& str );
	    

/*!
 * I'm copying the boost serialization library handling of pointers
 * 	- Loading
 * 		-# Read the weak pointer tag from the Node
 * 		-# Identify the Node that corresponds to the weak pointer tag
 * 		-# If the node says the object has already been created
 * 			then return the object address (tag now swizzled)
 * 		-# Otherwise create the object from the node.
 * 		-# Return the address of the object (tag now swizzled).
 *	- Saving
 *		-# Save the node/tag/object in a list for unswizzling during writing.
 *	During the writing to the output, every weak pointer is unswizzled once every
 *	node has been created.
 */
    template <class Dumb_Class>
    void archiveWeakPointerBase( string attribute, gctools::weak_smart_ptr<Dumb_Class>&obj, 
						bool errorIfAttributeMissing, 
						bool suppressNodeForBrokenOrNilWeakPointers,
						bool suppressAttributeForNilWeakPointers)
    { _G();
	    if ( this->loading() )
	    { _BLOCK_TRACEF(BF("Loading attribute(%s)") % attribute.c_str() );
		T_wp rawWeakObj = T_O::_nil;
		this->loadWeakPointerInAttribute( this, attribute,
						rawWeakObj, Dumb_Class::static_classSymbol(), errorIfAttributeMissing);
		ASSERTNOTNULL(rawWeakObj);
		if ( rawWeakObj.lock()->isNil() )
		{
		    LOG(BF("Assigning object nil"));
		    obj = Dumb_Class::_nil;
		} else {
		    LOG(BF("Assigning object value"));
		    obj = downcast<Dumb_Class>(rawWeakObj.lock());
		}
	    }
	    else
	    { _BLOCK_TRACE("Saving");
		this->saveWeakPointerInAttribute( attribute, obj, 
					suppressNodeForBrokenOrNilWeakPointers,
					suppressAttributeForNilWeakPointers );
	    }
	}
    template <class Dumb_Class>
    void archiveWeakPointer( string attribute, gctools::weak_smart_ptr<Dumb_Class>&obj)
    { _G();
	    this->archiveWeakPointerBase<Dumb_Class>(attribute,obj,true,false,false);
	}
    template <class Dumb_Class>
    void archiveWeakPointerIfNotNil( string attribute, gctools::weak_smart_ptr<Dumb_Class>&obj)
    { _G();
	    this->archiveWeakPointerBase<Dumb_Class>(attribute,obj,false,false,true);
	}
    template <class Dumb_Class>
    void archiveWeakPointerSuppressNodeIfBrokenOrNil( string attribute, gctools::weak_smart_ptr<Dumb_Class>&obj )
    { _G();
	    this->archiveWeakPointerBase<Dumb_Class>(attribute,obj,true,true,false);
	}


    template <class Dumb_Class>
    void archiveSaveObjectAsChildAssignAutoUniqueId( gctools::smart_ptr<Dumb_Class>& obj ) {
	    this->saveObjectWithinNodeWithUid( this->nextUniqueIdCharacters(), obj );
	}

    template <class Dumb_Class>
    void archiveLoadObjectDirectly( gctools::smart_ptr<Dumb_Class>& obj )
    { _G();
	    T_sp rawObj;
	    rawObj = this->loadObjectDirectly(this);
	    ASSERT(rawObj.use_count() > 0 );
	    if ( rawObj->isNil() )
	    {
		obj = Dumb_Class::_nil;
	    } else {
		obj = downcast<Dumb_Class>(rawObj);
	    }
	}

	/*! When you need to archive something unusual you can ask the node to create subnodes
	 * and manage them yourself
	 */
    ArchiveP	createChildNode(const string& nodeName);






		//
		// getData same as getCharacters
		//
    const char* characters();
//    string oneLineOfCharacters(const char** cur );
    bool	dataIsAllWhiteSpace();
    int		dataCountNewLines();
    void getDataAsVectorOfStrings(vector<string>& fill);
    void getDataAsVectorOfSymbols(vector<Symbol_sp>& fill);
    void getDataAsVectorOfInts(vector<int>& fill);
    void getDataAsVectorOfUnsignedInts(vector<uint>& fill);
    void getDataAsMapSymbolInts(Lisp_sp lisp, map<Symbol_sp,int>& vs);
    void getDataAsMapSymbolUnsignedInts(Lisp_sp lisp, map<Symbol_sp,uint>& vs);
    void getDataAsMultiMapSymbolUnsignedInts(Lisp_sp lisp, multimap<Symbol_sp,uint>& vs);
    void getDataAsVectorOfDoubles(vector<double>& fill);
//    StringSet_sp	getDataAsStringSet();


    void setCharacters(const string& chars);

    void eraseAll();

    void parseFile( FILE* fIn, const string fn );
    void parseFileName(const string fn);
    void open(const string fn) { this->parseFileName(fn);};


		/*! Lock the file and parse the fileName
			if lock fails return false otherwise true
		*/
    bool lockAndParseFileName(const string fn);
		/*! Lock the file and write to the fileName
			if lock fails return false otherwise true
		*/
    bool lockAndWriteToFileName( string fileName );

    VectorNodes	gatherSubNodesWithName(const string& name);
};




}; // namespace core



DEFINE_RETURN_VALUE_TYPE(core::Dumb_Node*);



#endif //]
