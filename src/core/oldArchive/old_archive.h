       
       

#ifndef	ARCHIVE_H //[
#define	ARCHIVE_H

#include "foundation.h"
#include "object.h"
#include "holder.h"
#include "fileSystem.fwd.h"
#include "lispStream.fwd.h"
#include "multiStringBuffer.h"
#include "archiveMemoryManager.h"


#define	USE_REFERENCE_HANDLES 1

namespace core {



extern	string	ObjectPathTag;

class	Dumb_Node;
typedef	Dumb_Node* ArchiveP;

class	SymbolManager
{
public:
private:
	map<string,handleType>	_SymbolsToHandles;
	vector<string>		_Symbols;
public:
	typedef vector<string>::iterator iterator;
	handleType	handle(const string& symbol);
	void handleSetExplicit(const string& symbol, handleType h);
	handleType	handleFromNumber(uint symbol);
	bool	isNumericSymbol(const handleType handle) const;
	uint	numericSymbol(const handleType handle) const;
	const string&	textSymbol(const handleType handle) const;
	string	anySymbol(const handleType handle) const;
	void	printDescription(const Lisp_sp& lisp);
	uint	numberOfSymbols() { return this->_Symbols.size(); };
	iterator begin() { return this->_Symbols.begin();};
	iterator end() { return this->_Symbols.end();};
	SymbolManager();
};


struct	SaveObjectNodeEntry {
    T_sp	_Object;
    ArchiveP	_Node;
};

struct	SaveWeakPointerEntry {
	ArchiveP		_Node;
	string		_Attribute;
	T_sp	_Object;
        bool		_SuppressNodeForBrokenOrNilWeakPointers;
    	bool		_SuppressAttributeForNilWeakPointers;
};


/*! Store key value pairs for serializing enumerated values
 */


/*! Virtual class
 */
SMART(Archive);
class Archive_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(CorePkg,Archive_O,"Archive");
public:
	void initialize();
public:
    static const string UniqueIdTag; //  = "_uid";
    static const string CoreBuiltInClassAttribute; //  = "_coreClass";
protected:
	int		_Version;
	string		_FileName;
	ArchiveP	_TopNode;
	ArchiveP	_CurrentNode;
	int		_NextUniqueId;
	bool		_DebugEnabled;
	bool		_HasError;
	stringstream	_ErrorStream;
	SymbolManager*   _SymbolManagerPtr;
	MultiStringBuffer_sp	_MultiStringBuffer;
	uint		_Verbosity;
	uint		_NextReferenceHandle;
protected:
	map<T_O*,ArchiveP>		_ObjectNodes;
	vector<SaveWeakPointerEntry>	_WeakObjectReferences;
	int				_BrokenWeakPointers;
		/*! Memory manager for ArchiveNodes and ChildHolders
		 */
	ArchiveMemoryManager_sp _ArchiveMemoryManager;
protected:
	void	eraseAll() { this->_TopNode = NULL; this->_CurrentNode=NULL; };
    void	_saveObjectWithinNodeWithUidAndPossiblyKey( ArchiveP node, handleType huid , const string& key, bool useKey, T_sp obj );
public:

	MultiStringBuffer_sp multiStringBuffer() { return this->_MultiStringBuffer; };
	MultiStringBuffer_sp multiStringBuffer() const { return this->_MultiStringBuffer; };

	/*! Advance the value of the reference handle and return it */
	uint nextReferenceHandle();
	void	setVerbosity(uint v) { this->_Verbosity = v;};

	bool	debugEnabled() { return this->_DebugEnabled; };
	void	enableDebug() { this->_DebugEnabled = true; };
	void	disableDebug() { this->_DebugEnabled = false; };
	string getFileName() { return this->_FileName; };
	void setFileName(string fn) { this->_FileName = fn; };
	int	getVersion() { return this->_Version;};
	void	setVersion(int i) { this->_Version = i; };

	void	describeMemoryUsage();


    uint nextUniqueId() { return (this->_NextUniqueId++);};

    bool	hasError() { return this->_HasError; };
    void	addError(const string& errorMessage, Dumb_Node* node);
    string	getErrors() { return this->_ErrorStream.str(); };
    SymbolManager& symbolManager() { ASSERT(this->_SymbolManagerPtr); return *this->_SymbolManagerPtr;};
    const SymbolManager& symbolManager() const { ASSERT(this->_SymbolManagerPtr); return *this->_SymbolManagerPtr;};

    virtual bool isSaveArchive() { return false; };
    bool loading() { return !this->isSaveArchive(); };

    virtual void addNodeToFinalize(Dumb_Node* node) {};

    bool	hadObjectWithinNodeWithUid( ArchiveP node, const string& uid );
    T_sp	loadObjectDirectly( ArchiveP node );
    void	loadObjectWithinNodeWithUid( ArchiveP node, const string& uid, T_sp& obj );
    void	loadObjectWithinNodeWithUid( ArchiveP node, handleType huid, T_sp& obj );
    void	loadOnlyObjectOfClassWithinNode( ArchiveP node, Symbol_sp classSYMBOL, T_sp& obj );
    void 	loadWeakPointerInAttribute( ArchiveP node, const string& attribute, T_wp& obj, Symbol_sp expectedClassSymbol, bool mustBeDefined );

    void	saveObjectWithinNodeWithUid( ArchiveP node, handleType huid, T_sp obj );
    void	saveObjectWithinNodeWithUidAndKey( ArchiveP node, handleType huid, const string& key,T_sp obj );
    void	saveObjectWithinNodeWithUid( ArchiveP node, const string& uid, T_sp obj );
    void	saveObjectWithinNodeWithUidAndKey( ArchiveP node, const string& uid, const string& key,T_sp obj );
    void	saveOnlyObjectOfClassWithinNode( ArchiveP node, Symbol_sp classSymbol, T_sp obj );
    void	saveWeakPointerInAttribute( ArchiveP node , const string& attribute, T_wp obj, 
    							bool suppressNodeForBrokenOrNilWeakPointers,
							bool suppressAttributeForNilWeakPointers );


    ArchiveP	getTopNode();

		/*!Return a pointer to an ArchiveNode */
    ArchiveP  newArchiveNode()
    {_OF();
	ASSERTP(this->_ArchiveMemoryManager,"ArchiveMemoryManager is Undefined and we are being asked to create an ArchiveNode");
	return this->_ArchiveMemoryManager->newArchiveNode();
    }


    		/*! Return a pointer to a ChildHolder */
    ChildHolder* newChildHolder()
    {_OF();
	ASSERTP(this->_ArchiveMemoryManager,"ArchiveMemoryManager is UNDEFINED and we are being asked to create an ChildHolder");
	return this->_ArchiveMemoryManager->newChildHolder();
    }
    explicit Archive_O();
    virtual ~Archive_O();
};






SMART(LoadArchive);
class LoadArchive_O : public Archive_O
{
    LISP_BASE1(Archive_O);
    LISP_CLASS(CorePkg,LoadArchive_O,"LoadArchive");
public:
	void initialize();
public:
    void addNodeToFinalize(Dumb_Node* node);

private:
	vector<Dumb_Node*>	_NodesToFinalize;

protected:

    virtual void createContents();
    virtual void finalizeObjects();


public:
    virtual void parseFromObject( Cons_sp object ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual void parseFromStream( T_sp streamDesignator ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};

    virtual bool contains(const string& uid);
    virtual T_sp get(const string& uid);
    virtual T_sp getContents();

    DEFAULT_CTOR_DTOR(LoadArchive_O);
};





SMART(SaveArchive);
class SaveArchive_O : public Archive_O
{
    LISP_BASE1(Archive_O);
    LISP_CLASS(CorePkg,SaveArchive_O,"SaveArchive");
public:
	void	initialize();
public:

		/*
		 *	- Saving
		 *		-# Save the node/tag/object in a list for unswizzling during writing.
		 *	During the writing to the output, every weak pointer is unswizzled once every
		 *	node has been created.
		 */

    void completeWeakObjectReferences();

public:
    bool	isSaveArchive() { return true; };
    virtual void put(const string& uid, T_sp obj);


    virtual	void	write(Stream_sp outStream) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual	void	saveAs(const string& fileName) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual	void	saveToSteeam(ostream& sout ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual	void	saveAsToPath(Path_sp path ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual	string	asString() {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual	string	asStringSubstituteRootNodeNameAndAttributes(const string& nodeName, const string& rawAttributes) {_OF(); SUBCLASS_MUST_IMPLEMENT();};


    DEFAULT_CTOR_DTOR(SaveArchive_O);
};













};
TRANSLATE(core::Archive_O);
TRANSLATE(core::LoadArchive_O);
TRANSLATE(core::SaveArchive_O);
#endif //]
