#ifndef _core_sourceFileInfo_H_
#define _core_sourceFileInfo_H_

#include "boost/filesystem.hpp"
#include "core/foundation.h"
#include "core/object.h"
#include "core/pathname.fwd.h"
#include "core/fileSystem.fwd.h"
#include "core/sourceFileInfo.fwd.h"
    
namespace core
{
    class SourceFileInfo_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,SourceFileInfo_O,"SourceFileInfo");
	DECLARE_INIT();
    public:
	static SourceFileInfo_sp create(const string& fileNamePath);
	static SourceFileInfo_sp create(Pathname_sp path);

    public: // ctor/dtor for classes with shared virtual base
	explicit SourceFileInfo_O();
	virtual ~SourceFileInfo_O() {};
	void initialize();
    private: // instance variables here
	Pathname_sp	_pathname;
	/*! Allocated buffer that stores the file name until the program exits */
	char* 	_PermanentPathName;
	char*	_PermanentFileName;
    public: // Functions here
	string fileName() const;
	string parentPathName() const;
	string namestring() const;
	Pathname_sp pathname() const { return this->_pathname;};
	const char* permanentPathName();
	const char* permanentFileName();

	string __repr__() const;
    }; // SourceFileInfo class





    FORWARD(SourcePosInfo);
    class SourcePosInfo_O : public T_O
    {
        friend class SourceManager_O;
        friend SourceFileInfo_mv af_sourceFileInfo(T_sp obj);

	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,SourcePosInfo_O,"SourcePosInfo");
    public:
    public: // ctor/dtor for classes with shared virtual base
	explicit SourcePosInfo_O() : _FileId(UNDEF_UINT), _LineNumber(0),_Column(0) {}; //, _FilePos(0) {};
    public: // instance variables here
	SourcePosInfo_O(uint spf, uint spln, uint spc) // , Function_sp expander=_Nil<Function_O>())
	    : _FileId(spf), _LineNumber(spln), _Column(spc) //, _Expander(expander) {}
        {};

    public:

        static SourcePosInfo_sp create(uint spf, uint spln=0, uint spc=0 )
        {
#if 0
            if ( filePos==UNDEF_UINT ) {
                printf("%s:%d Caught filePos=UNDEF_UINT\n", __FILE__, __LINE__ );
            }
#endif
            GC_ALLOCATE_VARIADIC(SourcePosInfo_O,me,spf,spln,spc); // ,filePos,fn);
            return me;
        }

        SourceFileInfo_sp sourceFileInfo(SourceManager_sp sm) const;
            
        uint lineNumber() const { return this->_LineNumber; };
        int column() const { return this->_Column; };
    public:
	uint	_FileId;
	uint	_LineNumber;
	uint	_Column;
//	uint	_FilePos;
//	Function_sp 	_Expander;
    };
};
template<> struct gctools::GCInfo<core::SourcePosInfo_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};


namespace core {
    class SourceManager_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,SourceManager_O,"SourceManager");
	DECLARE_INIT();
        void initialize();
    public: // ctor/dtor for classes with shared virtual base
	explicit SourceManager_O() {};
	virtual ~SourceManager_O() {};
    public: // instance variables here
        // TODO!!!!! Use a WeakKeyHashTable_sp here
        // Must be implemented first!!!!
        HashTableEq_sp                         _SourcePosInfo;
	/*! All SourceFileInfo_sp source files are stored here indexed by integer FileId */
//        gctools::Vec0<SourceFileInfo_sp>		_Files;
    public: // Functions here
        /*! Return true if the SourceManager is available */
        bool availablep() const { return this->_SourcePosInfo.notnilp(); };

	/*! Register the object with the source manager */
	SourcePosInfo_sp registerSourceInfo(T_sp obj, T_sp sourceFile, uint lineno, uint column);

//        SourceFileInfo_sp sourceFileInfoFromIndex(int idx) const;

	SourcePosInfo_sp registerSourceInfoFromStream(T_sp obj, Stream_sp stream);

	bool searchForSourceInfoAndDuplicateIt(T_sp orig, T_sp newObj);

	/*! Duplicate the source code information associated with orig_obj for new_obj */
	void duplicateSourceInfo(T_sp orig_obj, T_sp new_obj);

	/*! Duplicate the source code information for a macro expansion associated 
	  from orig_obj for new_obj */
	void duplicateSourceInfoForMacroExpansion(T_sp orig_obj, Function_sp expansionFunction, T_sp new_obj);

	/*! Return (values SourceFileInfo_sp lineno column sourcePos macroObject? ) for obj
          or (values) if nothing is found */
	SourceFileInfo_mv lookupSourceInfo(T_sp obj);

	SourcePosInfo_sp lookupSourcePosInfo(T_sp obj);

    }; // SourceManager class


    SourceFileInfo_mv af_walkToFindSourceInfo(T_sp obj);
//    SourceFileInfo_mv af_lookupSourceFileInfo(T_sp obj);



}; // core namespace
TRANSLATE(core::SourceFileInfo_O);
TRANSLATE(core::SourcePosInfo_O);
TRANSLATE(core::SourceManager_O);
    
extern "C" {
    void dumpSourceInfo(core::T_sp exp);
};



    

#endif /* _core_sourceFileInfo_H_ */
