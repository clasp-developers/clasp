#define DEBUG_LEVEL_FULL

#include <string.h>
#include "core/foundation.h"
#include "core/fileSystem.h"
#include "core/str.h"
#include "core/lispStream.h"
#include "core/hashTableEq.h"
#include "core/pathname.h"
#include "sourceFileInfo.h"
#include "bundle.h"
#include "wrappers.h"

#define REQUIRE_SOURCE_INFO 0
extern "C" {
    // For debugging the sourceManager
    void dumpSourceInfo(core::T_sp exp) 
    {
	string repExp = _rep_(exp);
	printf("Object: %s\n", repExp.c_str() );
        if ( _lisp->sourceDatabase().notnilp() ) {
            core::SourceFileInfo_mv sfi = _lisp->sourceDatabase()->lookupSourceInfo(exp);
            if ( sfi.number_of_values() >= 4 ) {
                core::SourceFileInfo_sp sf = sfi;
                core::Fixnum_sp lineno  = sfi.valueGet(1).as<core::Fixnum_O>();
                core::Fixnum_sp column  = sfi.valueGet(2).as<core::Fixnum_O>();
                core::Fixnum_sp filePos = sfi.valueGet(3).as<core::Fixnum_O>();
                printf("     Source file: %s   lineno: %d  column: %d   filePos %d\n",
                       _rep_(sf).c_str(), lineno->get(), column->get(), filePos->get());
            } else {
                printf("     No source file info found\n");
            }
            if ( core::af_consP(exp) ) {
                dumpSourceInfo(oCar(exp));
                dumpSourceInfo(oCdr(exp));
            }
        }
    }

    void dumpSourceInfoCons(core::Cons_sp exp) 
    {
        dumpSourceInfo(oCar(exp));
        dumpSourceInfo(oCdr(exp));
    }
};




namespace core
{


#if 0    
#define ARGS_af_sourceFileInfo "(arg)"
#define DECL_af_sourceFileInfo ""
#define DOCS_af_sourceFileInfo "sourceFileInfo"
    SourceFileInfo_ af_sourceFileInfo(T_sp obj)
    {_G();
	SourceFileInfo_sp result = _Nil<SourceFileInfo_O>();
	if ( obj.nilp() ) {
	    // do nothing
	} else if ( Cons_sp co = obj.asOrNull<Cons_O>() )
	{
            if ( _lisp->sourceDatabase().notnilp() ) {
                return _lisp->sourceDatabase()->lookupSourceInfo(co);
            }
            return _Nil<SourceFileInfo_O>();
	} else if ( Stream_sp so = obj.asOrNull<Stream_O>() )
	{
	    result = so->sourceFileInfo();
	} else if ( Function_sp fo = obj.asOrNull<Function_O>() )
	{
	    result = af_sourceFileInfo(fo->closure->sourcePosInfo());
        } else if ( SourcePosInfo_sp spi = obj.asOrNull<SourcePosInfo_O>() ) {
            result = _lisp->sourceDatabase()->sourceFileInfoFromIndex(spi->_FileId);
	} else
	{
	    SIMPLE_ERROR(BF("Implement sourceFileInfo for %s") % _rep_(obj));
	}
	if ( result.nilp() )
	{
	    return SourceFileInfo_O::getOrCreate("Anonymous");
	}
	return result;
    };
#endif

    
#define ARGS_af_lineNumber "(arg)"
#define DECL_af_lineNumber ""
#define DOCS_af_lineNumber "lineNumber"
    uint af_lineNumber(T_sp obj)
    {_G();
	if ( obj.nilp() )
	{
	    return 0;
	} else if ( Cons_sp co = obj.asOrNull<Cons_O>() )
	{
            IMPLEMENT_MEF(BF("Handle cons for af_lineNumber"));
	} else if ( Stream_sp so = obj.asOrNull<Stream_O>() )
	{
	    return clasp_input_lineno(so);
	} else if ( Function_sp fo = obj.asOrNull<Function_O>() )
	{
            return af_lineNumber(fo->closure->sourcePosInfo());
        } else if ( SourcePosInfo_sp info = obj.asOrNull<SourcePosInfo_O>() ) {
            return info->_LineNumber;
	}
	SIMPLE_ERROR(BF("Implement lineNumber for %s") % _rep_(obj));
    };


#define ARGS_af_column "(arg)"
#define DECL_af_column ""
#define DOCS_af_column "column"
    uint af_column(T_sp obj)
    {_G();
	if ( obj.nilp() )
	{
	    return 0;
	} else if ( Cons_sp co = obj.asOrNull<Cons_O>() )
	{
            IMPLEMENT_MEF(BF("Handle cons for af_column"));
	} else if ( Stream_sp so = obj.asOrNull<Stream_O>() )
	{
	    return clasp_input_column(so);
	} else if ( Function_sp fo = obj.asOrNull<Function_O>() )
	{
            return af_column(fo->closure->sourcePosInfo());
        } else if ( SourcePosInfo_sp info = obj.asOrNull<SourcePosInfo_O>() ) {
            return info->_Column;
	}
	SIMPLE_ERROR(BF("Implement column for %s") % _rep_(obj));
    };

    



    
    
#define ARGS_af_walkToFindSourceInfo "(arg)"
#define DECL_af_walkToFindSourceInfo ""
#define DOCS_af_walkToFindSourceInfo "walkToFindSourceInfo"
    SourceFileInfo_mv af_walkToFindSourceInfo(T_sp obj)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            if ( af_consP(obj) ) {
                SourceFileInfo_mv sfi = _lisp->sourceDatabase()->lookupSourceInfo(obj);
                if ( sfi.notnilp() ) {
                    return sfi;
                }
                T_sp cur = obj;
                for ( ; cur.notnilp(); cur=oCdr(cur) ) {
                    if ( af_consP(cur) ) {
                        SourceFileInfo_mv sfisub = af_walkToFindSourceInfo(oCar(cur));
                        if ( sfisub.notnilp() ) return sfisub;
                    } else {
                        return Values(_Nil<SourceFileInfo_O>());
                    }
                }
            }
        }
	return Values(_Nil<SourceFileInfo_O>());
    };



    
#if 0
#define ARGS_af_SourceFileInfoGetOrCreate "(arg)"
#define DECL_af_SourceFileInfoGetOrCreate ""
#define DOCS_af_SourceFileInfoGetOrCreate "SourceFileInfoGetOrCreate"
    T_sp af_SourceFileInfoGetOrCreate(T_sp arg)
    {_G();
	if ( Str_sp sarg = arg.asOrNull<Str_O>() )
	{
	    return SourceFileInfo_O::getOrCreate(sarg->get());
	} else if ( Pathname_sp parg = arg.asOrNull<Pathname_O>() )
	{
	    return SourceFileInfo_O::getOrCreate(parg);
	}
	SIMPLE_ERROR(BF("Illegal argument for source-file-info-get-or-create"));
    }
#endif


    SourceFileInfo_O::SourceFileInfo_O() : Base(), _PermanentPathName(NULL), _PermanentFileName(NULL) {};

    void SourceFileInfo_O::initialize()
    {_G();
	this->Base::initialize();
    }

    SourceFileInfo_sp SourceFileInfo_O::create(const string& str, int handle)
    {_G();
        GC_ALLOCATE(SourceFileInfo_O,spi );
	spi->_pathname = cl_pathname(Str_O::create(str));
        spi->_FileHandle = handle;
	return spi;
    }


    SourceFileInfo_sp SourceFileInfo_O::create(Pathname_sp path, int handle )
    {_G();
	Str_sp s = af_namestring(path);
        return SourceFileInfo_O::create(s->get(),handle);
    }


    string SourceFileInfo_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " " << _rep_(this->_pathname) << ">";
	return ss.str();
    }



    string SourceFileInfo_O::fileName() const
    {
	Str_sp s = af_fileNamestring(this->_pathname);
	return s->get();
    }


    string SourceFileInfo_O::namestring() const
    {
	Str_sp s = af_namestring(this->_pathname);
	return s->get();
    }


    string SourceFileInfo_O::parentPathName() const
    {
	Str_sp s = af_directoryNamestring(this->_pathname);
	return s->get();
    }


    const char* SourceFileInfo_O::permanentPathName()
    {
	if ( this->_PermanentPathName == NULL )
	{
	    string fn = this->namestring();
	    this->_PermanentPathName = (char*)malloc(fn.size()+1);
	    ::strcpy(this->_PermanentPathName,fn.c_str());
	}
	return this->_PermanentPathName;
    }


    const char* SourceFileInfo_O::permanentFileName()
    {
	if ( this->_PermanentFileName == NULL )
	{
	    string fn = this->fileName();
	    this->_PermanentFileName = (char*)malloc(fn.size()+1);
	    ::strcpy(this->_PermanentFileName,fn.c_str());
	}
	return this->_PermanentFileName;
    }


    EXPOSE_CLASS(core,SourceFileInfo_O);

    void SourceFileInfo_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<SourceFileInfo_O>()
	    .def("source-file-info-pathname",&SourceFileInfo_O::pathname)
	    ;
//	SYMBOL_SC_(CorePkg,SourceFileInfoGetOrCreate);
//	Defun(SourceFileInfoGetOrCreate);
	SYMBOL_EXPORT_SC_(CorePkg,walkToFindSourceInfo);
	Defun(walkToFindSourceInfo);
    }

    void SourceFileInfo_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SourceFileInfo,"","",_lisp)
	    ;
#endif
    }







    EXPOSE_CLASS(core,SourcePosInfo_O);

    void SourcePosInfo_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<SourcePosInfo_O>()
	    ;
    }


    void SourcePosInfo_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SourcePosInfo,"","",_lisp)
	    ;
#endif
    }






    
#define ARGS_af_lookupSourceFileInfo "(obj)"
#define DECL_af_lookupSourceFileInfo ""
#define DOCS_af_lookupSourceFileInfo "lookupSourceFileInfo"
    SourceFileInfo_mv af_lookupSourceFileInfo(T_sp obj)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            return _lisp->sourceDatabase()->lookupSourceInfo(obj);
        }
        return Values(_Nil<SourceFileInfo_O>());
    };



    
    
#define ARGS_af_dumpSourceManager "()"
#define DECL_af_dumpSourceManager ""
#define DOCS_af_dumpSourceManager "dumpSourceManager"
    void af_dumpSourceManager()
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            _lisp->print(BF("Source Manager entries: %d\n") % _lisp->sourceDatabase()->_SourcePosInfo->size());
#if 0
            for ( int i=0, iEnd(_lisp->sourceDatabase()->_Files.size()); i<iEnd; ++i ) {
                _lisp->print(BF("   File[%d] --> %s\n") % i % _lisp->sourceDatabase()->_Files[i]->namestring());
            }
#endif
        } else {
            _lisp->print(BF("No source manager available"));
        }
    };


    EXPOSE_CLASS(core,SourceManager_O);


    
    
#define ARGS_af_makeSourceManager "()"
#define DECL_af_makeSourceManager ""
#define DOCS_af_makeSourceManager "makeSourceManager"
    SourceManager_sp af_makeSourceManager()
    {_G();
        SourceManager_sp sm = SourceManager_O::create();
        return sm;
    };

    void SourceManager_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<SourceManager_O>()
	    ;

	SYMBOL_EXPORT_SC_(CorePkg,lookupSourceFileInfo);
	Defun(lookupSourceFileInfo);
	Defun(dumpSourceManager);
        Defun(makeSourceManager);
    }


    void SourceManager_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SourceManager,"","",_lisp)
	    ;
#endif
    }


    void SourceManager_O::initialize()
    {
        this->Base::initialize();
        this->_SourcePosInfo = HashTableEq_O::create_default();
        
    }


    SYMBOL_EXPORT_SC_(CorePkg,STARmonitorRegisterSourceInfoSTAR);
    SourcePosInfo_sp SourceManager_O::registerSourceInfo(T_sp key,
                                                         T_sp sourceFile,
                                                         uint lineno,
                                                         uint column )
    {_G();
        if ( _sym_STARmonitorRegisterSourceInfoSTAR->symbolValue().notnilp() ) {
            printf("%s:%d  registerSourceInfo  sourceFile: %s:%d:%d  --> %s\n", __FILE__, __LINE__, sourceFile->__repr__().c_str(), lineno, column, _rep_(key).c_str() );
            printf("%s:%d        *source-database* =\n",__FILE__, __LINE__);
            af_dumpSourceManager();
        }
        
        if ( this->availablep() ) {
            SourceFileInfo_mv sfi = af_sourceFileInfo(sourceFile);
            Fixnum_sp fileId = sfi.valueGet(1).as<Fixnum_O>();
            SourcePosInfo_sp info = SourcePosInfo_O::create(fileId->get(),lineno,column);
            this->_SourcePosInfo->setf_gethash(key,info);
            return info;
        }
        return _Nil<SourcePosInfo_O>();
    }


#if 0
    SourceFileInfo_sp SourceManager_O::sourceFileInfoFromIndex(int idx) const {
        return this->_Files[idx];
    }
#endif
    SourcePosInfo_sp SourceManager_O::registerSourceInfoFromStream(T_sp obj, Stream_sp stream)
    {_G();
	SourceFileInfo_sp sfi  = clasp_input_source_file_info(stream);
	uint lineNumber = clasp_input_lineno(stream);
	uint column = clasp_input_column(stream);
	return this->registerSourceInfo(obj,sfi,lineNumber,column);
    }

    bool SourceManager_O::searchForSourceInfoAndDuplicateIt(T_sp orig_obj, T_sp new_obj)
    {
        if ( _lisp->sourceDatabase().notnilp() ) {
            SourceFileInfo_mv info = _lisp->sourceDatabase()->lookupSourceInfo(orig_obj);
            if ( info.notnilp() ) {
                if ( info.number_of_values() >= 4) {
                    SourceFileInfo_sp sfi = info;
                    uint lineno = info.valueGet(1).as<Fixnum_O>()->get();
                    uint column = info.valueGet(2).as<Fixnum_O>()->get();
                    this->registerSourceInfo(new_obj,sfi,lineno,column);
                    return true;
                } else {
                    if ( af_consP(orig_obj) ) {
                        T_sp orig_cons = orig_obj;
                        while (orig_cons.notnilp() && af_consP(orig_cons)) {
                            orig_cons = oCdr(orig_cons);
                            if ( af_consP(orig_cons) ) {
                                if ( this->searchForSourceInfoAndDuplicateIt(oCar(orig_cons),new_obj) ) return true;
                            }
                        }
                    }
                }
            }
        }
	return false;
    }
	


    void SourceManager_O::duplicateSourceInfo(T_sp orig_obj, T_sp new_obj)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            SourceFileInfo_mv info = _lisp->sourceDatabase()->lookupSourceInfo(orig_obj);
            if (info.notnilp() ) {
                if ( info.number_of_values() >= 2) {
                    SourceFileInfo_sp sfi = info;
                    uint lineno = info.valueGet(1).as<Fixnum_O>()->get();
                    uint column = info.valueGet(2).as<Fixnum_O>()->get();
                    this->registerSourceInfo(new_obj,sfi,lineno,column);
                } else {
                    this->searchForSourceInfoAndDuplicateIt(orig_obj, new_obj);
#if 0
#if REQUIRE_SOURCE_INFO
                    printf("Dumping source info for orig_obj\n");
                    dumpSourceInfo(orig_obj);
                    IMPLEMENT_MEF(BF("There was no source info to duplicate"));
#endif
#endif
                }
            }
        }
    }

    void SourceManager_O::duplicateSourceInfoForMacroExpansion(T_sp orig_obj, Function_sp expander, T_sp new_obj)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            SourceFileInfo_mv info = _lisp->sourceDatabase()->lookupSourceInfo(orig_obj);
            if ( info.notnilp() ) {
                if ( info.number_of_values() >= 4) {
                    SourceFileInfo_sp sfi = info;
                    uint lineno = info.valueGet(1).as<Fixnum_O>()->get();
                    uint column = info.valueGet(2).as<Fixnum_O>()->get();
                    this->registerSourceInfo(new_obj,sfi,lineno,column);
                } else {
                    this->searchForSourceInfoAndDuplicateIt(orig_obj, new_obj);
#if 0
#if REQUIRE_SOURCE_INFO
                    printf("Dumping source info for orig_obj\n");
                    dumpSourceInfo(orig_obj);
                    IMPLEMENT_MEF(BF("There was no source info to duplicate"));
#endif
#endif
                }
            }
        }
    }


    SourceFileInfo_mv SourceManager_O::lookupSourceInfo(T_sp key)
    {
        if ( this->availablep() ) {
            SourcePosInfo_sp it = this->_SourcePosInfo->gethash(key,_Nil<SourcePosInfo_O>()).as<SourcePosInfo_O>();
            if (it.notnilp()) {
                SourceFileInfo_sp sfi = af_sourceFileInfo(Fixnum_O::create(it->_FileId));
                return Values(sfi,Fixnum_O::create(it->_LineNumber),
                              Fixnum_O::create(it->_Column));
            }
        }
        return Values(_Nil<SourceFileInfo_O>());
    }

    SourcePosInfo_sp SourceManager_O::lookupSourcePosInfo(T_sp key)
    {
        if ( this->availablep() ) {
            SourcePosInfo_sp it = this->_SourcePosInfo->gethash(key,_Nil<SourcePosInfo_O>()).as<SourcePosInfo_O>();
            return it;
        }
        return _Nil<SourcePosInfo_O>();
    }


};
