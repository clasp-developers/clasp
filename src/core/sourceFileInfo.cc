/*
    File: sourceFileInfo.cc
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

#include <string.h>
#include "core/foundation.h"
#include "core/fileSystem.h"
#include "core/str.h"
#include "core/lispStream.h"
#include "core/hashTableEq.h"
#include "core/pathname.h"
#include "core/print.h"
#include "lispStream.h"
#include "sourceFileInfo.h"
#include "bundle.h"
#include "write_ugly.h"
#include "wrappers.h"

#define REQUIRE_SOURCE_INFO 0
extern "C" {
    // For debugging the sourceManager
    void dumpSourceInfo(core::T_sp exp) 
    {
	string repExp = _rep_(exp);
	printf("Object: %s\n", repExp.c_str() );
        if ( _lisp->sourceDatabase().notnilp() ) {
            core::SourceFileInfo_mv sfi = _lisp->sourceDatabase()->multipleValuesSourceInfo(exp);
            if ( sfi.number_of_values() >= 4 ) {
                core::SourceFileInfo_sp sf = sfi;
                core::Fixnum_sp filepos = sfi.valueGet(1).as<core::Fixnum_O>();
                core::Fixnum_sp lineno  = sfi.valueGet(2).as<core::Fixnum_O>();
                core::Fixnum_sp column  = sfi.valueGet(3).as<core::Fixnum_O>();
                printf("     Source file: %s   lineno: %d  column: %d   filepos %d\n",
                       _rep_(sf).c_str(), lineno->get(), column->get(), filepos->get());
            } else {
                printf("     No source file info found\n");
            }
            if ( core::cl_consp(exp) ) {
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

    
    uint clasp_sourcePosInfo_fileHandle(SourcePosInfo_sp info)
    {
	return info->_FileId;
    }

    
    size_t clasp_sourcePosInfo_filepos(SourcePosInfo_sp info)
    {
	return info->_Filepos;
    }

    
#define ARGS_core_sourcePosInfoFilepos "(source-pos-info)"
#define DECL_core_sourcePosInfoFilepos ""
#define DOCS_core_sourcePosInfoFilepos "sourcePosInfoFilepos"
    Integer_sp core_sourcePosInfoFilepos(SourcePosInfo_sp info)
    {
	return Integer_O::create((size_t)clasp_sourcePosInfo_filepos(info));
    }

    
    
    uint clasp_sourcePosInfo_lineno(SourcePosInfo_sp info)
    {
	return info->_Lineno;
    }

#define ARGS_core_sourcePosInfoLineno "(source-pos-info)"
#define DECL_core_sourcePosInfoLineno ""
#define DOCS_core_sourcePosInfoLineno "sourcePosInfoLineno"
    Fixnum_sp core_sourcePosInfoLineno(SourcePosInfo_sp info)
    {
	return Fixnum_O::create(clasp_sourcePosInfo_lineno(info));
    }

    uint clasp_sourcePosInfo_column(SourcePosInfo_sp info)
    {
	return info->_Column;
    }

#define ARGS_core_sourcePosInfoColumn "(source-pos-info)"
#define DECL_core_sourcePosInfoColumn ""
#define DOCS_core_sourcePosInfoColumn "sourcePosInfoColumn"
    Fixnum_sp core_sourcePosInfoColumn(SourcePosInfo_sp info)
    {
	return Fixnum_O::create(clasp_sourcePosInfo_column(info));
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
	} else if ( Cons_sp co = obj.asOrNull<Cons_O>() ) {
            if ( _lisp->sourceDatabase().notnilp() ) {
                return _lisp->sourceDatabase()->lookupSourceInfo(co);
            }
            return _Nil<SourceFileInfo_O>();
	} else if ( cl_streamp(obj) ) {
	    T_sp so = obj;
	    result = so->sourceFileInfo();
	} else if ( Function_sp fo = obj.asOrNull<Function_O>() ) {
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

    
#define ARGS_af_lineno "(arg)"
#define DECL_af_lineno ""
#define DOCS_af_lineno "lineNumber"
    uint af_lineno(T_sp obj)
    {_G();
	if ( obj.nilp() ) {
	    return 0;
	} else if ( Cons_sp co = obj.asOrNull<Cons_O>() ) {
            IMPLEMENT_MEF(BF("Handle cons for af_lineno"));
	} else if ( cl_streamp(obj) ) {
	    return clasp_input_lineno(obj);
	} else if ( Function_sp fo = obj.asOrNull<Function_O>() )
	{
            return af_lineno(fo->closure->sourcePosInfo());
        } else if ( SourcePosInfo_sp info = obj.asOrNull<SourcePosInfo_O>() ) {
            return info->_Lineno;
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
	} else if ( cl_streamp(obj) ) {
	    return clasp_input_column(obj);
	} else if ( Function_sp fo = obj.asOrNull<Function_O>() )
	{
            return af_column(fo->closure->sourcePosInfo());
        } else if ( SourcePosInfo_sp info = obj.asOrNull<SourcePosInfo_O>() ) {
            return info->_Column;
	}
	SIMPLE_ERROR(BF("Implement column for %s") % _rep_(obj));
    };

    

#define ARGS_core_walkToFindSourceInfo "(arg)"
#define DECL_core_walkToFindSourceInfo ""
#define DOCS_core_walkToFindSourceInfo "walkToFindSourceInfo"
    SourceFileInfo_mv core_walkToFindSourceInfo(T_sp obj)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            if ( cl_consp(obj) ) {
                SourceFileInfo_mv sfi = _lisp->sourceDatabase()->multipleValuesSourceInfo(obj);
                if ( sfi.notnilp() ) {
                    return sfi;
                }
                T_sp cur = obj;
                for ( ; cur.notnilp(); cur=oCdr(cur) ) {
                    if ( cl_consp(cur) ) {
                        SourceFileInfo_mv sfisub = core_walkToFindSourceInfo(oCar(cur));
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
#define ARGS_core_walkToFindSourceInfo "(arg)"
#define DECL_core_walkToFindSourceInfo ""
#define DOCS_core_walkToFindSourceInfo "Walk down the tree looking for the first source info you find and return as multiple values"
    SourceFileInfo_mv core_walkToFindSourceInfo(T_sp obj)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            if ( cl_consp(obj) ) {
                SourceFileInfo_mv sfi = _lisp->sourceDatabase()->multipleValuesSourceInfo(obj);
                if ( sfi.notnilp() ) {
                    return sfi;
                }
                T_sp cur = obj;
                for ( ; cur.notnilp(); cur=oCdr(cur) ) {
                    if ( cl_consp(cur) ) {
                        SourceFileInfo_mv sfisub = core_walkToFindSourceInfo(oCar(cur));
                        if ( sfisub.notnilp() ) return sfisub;
                    } else {
                        return Values(_Nil<SourceFileInfo_O>());
                    }
                }
            }
        }
	return Values(_Nil<SourceFileInfo_O>());
    };
#endif


    

#define ARGS_core_walkToAssignSourcePosInfo "(obj top &optional stream)"
#define DECL_core_walkToAssignSourcePosInfo ""
#define DOCS_core_walkToAssignSourcePosInfo "Walk down the tree and carry source info down"
    void core_walkToAssignSourcePosInfo(T_sp obj, SourcePosInfo_sp topInfo, T_sp stream )
    {
	if ( _lisp->sourceDatabase().notnilp() ) {
	    if ( cl_consp(obj) ) {
		SourcePosInfo_sp curInfo = _lisp->sourceDatabase()->lookupSourcePosInfo(obj);
		if ( curInfo.nilp() ) {
		    curInfo = topInfo;
		    lisp_registerSourcePosInfo(obj,curInfo);
		    if ( stream.notnilp() ) {
			clasp_write_string("Updating SourcePosInfo ",stream);
			write_ugly_object(curInfo,stream);
			clasp_write_string(" --> ", stream);
			write_ugly_object(obj,stream);
			clasp_terpri(stream);
		    }
		}
		T_sp car = oCar(obj);
		if ( cl_consp(car) ) core_walkToAssignSourcePosInfo(car,curInfo,stream);
		T_sp cdr = oCdr(obj);
		if ( cl_consp(cdr) ) core_walkToAssignSourcePosInfo(cdr,curInfo,stream);
	    }
	}
    }
		


    


#define ARGS_core_walkToFindSourcePosInfo "(arg &optional default-spi)"
#define DECL_core_walkToFindSourcePosInfo ""
#define DOCS_core_walkToFindSourcePosInfo "Walk down the tree and find the first source info you can"
    SourcePosInfo_sp core_walkToFindSourcePosInfo(T_sp obj,T_sp defaultSpi)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            if ( cl_consp(obj) ) {
                SourcePosInfo_sp spi = _lisp->sourceDatabase()->lookupSourcePosInfo(obj);
                if ( spi.notnilp() ) {
                    return spi;
                }
                T_sp cur = obj;
                for ( ; cur.notnilp(); cur=oCdr(cur) ) {
                    if ( cl_consp(cur) ) {
                        SourcePosInfo_sp spisub = core_walkToFindSourcePosInfo(oCar(cur));
                        if ( spisub.notnilp() ) return spisub;
                    } else {
                        return defaultSpi.as<SourcePosInfo_O>();
                    }
                }
            }
        }
	return defaultSpi.as<SourcePosInfo_O>();
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
	CoreDefun(walkToFindSourcePosInfo);
	CoreDefun(walkToFindSourceInfo);
	CoreDefun(walkToAssignSourcePosInfo);

    }

    void SourceFileInfo_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SourceFileInfo,"","",_lisp)
	    ;
#endif
    }







    EXPOSE_CLASS(core,SourcePosInfo_O);

    string SourcePosInfo_O::__repr__() const
    {
        stringstream ss;
        ss << "#<" << this->_instanceClass()->classNameAsString();
        ss << " :fileId " << this->_FileId;
        ss << " :filepos " << this->_Filepos;
        ss << " :lineno " << this->_Lineno;
        ss << " :column " << this->_Column;
        ss << ">";
        return ss.str();
    }

    void SourcePosInfo_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<SourcePosInfo_O>()
	    ;
	CoreDefun(sourcePosInfoFilepos);
	CoreDefun(sourcePosInfoLineno);
	CoreDefun(sourcePosInfoColumn);
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
            return _lisp->sourceDatabase()->multipleValuesSourceInfo(obj);
        }
        return Values(_Nil<SourceFileInfo_O>());
    };



    
    
#define ARGS_af_dumpSourceManager "(dumpAll)"
#define DECL_af_dumpSourceManager ""
#define DOCS_af_dumpSourceManager "dumpSourceManager"
    void af_dumpSourceManager(T_sp dumpAll)
    {_G();
        if ( _lisp->sourceDatabase().notnilp() ) {
            _lisp->print(BF("Source Manager entries: %d\n") % _lisp->sourceDatabase()->_SourcePosInfo->size());
	    if ( dumpAll.isTrue() ) {
		printf("Dumping contents\n");
		_lisp->sourceDatabase()->dump();
	    }
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
	//        this->_SourcePosInfo = core_makeWeakKeyHashTable(Fixnum_O::create(1024));
	//	printf("%s:%d>>%s  WARNING:   SourceManager uses a regular hash table - this will gobble memory\n", __FILE__, __LINE__, __FUNCTION__ );
	this->_SourcePosInfo = HashTableEq_O::create_default();
    }


    SYMBOL_EXPORT_SC_(CorePkg,STARmonitorRegisterSourceInfoSTAR);
    SourcePosInfo_sp SourceManager_O::registerSourceInfo(T_sp key,
                                                         T_sp sourceFile,
							 size_t filepos,
                                                         uint lineno,
                                                         uint column )
    {_G();
        if ( _sym_STARmonitorRegisterSourceInfoSTAR->symbolValue().notnilp() ) {
            printf("%s:%d  registerSourceInfo  sourceFile: %s:%d:%d  --> %s\n", __FILE__, __LINE__, sourceFile->__repr__().c_str(), lineno, column, _rep_(key).c_str() );
            printf("%s:%d        *source-database* =\n",__FILE__, __LINE__);
            af_dumpSourceManager(_lisp->_true());
        }
        
        if ( this->availablep() ) {
            SourceFileInfo_mv sfi = af_sourceFileInfo(sourceFile);
            Fixnum_sp fileId = sfi.valueGet(1).as<Fixnum_O>();
            SourcePosInfo_sp info = SourcePosInfo_O::create(fileId->get(),filepos,lineno,column);
            this->_SourcePosInfo->setf_gethash(key,info);
            return info;
        }
        return _Nil<SourcePosInfo_O>();
    }


    SourcePosInfo_sp SourceManager_O::registerSourcePosInfo(T_sp obj, SourcePosInfo_sp info)
    {_G();
        if ( this->availablep() ) {
            this->_SourcePosInfo->setf_gethash(obj,info);
            return info;
        }
        return _Nil<SourcePosInfo_O>();
    }

#if 0
    SourcePosInfo_sp SourceManager_O::registerSourceInfoFromStream(T_sp obj, T_sp stream)
    {_G();
	SourceFileInfo_sp sfi  = clasp_input_source_file_info(stream);
	uint lineNumber = clasp_input_lineno(stream);
	uint column = clasp_input_column(stream);
	return this->registerSourceInfo(obj,sfi,lineNumber,column);
    }
#endif
    
    SourcePosInfo_sp SourceManager_O::duplicateSourcePosInfo(T_sp orig_obj, T_sp new_obj, T_sp macroExpansion)
    {
        if ( _lisp->sourceDatabase().notnilp()  ) {
            SourcePosInfo_sp info = _lisp->sourceDatabase()->lookupSourcePosInfo(orig_obj);
            if ( info.notnilp() ) {
		this->registerSourcePosInfo(new_obj,info);
		return info;
	    } else if (cl_consp(orig_obj)) {
		SourcePosInfo_sp walkInfo = core_walkToFindSourcePosInfo(orig_obj);
		if ( walkInfo.notnilp() ) {
		    this->registerSourcePosInfo(new_obj,walkInfo);
		    return walkInfo;
		}
		// Use the REPL source pos info
		SourcePosInfo_sp currentSourcePosInfo = _sym_STARcurrentSourcePosInfoSTAR->symbolValue().as<SourcePosInfo_O>();
		if ( currentSourcePosInfo.notnilp() ) {
		    this->registerSourcePosInfo(new_obj,currentSourcePosInfo);
		    return currentSourcePosInfo;
		}
		printf("%s:%d>>%s   Missing source info  for: %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(orig_obj).c_str() );
	    }
        }
	return _Nil<SourcePosInfo_O>();
    }
	

    void SourceManager_O::dump()
    {
	T_sp stream = cl::_sym_STARstandard_outputSTAR->symbolValue();
	this->_SourcePosInfo->mapHash( [this,&stream] (T_sp k, T_sp v) {
		cl_print(k,stream);
		clasp_write_string(" --> ",stream);
		cl_print(v,stream);
		clasp_terpri(stream);
	    } );
    }
			      

    SourceFileInfo_mv SourceManager_O::multipleValuesSourceInfo(T_sp key)
    {
        if ( this->availablep() ) {
	    T_mv result = this->_SourcePosInfo->gethash(key,_Nil<SourcePosInfo_O>());
	    SourcePosInfo_sp it = result.as<SourcePosInfo_O>();
            if (it.notnilp()) {
                SourceFileInfo_sp sfi = af_sourceFileInfo(Fixnum_O::create(it->_FileId));
                return Values(sfi
			      , Fixnum_O::create((int)it->_Filepos)
			      , Fixnum_O::create(it->_Lineno)
			      , Fixnum_O::create(it->_Column)
			      );
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
