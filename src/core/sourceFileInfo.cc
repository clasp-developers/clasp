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
//#define DEBUG_LEVEL_FULL

#include <string.h>
#include <clasp/core/foundation.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/pathname.h>
#include <clasp/core/print.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/bundle.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(name &optional source-debug-pathname (source-debug-offset 0) (use-lineno t));
CL_DECLARE();
CL_DOCSTRING("sourceFileInfo given a source name (string) or pathname or integer, return the source-file-info structure and the integer index");
CL_DEFUN T_mv core__source_file_info(T_sp sourceFile, T_sp sourceDebugPathname, size_t sourceDebugOffset, bool useLineno) {
  if (sourceFile.nilp()) {
    return core__source_file_info(make_fixnum(0));
  } else if (cl__stringp(sourceFile)) {
    return _lisp->getOrRegisterSourceFileInfo(gc::As<String_sp>(sourceFile)->get_std_string(), sourceDebugPathname, sourceDebugOffset, useLineno);
  } else if (Pathname_sp pnSourceFile = sourceFile.asOrNull<Pathname_O>()) {
    T_sp ns = cl__namestring(pnSourceFile);
    if (ns.nilp()) {
      SIMPLE_ERROR(BF("No namestring could be generated for %s") % _rep_(pnSourceFile));
    }
    return _lisp->getOrRegisterSourceFileInfo(gc::As<String_sp>(ns)->get_std_string(), sourceDebugPathname, sourceDebugOffset, useLineno);
  } else if (sourceFile.fixnump()) { // Fixnum_sp fnSourceFile = sourceFile.asOrNull<Fixnum_O>() ) {
    WITH_READ_LOCK(_lisp->_Roots._SourceFilesMutex);
    Fixnum_sp fnSourceFile(gc::As<Fixnum_sp>(sourceFile));
    size_t idx = unbox_fixnum(fnSourceFile);
    if (idx >= _lisp->_Roots._SourceFiles.size()) {
      idx = 0;
      //                SIMPLE_ERROR(BF("Illegal index %d for source file info") % fnSourceFile->get() );
    }
    return Values(_lisp->_Roots._SourceFiles[idx], fnSourceFile);
  } else if (cl__streamp(sourceFile)) {
    T_sp so = sourceFile;
    T_sp sfi = clasp_input_source_file_info(so);
    return core__source_file_info(sfi);
  } else if (SourceFileInfo_sp sfi = sourceFile.asOrNull<SourceFileInfo_O>()) {
    return _lisp->getOrRegisterSourceFileInfo(sfi->namestring(), sourceDebugPathname, sourceDebugOffset, useLineno);
  } else if (SourcePosInfo_sp spi = sourceFile.asOrNull<SourcePosInfo_O>()) {
    return core__source_file_info(make_fixnum(spi->_FileId));
  }
  SIMPLE_ERROR(BF("Add support for source-file-info for %s") % _rep_(sourceFile));
};
};

namespace core {

uint clasp_sourcePosInfo_fileHandle(SourcePosInfo_sp info) {
  return info->_FileId;
}

size_t clasp_sourcePosInfo_filepos(SourcePosInfo_sp info) {
  return info->_Filepos;
}

CL_LAMBDA(source-pos-info);
CL_DEFUN Integer_mv core__source_pos_info_unpack(T_sp source_pos_info) {
  if (source_pos_info.nilp() ) return Values0<T_O>();
  SourcePosInfo_sp info = gc::As<SourcePosInfo_sp>(source_pos_info);
  return Values(Integer_O::create((gc::Fixnum)info->_FileId),
                Integer_O::create((gc::Fixnum)info->_Filepos),
                Integer_O::create((gc::Fixnum)info->_Lineno),
                Integer_O::create((gc::Fixnum)info->_Column));
}

CL_LAMBDA(source-pos-info);
CL_DEFUN Integer_sp core__source_pos_info_file_handle(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  return Integer_O::create((gc::Fixnum)clasp_sourcePosInfo_fileHandle(info));
}

CL_LAMBDA(source-pos-info);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfoFilepos");
CL_DEFUN Integer_sp core__source_pos_info_filepos(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  return Integer_O::create((gc::Fixnum)clasp_sourcePosInfo_filepos(info));
}

uint clasp_sourcePosInfo_lineno(SourcePosInfo_sp info) {
  return info->_Lineno;
}

CL_LAMBDA(source-pos-info);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfoLineno");
CL_DEFUN Fixnum_sp core__source_pos_info_lineno(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  return make_fixnum(clasp_sourcePosInfo_lineno(info));
}

uint clasp_sourcePosInfo_column(SourcePosInfo_sp info) {
  return info->_Column;
}

CL_LAMBDA(source-pos-info);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfoColumn");
CL_DEFUN Fixnum_sp core__source_pos_info_column(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  return make_fixnum(clasp_sourcePosInfo_column(info));
}
};

namespace core {

#define ARGS_af_lineno "(arg)"
#define DECL_af_lineno ""
#define DOCS_af_lineno "lineNumber"
uint af_lineno(T_sp obj) {
  if (obj.nilp()) {
    return 0;
  } else if (Cons_sp co = obj.asOrNull<Cons_O>()) {
    IMPLEMENT_MEF(BF("Handle cons %s for af_lineno") % _rep_(co));
  } else if (cl__streamp(obj)) {
    return clasp_input_lineno(obj);
  } else if (Closure_sp fo = obj.asOrNull<Closure_O>()) {
    return af_lineno(fo->sourcePosInfo());
  } else if (SourcePosInfo_sp info = obj.asOrNull<SourcePosInfo_O>()) {
    return info->_Lineno;
  }
  SIMPLE_ERROR(BF("Implement lineNumber for %s") % _rep_(obj));
};

#define ARGS_af_column "(arg)"
#define DECL_af_column ""
#define DOCS_af_column "column"
uint af_column(T_sp obj) {
  if (obj.nilp()) {
    return 0;
  } else if (Cons_sp co = obj.asOrNull<Cons_O>()) {
    (void)co;
    IMPLEMENT_MEF("Handle cons for af_column");
  } else if (cl__streamp(obj)) {
    return clasp_input_column(obj);
  } else if (Closure_sp fo = obj.asOrNull<Closure_O>()) {
    return af_column(fo->sourcePosInfo());
  } else if (SourcePosInfo_sp info = obj.asOrNull<SourcePosInfo_O>()) {
    return info->_Column;
  }
  SIMPLE_ERROR(BF("Implement column for %s") % _rep_(obj));
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("walkToFindSourceInfo");
CL_DEFUN T_mv core__walk_to_find_source_info(T_sp obj) {
  if (_lisp->sourceDatabase().notnilp()) {
    if ((obj).consp()) {
      T_sp tspi = gc::As<SourceManager_sp>(_lisp->sourceDatabase())->lookupSourcePosInfo(obj);
      if (SourcePosInfo_sp spi = tspi.asOrNull<SourcePosInfo_O>()) {
        SourceFileInfo_sp sfi = core__source_file_info(make_fixnum(spi->fileHandle()));
        Fixnum_sp fnlineno = make_fixnum(spi->_Lineno);
        Fixnum_sp fncolumn = make_fixnum(spi->_Column);
        Integer_sp fnfilepos = Integer_O::create((gc::Fixnum)spi->_Filepos);
        return Values(sfi, fnfilepos, fnlineno, fncolumn);
      }
      T_sp cur = obj;
      for (; cur.notnilp(); cur = oCdr(cur)) {
        if ((cur).consp()) {
          T_mv sfisub = core__walk_to_find_source_info(oCar(cur));
          if (sfisub.notnilp())
            return sfisub;
        } else {
          return Values(_Nil<T_O>());
        }
      }
    }
  }
  return Values(_Nil<T_O>());
};

CL_LAMBDA(obj top &optional stream);
CL_DECLARE();
CL_DOCSTRING("Walk down the tree and carry source info down");
CL_DEFUN void core__walk_to_assign_source_pos_info(T_sp obj, SourcePosInfo_sp topInfo, T_sp stream) {
  if (_lisp->sourceDatabase().notnilp()) {
    if ((obj).consp()) {
      T_sp curInfo = gc::As<SourceManager_sp>(_lisp->sourceDatabase())->lookupSourcePosInfo(obj);
      if (curInfo.nilp()) {
        curInfo = topInfo;
        lisp_registerSourcePosInfo(obj, curInfo);
        if (stream.notnilp()) {
          clasp_write_string("Updating SourcePosInfo ", stream);
          write_ugly_object(curInfo, stream);
          clasp_write_string(" --> ", stream);
          write_ugly_object(obj, stream);
          clasp_terpri(stream);
        }
      }
      T_sp car = oCar(obj);
      if ((car).consp())
        core__walk_to_assign_source_pos_info(car, curInfo, stream);
      T_sp cdr = oCdr(obj);
      if ((cdr).consp())
        core__walk_to_assign_source_pos_info(cdr, curInfo, stream);
    }
  }
}

CL_DOCSTRING("Lookup the form in the source manager and return its source position or NIL if not known");
CL_DEFUN T_sp core__source_manager_lookup(T_sp source_manager, T_sp form) {
  if (source_manager.notnilp() && form.consp()) {
    SourceManager_sp sm = gctools::As<SourceManager_sp>(source_manager);
    T_sp tspi = sm->lookupSourcePosInfo(form);
    if (SourcePosInfo_sp spi = tspi.asOrNull<SourcePosInfo_O>() ) {
      return spi;
    }
    return _Nil<T_O>();
  }
//  printf("%s:%d  source_manager.nilp() --> %d\n", __FILE__, __LINE__, source_manager.nilp());
//  printf("%s:%d  form --> %s\n", __FILE__,__LINE__, _rep_(form).c_str());
  return _Nil<T_O>();
}


CL_LAMBDA(arg &optional default-spi);
CL_DECLARE();
CL_DOCSTRING("Walk down the tree and find the first source info you can");
CL_DEFUN T_sp core__walk_to_find_source_pos_info(T_sp obj, T_sp defaultSpi) {
  if (_lisp->sourceDatabase().notnilp()) {
    if ((obj).consp()) {
      T_sp spi = gc::As<SourceManager_sp>(_lisp->sourceDatabase())->lookupSourcePosInfo(obj);
      if (spi.notnilp()) {
        return spi;
      }
      T_sp cur = obj;
      for (; cur.notnilp(); cur = oCdr(cur)) {
        if ((cur).consp()) {
          T_sp spisub = core__walk_to_find_source_pos_info(oCar(cur));
          if (spisub.notnilp())
            return spisub;
        } else {
          return defaultSpi;
        }
      }
    }
  }
  return defaultSpi;
};


SourceFileInfo_O::SourceFileInfo_O() : Base(), _PermanentPathName(NULL), _PermanentFileName(NULL){};

void SourceFileInfo_O::initialize() {
  this->Base::initialize();
}

SourceFileInfo_sp SourceFileInfo_O::create(Pathname_sp path, int handle, T_sp sourceDebugPathname, size_t sourceDebugOffset, bool useLineno) {
  GC_ALLOCATE(SourceFileInfo_O, sfi);
  sfi->_pathname = path;
  sfi->_FileHandle = handle;
  sfi->_SourceDebugPathname = sourceDebugPathname;
  sfi->_SourceDebugOffset = sourceDebugOffset;
  sfi->_TrackLineno = useLineno;
  return sfi;
}

void SourceFileInfo_O::fields(Record_sp node) {
  node->field(INTERN_(kw,pathname),this->_pathname);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    SourceFileInfo_mv sfi = _lisp->getOrRegisterSourceFileInfo(gc::As<String_sp>(cl__namestring(this->_pathname))->get());
    *this = *sfi;
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Add support to patch SourceFileInfo_O");
  } break;
  default: {
    // nothing
  }
  }
}
      
  
SourceFileInfo_sp SourceFileInfo_O::create(const string &str, int handle, T_sp truename, size_t offset, bool useLineno) {
  Pathname_sp pn = cl__pathname(SimpleBaseString_O::make(str));
  return SourceFileInfo_O::create(pn, handle, truename, offset, useLineno);
}

string SourceFileInfo_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
  ss << " " << _rep_(this->_pathname);
  ss << " :file-handle " << this->_FileHandle;
  ss << " :source-debug-pathname " << _rep_(this->_SourceDebugPathname);
  ss << " :source-debug-offset " << this->_SourceDebugOffset;
  ss << " :trackLineno " << this->_TrackLineno;
  ss << " >";
  return ss.str();
}

CL_LISPIFY_NAME("SourceFileInfo-sourceDebugPathname");
CL_DEFMETHOD Pathname_sp SourceFileInfo_O::sourceDebugPathname() const {
  if (this->_SourceDebugPathname.notnilp()) {
    return this->_SourceDebugPathname;
  }
  return this->_pathname;
}

string SourceFileInfo_O::fileName() const {
  String_sp s = gc::As<String_sp>(cl__file_namestring(this->_pathname));
  return s->get_std_string();
}

string SourceFileInfo_O::namestring() const {
  String_sp s = gc::As<String_sp>(cl__namestring(this->_pathname));
  return s->get_std_string();
}

string SourceFileInfo_O::parentPathName() const {
  String_sp s = gc::As<String_sp>(cl__directory_namestring(this->_pathname));
  return s->get_std_string();
}

const char *SourceFileInfo_O::permanentPathName() {
  if (this->_PermanentPathName == NULL) {
    string fn = this->namestring();
    this->_PermanentPathName = (char *)malloc(fn.size() + 1);
    ::strcpy(this->_PermanentPathName, fn.c_str());
  }
  return this->_PermanentPathName;
}

const char *SourceFileInfo_O::permanentFileName() {
  if (this->_PermanentFileName == NULL) {
    string fn = this->fileName();
    this->_PermanentFileName = (char *)malloc(fn.size() + 1);
    ::strcpy(this->_PermanentFileName, fn.c_str());
  }
  return this->_PermanentFileName;
}



  SYMBOL_EXPORT_SC_(CorePkg, walkToFindSourceInfo);




SourcePosInfo_sp SourcePosInfo_O::make(const string& filename, size_t filepos, size_t lineno, size_t column)
{
  SourceFileInfo_mv sfi = _lisp->getOrRegisterSourceFileInfo(filename);
  uint sfi_handle = sfi->fileHandle();
  return SourcePosInfo_O::create(sfi_handle,filepos,lineno,column);
}


void SourcePosInfo_O::fields(Record_sp node)
{
  node->field(INTERN_(kw,fp),this->_Filepos);
  node->field(INTERN_(kw,l),this->_Lineno);
  node->field(INTERN_(kw,c),this->_Column);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    SourceFileInfo_sp sfi;
    node->field(INTERN_(kw,sfi),sfi);
    this->_FileId = sfi->_FileHandle;
  } break;
  case Record_O::saving: {
    SourceFileInfo_sp sfi = core__source_file_info(make_fixnum(this->_FileId));
    node->field(INTERN_(kw,sfi),sfi);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Handle patching of SourcePosInfo_O");
  } break;
  }
}

string SourcePosInfo_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
  ss << " :fileId " << this->_FileId;
  ss << " :filepos " << this->_Filepos;
  ss << " :lineno " << this->_Lineno;
  ss << " :column " << this->_Column;
  ss << ">";
  return ss.str();
}

bool SourcePosInfo_O::equalp(T_sp other) const {
  if (this == &*other) { return true; };
  if (!gc::IsA<SourcePosInfo_sp>(other)) return false;
  SourcePosInfo_sp spi_other = gc::As_unsafe<SourcePosInfo_sp>(other);
  if (this->_FileId != spi_other->_FileId) return false;
  if (this->_Filepos != spi_other->_Filepos) return false;
  if (this->_Lineno != spi_other->_Lineno) return false;
  if (this->_Column != spi_other->_Column) return false;
  return true;
}

CL_LAMBDA(dumpAll);
CL_DECLARE();
CL_DOCSTRING("dumpSourceManager");
CL_DEFUN void core__dump_source_manager(T_sp dumpAll) {
  if (_lisp->sourceDatabase().notnilp()) {
    _lisp->print(BF("Source Manager entries: %d\n") % gc::As<SourceManager_sp>(_lisp->sourceDatabase())->_SourcePosInfo->size());
    if (dumpAll.isTrue()) {
      printf("Dumping contents\n");
      gc::As<SourceManager_sp>(_lisp->sourceDatabase())->dump();
    }
  } else {
    _lisp->print(BF("No source manager available"));
  }
};



CL_DEFUN void core__source_manager_empty(T_sp obj)
{
  if (SourceManager_sp sm = obj.asOrNull<SourceManager_O>() ) {
    sm->empty();
  }
}



CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("makeSourceManager");
CL_DEFUN T_sp core__make_source_manager() {
  SourceManager_sp sm = SourceManager_O::create();
  return sm;
};

  SYMBOL_EXPORT_SC_(CorePkg, lookupSourceFileInfo);




void SourceManager_O::initialize() {
  this->Base::initialize();
//        this->_SourcePosInfo = core__make_weak_key_hash_table(make_fixnum(1024));
//	printf("%s:%d>>%s  WARNING:   SourceManager uses a regular hash table - this will gobble memory\n", __FILE__, __LINE__, __FUNCTION__ );
#ifdef USE_WEAK_HASH_TABLE_FOR_SOURCE_POS_INFO
  this->_SourcePosInfo = WeakKeyHashTable_O::create();
#else
  this->_SourcePosInfo = HashTableEq_O::create_default();
#endif
}

SYMBOL_EXPORT_SC_(CorePkg, STARmonitorRegisterSourceInfoSTAR);
T_sp SourceManager_O::registerSourceInfo(T_sp key,
                                         T_sp sourceFile,
                                         size_t filepos,
                                         uint lineno,
                                         uint column) {
  if (_sym_STARmonitorRegisterSourceInfoSTAR->symbolValue().notnilp()) {
    printf("%s:%d  registerSourceInfo  sourceFile: %s:%d:%d  --> %s\n", __FILE__, __LINE__, sourceFile.as<General_O>()->__repr__().c_str(), lineno, column, _rep_(key).c_str());
    printf("%s:%d        *source-database* =\n", __FILE__, __LINE__);
    core__dump_source_manager(_lisp->_true());
  }

  if (this->availablep()) {
    SourceFileInfo_sp sfi = core__source_file_info(sourceFile);
    SourcePosInfo_sp info = SourcePosInfo_O::create(sfi->fileHandle(), filepos, lineno, column);
    this->_SourcePosInfo->setf_gethash(key, info);
    return info;
  }
  return _Nil<T_O>();
}

T_sp SourceManager_O::registerSourcePosInfo(T_sp obj, SourcePosInfo_sp info) {
  if (this->availablep() && !cl__atom(obj)) {
    this->_SourcePosInfo->setf_gethash(obj, info);
    return info;
  }
  return _Nil<T_O>();
}

#if 0
    SourcePosInfo_sp SourceManager_O::registerSourceInfoFromStream(T_sp obj, T_sp stream)
    {
	SourceFileInfo_sp sfi  = clasp_input_source_file_info(stream);
	uint lineNumber = clasp_input_lineno(stream);
	uint column = clasp_input_column(stream);
	return this->registerSourceInfo(obj,sfi,lineNumber,column);
    }
#endif

T_sp SourceManager_O::duplicateSourcePosInfo(T_sp orig_obj, T_sp new_obj, T_sp macroExpansion) {
  if (_lisp->sourceDatabase().notnilp()) {
    T_sp info = gc::As<SourceManager_sp>(_lisp->sourceDatabase())->lookupSourcePosInfo(orig_obj);
    if (info.notnilp()) {
      this->registerSourcePosInfo(new_obj, info);
      return info;
    } else if ((orig_obj).consp()) {
      T_sp walkInfo = core__walk_to_find_source_pos_info(orig_obj);
      if (walkInfo.notnilp()) {
        this->registerSourcePosInfo(new_obj, walkInfo);
        return walkInfo;
      }
      // Use the REPL source pos info
      T_sp currentSourcePosInfo = _sym_STARcurrentSourcePosInfoSTAR->symbolValue();
      if (currentSourcePosInfo.notnilp()) {
        this->registerSourcePosInfo(new_obj, currentSourcePosInfo);
        return currentSourcePosInfo;
      }
      printf("%s:%d>>%s   Missing source info  for: %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(orig_obj).c_str());
    }
  }
  return _Nil<T_O>();
}

void SourceManager_O::dump() {
  T_sp stream = cl::_sym_STARstandard_outputSTAR->symbolValue();
  this->_SourcePosInfo->maphash([this, &stream](T_sp k, T_sp v) {
		cl__print(k,stream);
		clasp_write_string(" --> ",stream);
		cl__print(v,stream);
		clasp_terpri(stream);
  });
}

T_sp SourceManager_O::lookupSourcePosInfo(T_sp key) {
  if (this->availablep()) {
    T_sp it = this->_SourcePosInfo->gethash(key, _Nil<T_O>());
    return it;
  }
  return _Nil<T_O>();
}
};
