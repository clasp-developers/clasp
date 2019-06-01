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
#include <clasp/core/bformat.h>
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
CL_DEFUN T_mv core__file_scope(T_sp sourceFile, T_sp sourceDebugPathname, size_t sourceDebugOffset, bool useLineno) {
  if (sourceFile.nilp()) {
    return core__file_scope(make_fixnum(0));
  } else if (cl__stringp(sourceFile)) {
    return _lisp->getOrRegisterFileScope(gc::As<String_sp>(sourceFile)->get_std_string(), sourceDebugPathname, sourceDebugOffset, useLineno);
  } else if (Pathname_sp pnSourceFile = sourceFile.asOrNull<Pathname_O>()) {
    T_sp ns = cl__namestring(pnSourceFile);
    if (ns.nilp()) {
      SIMPLE_ERROR(BF("No namestring could be generated for %s") % _rep_(pnSourceFile));
    }
    return _lisp->getOrRegisterFileScope(gc::As<String_sp>(ns)->get_std_string(), sourceDebugPathname, sourceDebugOffset, useLineno);
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
    return core__file_scope(sfi);
  } else if (FileScope_sp sfi = sourceFile.asOrNull<FileScope_O>()) {
    return _lisp->getOrRegisterFileScope(sfi->namestring(), sourceDebugPathname, sourceDebugOffset, useLineno);
  } else if (SourcePosInfo_sp spi = sourceFile.asOrNull<SourcePosInfo_O>()) {
    return core__file_scope(make_fixnum(spi->_FileId));
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
  if (gc::IsA<SourcePosInfo_sp>(info)) {
    SourcePosInfo_sp spi = gc::As_unsafe<SourcePosInfo_sp>(info);
    return Integer_O::create((gc::Fixnum)clasp_sourcePosInfo_fileHandle(spi));
  }
  SIMPLE_ERROR(BF("Argument %s must be a source-pos-info object") % _rep_(info));
}

CL_LAMBDA(source-pos-info);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfoFilepos");
CL_DEFUN Integer_sp core__source_pos_info_filepos(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  if (gc::IsA<SourcePosInfo_sp>(info)) {
    return Integer_O::create((gc::Fixnum)clasp_sourcePosInfo_filepos(gc::As_unsafe<SourcePosInfo_sp>(info)));
  }
  SIMPLE_ERROR(BF("Argument %s must be a source-pos-info object") % _rep_(info));
}

uint clasp_sourcePosInfo_lineno(SourcePosInfo_sp info) {
  return info->_Lineno;
}

CL_LAMBDA(source-pos-info);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfoLineno");
CL_DEFUN Fixnum_sp core__source_pos_info_lineno(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  if (gc::IsA<SourcePosInfo_sp>(info)) {
    return Integer_O::create((gc::Fixnum)clasp_sourcePosInfo_lineno(gc::As_unsafe<SourcePosInfo_sp>(info)));
  }
  SIMPLE_ERROR(BF("Argument %s must be a source-pos-info object") % _rep_(info));
}

uint clasp_sourcePosInfo_column(SourcePosInfo_sp info) {
  return info->_Column;
}

CL_LAMBDA(source-pos-info);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfoColumn");
CL_DEFUN Fixnum_sp core__source_pos_info_column(T_sp info) {
  if (info.nilp() ) return make_fixnum(0);
  if (gc::IsA<SourcePosInfo_sp>(info)) {
    return make_fixnum(clasp_sourcePosInfo_column(gc::As_unsafe<SourcePosInfo_sp>(info)));
  }
  SIMPLE_ERROR(BF("Argument %s must be a source-pos-info object") % _rep_(info));
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

FileScope_O::FileScope_O() : Base(), _PermanentPathName(NULL), _PermanentFileName(NULL){};

void FileScope_O::initialize() {
  this->Base::initialize();
}

FileScope_sp FileScope_O::create(Pathname_sp path, int handle, T_sp sourceDebugPathname, size_t sourceDebugOffset, bool useLineno) {
  GC_ALLOCATE(FileScope_O, sfi);
  sfi->_pathname = path;
  sfi->_FileHandle = handle;
  sfi->_SourceDebugPathname = sourceDebugPathname;
  sfi->_SourceDebugOffset = sourceDebugOffset;
  sfi->_TrackLineno = useLineno;
  return sfi;
}

void FileScope_O::fields(Record_sp node) {
  node->field(INTERN_(kw,pathname),this->_pathname);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    FileScope_mv sfi = _lisp->getOrRegisterFileScope(gc::As<String_sp>(cl__namestring(this->_pathname))->get());
    *this = *sfi;
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Add support to patch FileScope_O");
  } break;
  default: {
    // nothing
  }
  }
}
      
  
FileScope_sp FileScope_O::create(const string &str, int handle, T_sp truename, size_t offset, bool useLineno) {
  Pathname_sp pn = cl__pathname(SimpleBaseString_O::make(str));
  return FileScope_O::create(pn, handle, truename, offset, useLineno);
}

string FileScope_O::__repr__() const {
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

CL_LISPIFY_NAME("FileScope-sourceDebugPathname");
CL_DEFMETHOD Pathname_sp FileScope_O::sourceDebugPathname() const {
  if (this->_SourceDebugPathname.notnilp()) {
    return gc::As<Pathname_sp>(this->_SourceDebugPathname);
  }
  return this->_pathname;
}

string FileScope_O::fileName() const {
  String_sp s = gc::As<String_sp>(cl__file_namestring(this->_pathname));
  return s->get_std_string();
}

string FileScope_O::namestring() const {
  String_sp s = gc::As<String_sp>(cl__namestring(this->_pathname));
  return s->get_std_string();
}

string FileScope_O::parentPathName() const {
  String_sp s = gc::As<String_sp>(cl__directory_namestring(this->_pathname));
  return s->get_std_string();
}

const char *FileScope_O::permanentPathName() {
  if (this->_PermanentPathName == NULL) {
    string fn = this->namestring();
    this->_PermanentPathName = (char *)malloc(fn.size() + 1);
    ::strcpy(this->_PermanentPathName, fn.c_str());
  }
  return this->_PermanentPathName;
}

const char *FileScope_O::permanentFileName() {
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
  FileScope_mv sfi = _lisp->getOrRegisterFileScope(filename);
  uint sfi_handle = sfi->fileHandle();
  SourcePosInfo_sp res = SourcePosInfo_O::create(sfi_handle,filepos,lineno,column);
  return res;
}


CL_DEFMETHOD SourcePosInfo_sp SourcePosInfo_O::source_pos_info_copy() const {
  GC_COPY(SourcePosInfo_O, copy, *this);
  return copy;
}

CL_DEFMETHOD T_sp SourcePosInfo_O::source_pos_info_inlined_at() const {
  return this->_InlinedAt;
}

CL_DEFMETHOD T_sp SourcePosInfo_O::setf_source_pos_info_inlined_at(T_sp inlinedAt) {
  this->_InlinedAt = inlinedAt;
  return inlinedAt;
}



void SourcePosInfo_O::fields(Record_sp node)
{
  node->field(INTERN_(kw,fp),this->_Filepos);
  node->field(INTERN_(kw,l),this->_Lineno);
  node->field(INTERN_(kw,c),this->_Column);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    FileScope_sp sfi;
    node->field(INTERN_(kw,sfi),sfi);
    this->_FileId = sfi->_FileHandle;
  } break;
  case Record_O::saving: {
    FileScope_sp sfi = gc::As<FileScope_sp>(core__file_scope(make_fixnum(this->_FileId)));
    node->field(INTERN_(kw,sfi),sfi);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Handle patching of SourcePosInfo_O");
  } break;
  }
}

string SourcePosInfo_O::__repr__() const {
  if (cl::_sym_STARprint_readablySTAR->symbolValue().notnilp()) {
    stringstream ss;
    T_mv source_file_info = core__file_scope(core::make_fixnum(this->_FileId));
    String_sp namestring = gc::As<String_sp>(cl__namestring(gc::As<FileScope_sp>(source_file_info)->pathname()));
    ss << "#.(CORE:MAKE-SOURCE-POS-INFO \"" << namestring->get_std_string() << "\"";
    ss << " " << this->_Filepos << " " << this->_Lineno << " " << this->_Column << ")";
    return ss.str();
  }
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

SYMBOL_EXPORT_SC_(CorePkg, lookupFileScope);

};
