/*
    File: sourceFileInfo.h
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

#define USE_WEAK_HASH_TABLE_FOR_SOURCE_POS_INFO 1

#ifndef _core__source_file_info_H_
#define _core__source_file_info_H_

#include <clasp/core/object.h>
#include <clasp/core/pathname.fwd.h>
#include <clasp/core/fileSystem.fwd.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/sourceFileInfo.fwd.h>

namespace core {

class Scope_O : public General_O {
  LISP_CLASS(core, CorePkg, Scope_O, "Scope", General_O);
public:
};


class FileScope_O : public Scope_O {
  LISP_CLASS(core, CorePkg, FileScope_O, "FileScope",Scope_O);
 public:
  bool fieldsp() const { return true; };
  void fields(Record_sp node);
public:
  static FileScope_sp create(Pathname_sp path, int handle, T_sp truename = _Nil<T_O>(), size_t offset = 0, bool useLineno = true);
  static FileScope_sp create(const string &fileNamePath, int handle, T_sp truename = _Nil<T_O>(), size_t offset = 0, bool useLineno = true);

public: // ctor/dtor for classes with shared virtual base
  explicit FileScope_O();
  virtual ~FileScope_O(){};
  void initialize();
GCPRIVATE: // instance variables here
  Pathname_sp _pathname;
  /*! Allocated buffer that stores the file name until the program exits */
  char *_PermanentPathName;
  char *_PermanentFileName;
  int _FileHandle;
  /* These next two are used for compiling from a temp file like SLIME does.
   * In that case, the actual tempfile is the pathname, but the file it's
   * excerpted from has its namestring stored here. The offset is the offset
   * of the tempfile in that file.
   */
  T_sp _SourceDebugPathname;
  size_t _SourceDebugOffset;
  bool _TrackLineno;

public: // Functions here
  int fileHandle() const { return this->_FileHandle; };
  /*! Return the value of _SourceDebugPathname unless nil then return _pathname */
  Pathname_sp sourceDebugPathname() const;
  string fileName() const;
  string parentPathName() const;
  string namestring() const;
CL_LISPIFY_NAME("FileScope-pathname");
CL_DEFMETHOD   Pathname_sp pathname() const { return this->_pathname; };
  const char *permanentPathName();
  const char *permanentFileName();

CL_LISPIFY_NAME("FileScope-useLineno");
CL_DEFMETHOD   bool useLineno() const { return this->_TrackLineno; };
CL_LISPIFY_NAME("FileScope-sourceDebugOffset");
CL_DEFMETHOD   size_t sourceDebugOffset() const { return this->_SourceDebugOffset; };
  string __repr__() const;
}; // FileScope class







FORWARD(SourcePosInfo);
class SourcePosInfo_O : public General_O {
  friend T_mv core__file_scope(T_sp sourceFile, T_sp truename, size_t offset, bool useLineno);

  LISP_CLASS(core, CorePkg, SourcePosInfo_O, "SourcePosInfo",General_O);
 public:
  bool fieldsp() const { return true; };
  void fields(Record_sp node);
public:                                                                                    // ctor/dtor for classes with shared virtual base
  explicit SourcePosInfo_O() : _FileId(UNDEF_UINT), _Filepos(0), _Lineno(0), _Column(0), _InlinedAt(_Nil<T_O>()){}; //, _Filepos(0) {};
public:                                                                                    // instance variables here
  SourcePosInfo_O(uint spf, size_t filepos, uint spln, uint spc)                           // , Function_sp expander=_Nil<Function_O>())
      : _FileId(spf),
        _Filepos(filepos),
        _Lineno(spln),
        _Column(spc), //, _Expander(expander) {}
        _InlinedAt(_Nil<T_O>())
        {};

public:
  static SourcePosInfo_sp create(uint spf, size_t filepos, uint spln, uint spcol) {
    GC_ALLOCATE_VARIADIC(SourcePosInfo_O, me, spf, filepos, spln, spcol); // ,filepos,fn);
    return me;
  }
  CL_LISPIFY_NAME(make_source_pos_info);
  CL_DEF_CLASS_METHOD static SourcePosInfo_sp make(const string& filename, size_t filepos, size_t lineno, size_t column);
  string __repr__() const;
  int fileHandle() const { return this->_FileId; };
  size_t filepos() const { return this->_Filepos; };
  uint lineno() const { return this->_Lineno; };
  int column() const { return this->_Column; };
  bool equalp(T_sp obj) const;
public:
  uint _FileId;
  size_t _Filepos;
  uint _Lineno;
  uint _Column;
  T_sp _InlinedAt;
  //	Function_sp 	_Expander;
  CL_DEFMETHOD size_t source_file_pos_filepos() const { return this->_Filepos;}
  CL_DEFMETHOD size_t source_file_pos_lineno() const { return this->_Lineno;}
  CL_DEFMETHOD size_t source_file_pos_column() const { return this->_Column;}
  SourcePosInfo_sp source_pos_info_copy() const;
  T_sp setf_source_pos_info_inlined_at(T_sp inlinedAt);
  T_sp source_pos_info_inlined_at() const;
};
inline core::Fixnum safe_fileId(T_sp spi) {
  if (spi.nilp())
    return 0;
  return gc::As<SourcePosInfo_sp>(spi)->_FileId;
}

inline core::Fixnum safe_filepos(T_sp spi) {
  if (spi.nilp())
    return 0;
  return gc::As<SourcePosInfo_sp>(spi)->_FileId;
}

inline core::Fixnum safe_lineno(T_sp spi) {
  if (spi.nilp())
    return 0;
  return gc::As<SourcePosInfo_sp>(spi)->_Lineno;
}

inline core::Fixnum safe_column(T_sp spi) {
  if (spi.nilp())
    return 0;
  return gc::As<SourcePosInfo_sp>(spi)->_Column;
}
// Pass all arguments to a FunctionClosure
#define SOURCE_POS_INFO_FIELDS(spi) safe_fileId(spi), safe_filepos(spi), safe_lineno(spi), safe_column(spi)
};
template <>
struct gctools::GCInfo<core::SourcePosInfo_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

T_mv core__file_scope(T_sp sourceFile, T_sp truename = _Nil<T_O>(), size_t offset = 0, bool useLineno = true);

}; // core namespace

extern "C" {
void dumpSourceInfo(core::T_sp exp);
};

#endif /* _core__source_file_info_H_ */
