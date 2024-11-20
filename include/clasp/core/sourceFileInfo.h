#pragma once
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

#include <clasp/core/object.h>
#include <clasp/core/pathname.fwd.h>
#include <clasp/core/fileSystem.fwd.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/sourceFileInfo.fwd.h>

namespace core {

class Scope_O : public General_O {
  LISP_CLASS(core, CorePkg, Scope_O, "Scope", General_O);

public:
  CLASP_DEFAULT_CTOR Scope_O(){};
};

class FileScope_O : public Scope_O {
  LISP_CLASS(core, CorePkg, FileScope_O, "FileScope", Scope_O);

public:
  bool fieldsp() const override { return true; };
  void fields(Record_sp node) override;

public:
  static FileScope_sp create(Pathname_sp path, int handle);
  static FileScope_sp create(const string& fileNamePath, int handle);

public: // ctor/dtor for classes with shared virtual base
  explicit FileScope_O();
  void initialize() override;
  GCPRIVATE : // instance variables here
              Pathname_sp _pathname;
  /*! Allocated buffer that stores the file name until the program exits */
  char* _PermanentPathName;
  char* _PermanentFileName;
  int _FileHandle;

public: // Functions here
  int fileHandle() const { return this->_FileHandle; };
  string fileName() const;
  string parentPathName() const;
  string namestring() const;
  CL_LISPIFY_NAME("FileScope-pathname");
  CL_DEFMETHOD Pathname_sp pathname() const { return this->_pathname; };
  const char* permanentPathName();
  const char* permanentFileName();
  string __repr__() const override;
}; // FileScope class

FORWARD(SourcePosInfo);
class SourcePosInfo_O : public General_O {
  friend T_mv core__file_scope(T_sp sourceFile);

  LISP_CLASS(core, CorePkg, SourcePosInfo_O, "SourcePosInfo", General_O);

public:
  bool fieldsp() const override { return true; };
  void fields(Record_sp node) override;

public: // ctor/dtor for classes with shared virtual base
  explicit SourcePosInfo_O()
      : _FileId(UNDEF_UINT), _Filepos(0), _Lineno(0), _Column(0), _FunctionScope(nil<T_O>()),
        _InlinedAt(nil<T_O>()){}; //, _Filepos(0) {};
public:                           // instance variables here
  SourcePosInfo_O(uint spf, size_t filepos, uint spln, uint spc, T_sp function_scope, T_sp inlined_at)
      : _FileId(spf), _Filepos(filepos), _Lineno(spln), _Column(spc), //, _Expander(expander) {}
        _FunctionScope(function_scope), _InlinedAt(inlined_at){};

public:
  static SourcePosInfo_sp create(uint spf, size_t filepos, uint spln, uint spcol, T_sp function_scope = nil<core::T_O>(),
                                 T_sp inlined_at = nil<core::T_O>()) {
    auto me = gctools::GC<SourcePosInfo_O>::allocate(spf, filepos, spln, spcol, function_scope, inlined_at);
    return me;
  }
  string __repr__() const override;
  int fileHandle() const { return this->_FileId; };
  size_t filepos() const { return this->_Filepos; };
  uint lineno() const { return this->_Lineno; };
  int column() const { return this->_Column; };
  T_sp function_scope() const { return this->_FunctionScope; };
  T_sp inlined_at() const { return this->_InlinedAt; };
  //  bool equalp(T_sp obj) const;
public:
  uint _FileId;
  size_t _Filepos;
  uint _Lineno;
  uint _Column;
  T_sp _FunctionScope;
  T_sp _InlinedAt;
  //	Function_sp 	_Expander;
  CL_DEFMETHOD size_t source_file_pos_filepos() const { return this->_Filepos; }
  CL_DEFMETHOD size_t source_file_pos_lineno() const { return this->_Lineno; }
  CL_DEFMETHOD size_t source_file_pos_column() const { return this->_Column; }
  SourcePosInfo_sp source_pos_info_copy() const;
  T_sp setf_source_pos_info_inlined_at(T_sp inlinedAt);
  T_sp source_pos_info_inlined_at() const;
  T_sp setf_source_pos_info_function_scope(T_sp function_scope);
  T_sp source_pos_info_function_scope() const;
  void setf_source_pos_info_extra(T_sp, T_sp);
};

SourcePosInfo_sp core__makeSourcePosInfo(const string& filename, bool filenamep, size_t filepos, bool fileposp, size_t lineno,
                                         bool linenop, size_t column, bool columnp, T_sp function_scope = nil<T_O>(),
                                         bool function_scope_p = false, T_sp inlined_at = nil<T_O>(), bool inlined_at_p = false,
                                         T_sp defaults = nil<T_O>(), bool defaults_p = false);

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
}; // namespace core
template <> struct gctools::GCInfo<core::SourcePosInfo_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

T_mv core__file_scope(T_sp sourceFile);

}; // namespace core

extern "C" {
void dumpSourceInfo(core::T_sp exp);
};
