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
class SourceFileInfo_O : public General_O {
  LISP_CLASS(core, CorePkg, SourceFileInfo_O, "SourceFileInfo",General_O);
 public:
  bool fieldsp() const { return true; };
  void fields(Record_sp node);
public:
  static SourceFileInfo_sp create(Pathname_sp path, int handle, T_sp truename = _Nil<T_O>(), size_t offset = 0, bool useLineno = true);
  static SourceFileInfo_sp create(const string &fileNamePath, int handle, T_sp truename = _Nil<T_O>(), size_t offset = 0, bool useLineno = true);

public: // ctor/dtor for classes with shared virtual base
  explicit SourceFileInfo_O();
  virtual ~SourceFileInfo_O(){};
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
CL_LISPIFY_NAME("SourceFileInfo-pathname");
CL_DEFMETHOD   Pathname_sp pathname() const { return this->_pathname; };
  const char *permanentPathName();
  const char *permanentFileName();

CL_LISPIFY_NAME("SourceFileInfo-useLineno");
CL_DEFMETHOD   bool useLineno() const { return this->_TrackLineno; };
CL_LISPIFY_NAME("SourceFileInfo-sourceDebugOffset");
CL_DEFMETHOD   size_t sourceDebugOffset() const { return this->_SourceDebugOffset; };
  string __repr__() const;
}; // SourceFileInfo class

FORWARD(SourcePosInfo);
class SourcePosInfo_O : public General_O {
  friend class SourceManager_O;
  friend T_mv core__source_file_info(T_sp sourceFile, T_sp truename, size_t offset, bool useLineno);

  LISP_CLASS(core, CorePkg, SourcePosInfo_O, "SourcePosInfo",General_O);
 public:
  bool fieldsp() const { return true; };
  void fields(Record_sp node);
public:                                                                                    // ctor/dtor for classes with shared virtual base
  explicit SourcePosInfo_O() : _FileId(UNDEF_UINT), _Filepos(0), _Lineno(0), _Column(0){}; //, _Filepos(0) {};
public:                                                                                    // instance variables here
  SourcePosInfo_O(uint spf, size_t filepos, uint spln, uint spc)                           // , Function_sp expander=_Nil<Function_O>())
      : _FileId(spf),
        _Filepos(filepos),
        _Lineno(spln),
        _Column(spc) //, _Expander(expander) {}
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
  //	Function_sp 	_Expander;
  CL_DEFMETHOD size_t source_file_pos_filepos() const { return this->_Filepos;}
  CL_DEFMETHOD size_t source_file_pos_lineno() const { return this->_Lineno;}
  CL_DEFMETHOD size_t source_file_pos_column() const { return this->_Column;}
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
class SourceManager_O : public General_O {
  LISP_CLASS(core, CorePkg, SourceManager_O, "ClaspSourceManager",General_O);
  void initialize();

public: // ctor/dtor for classes with shared virtual base
  explicit SourceManager_O(){};
  virtual ~SourceManager_O(){};

public: // instance variables here
#ifdef USE_WEAK_HASH_TABLE_FOR_SOURCE_POS_INFO
  typedef WeakKeyHashTable_sp HashTableType;
#else
  typedef HashTableEq_sp HashTableType;
#endif
  HashTableType _SourcePosInfo;

public: // Functions here
  /*! Return true if the SourceManager is available */
  bool availablep() const { return true; };

  /*! Register the object with the source manager */
  T_sp registerSourceInfo(T_sp obj, T_sp sourceFile, size_t filepos, uint lineno, uint column);

  void dump();

  T_sp registerSourcePosInfo(T_sp obj, SourcePosInfo_sp spi);

  //        SourceFileInfo_sp sourceFileInfoFromIndex(int idx) const;

  //	SourcePosInfo_sp registerSourceInfoFromStream(T_sp obj, T_sp stream);

  /*! Duplicate the source code information associated with orig_obj for new_obj 
	 In the future I could do something more sophisticated with macroExpansionFunction*/
  T_sp duplicateSourcePosInfo(T_sp orig_obj, T_sp new_obj, T_sp macroExpansionFunction = _Nil<T_O>());

  T_sp lookupSourcePosInfo(T_sp obj);

  void empty() {
    this->_SourcePosInfo->clrhash();
  }

}; // SourceManager class


 T_sp core__source_manager_lookup(T_sp source_manager, T_sp form);
 
T_mv core__walk_to_find_source_info(T_sp obj);
T_sp core__walk_to_find_source_pos_info(T_sp obj, T_sp defaultSpi = _Nil<T_O>());
//    SourceFileInfo_mv af_lookupSourceFileInfo(T_sp obj);

T_mv core__source_file_info(T_sp sourceFile, T_sp truename = _Nil<T_O>(), size_t offset = 0, bool useLineno = true);

}; // core namespace

extern "C" {
void dumpSourceInfo(core::T_sp exp);
};

#endif /* _core__source_file_info_H_ */
