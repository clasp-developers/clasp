#pragma once
/*
    File: fileSystem.h
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
// stuff

#include <filesystem>
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/iterator.h>
#include <clasp/core/pathname.fwd.h>

template <> struct gctools::GCInfo<core::Path_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

SMART(Path);
class Path_O : public General_O {
  friend class XmlSaveArchive_O;
  LISP_CLASS(core, CorePkg, Path_O, "path", General_O);

public:
  void initialize();

private:
  dont_expose<std::filesystem::path> _Path;

public:
  static Path_sp create(const string& path);
  static Path_sp create(std::filesystem::path p);
  static Path_sp pathnameDesignator(T_sp obj);

public:
  std::filesystem::path& getPath() { return this->_Path._value; };

public:
  CL_LISPIFY_NAME("isAbsolute");
  CL_DEFMETHOD bool isAbsolute() const { return this->_Path._value.is_absolute(); };

  Path_sp copyPath() const;

  /*! Generate the std::filesystem complete path */
  Path_sp absolute() const;

  /*! Return the POSIX time_t value for the last_write_time */
  Integer_sp last_write_time() const;

  void setPath(const std::filesystem::path& p);
  void setPathFromString(const string& path);

  void sxhash_equal(HashGenerator& hg) const;
  Path_sp parent_path();

  /*! Return just the fileName (*--end) as a string*/
  string fileName() const;

  /*! Return the path as a string */
  string asString() const;

  string __repr__() const;

  /*! If the fileName has aaa/bbbb/xxxx.yyy
   * then the fileName is xxxx.yyy
   * and this function only returns the xxxx part
   * the prefix of the fileName
   */
  string stem();

  /*! Return the extension */
  string extension();

  /*! Append to the extension with this new extension */
  void appendToExtension(string const& newExtension);

  /*! Replace the extension with this new extension */
  Path_sp replaceExtension(string const& newExtension);

  /*! Append a path component */
  Path_O& operator/=(string const& pp);

  /*! Append to the path - returns itself */
  Path_sp path_append(string const& pp);

  /*! Break the path up into parts. */
  List_sp parts() const;

  List_sp glob(const string& globTemplate);

  /*! Return true if the file pointed to by this path exists */
  bool exists();
  Path_O(const Path_O& ss); //!< Copy constructor

  DEFAULT_CTOR_DTOR(Path_O);
};
}; // namespace core
template <> struct gctools::GCInfo<core::DirectoryIterator_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
SMART(DirectoryIterator);
class DirectoryIterator_O : public Iterator_O {
  LISP_CLASS(core, CorePkg, DirectoryIterator_O, "DirectoryIterator", Iterator_O);

public:
  void initialize();
  GCPRIVATE : Path_sp _Path;
  /* A new CurrentIterator is created (new) whenever first() is called
           So we have to manage the memory for _CurrentIterator
         */
  dont_expose<std::filesystem::directory_iterator*> _CurrentIterator;
  dont_expose<std::filesystem::directory_iterator> _EndIterator;

public:
  DirectoryIterator_sp create(Path_sp path);

private:
  void setupCurrentIterator();
  void setPath(Path_sp p);

public:
  virtual void first();
  virtual void next();
  virtual bool isDone();
  virtual T_sp currentObject();
  explicit DirectoryIterator_O() : Base(), _CurrentIterator((std::filesystem::directory_iterator*)NULL){};
  virtual ~DirectoryIterator_O(); // non-trivial destructor
};
}; // namespace core

template <> struct gctools::GCInfo<core::RecursiveDirectoryIterator_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
SMART(RecursiveDirectoryIterator);
class RecursiveDirectoryIterator_O : public Iterator_O {
  LISP_CLASS(core, CorePkg, RecursiveDirectoryIterator_O, "RecursiveDirectoryIterator", Iterator_O);

public:
  void initialize();
  GCPRIVATE : Path_sp _Path;
  bool _EnterHidden;
  /* A new CurrentIterator is created (new) whenever first() is called
           So we have to manage the memory for _CurrentIterator
         */
  dont_expose<std::filesystem::recursive_directory_iterator*> _CurrentIterator;
  dont_expose<std::filesystem::recursive_directory_iterator> _EndIterator;

public:
  RecursiveDirectoryIterator_sp create(Path_sp path);

private:
  void setupCurrentIterator();
  void setPath(Path_sp p);
  void setHidden(bool b) { this->_EnterHidden = b; };

public:
  virtual void first();
  virtual void next();
  virtual bool isDone();
  virtual T_sp currentObject();
  explicit RecursiveDirectoryIterator_O() : Base(), _CurrentIterator((std::filesystem::recursive_directory_iterator*)NULL){};
  virtual ~RecursiveDirectoryIterator_O(); // nontrivial
};
}; // namespace core

namespace core {
SMART(FileStatus);

SMART(DirectoryEntry);
class DirectoryEntry_O : public General_O {
  LISP_CLASS(core, CorePkg, DirectoryEntry_O, "DirectoryEntry", General_O);

public:
  void initialize();

private:
  std::filesystem::directory_entry* _Entry;

public:
  void setEntry(const std::filesystem::directory_entry& entry);

public:
  FileStatus_sp fileStatus();
  FileStatus_sp symlinkStatus();
  Path_sp path();
  explicit DirectoryEntry_O() : DirectoryEntry_O::Base(), _Entry(NULL){};
  virtual ~DirectoryEntry_O(); // Nontrivial
};
}; // namespace core
template <> struct gctools::GCInfo<core::DirectoryEntry_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
SMART(FileStatus);
class FileStatus_O : public General_O {
  LISP_CLASS(core, CorePkg, FileStatus_O, "FileStatus", General_O);

public:
  void initialize();

private:
  std::filesystem::file_status _FileStatus;

public:
  void setFileStatus(const std::filesystem::file_status& fs);

public:
  bool exists();
  bool isRegularFile();
  bool isDirectory();
  bool isSymlink();
  bool isOther();
  DEFAULT_CTOR_DTOR(FileStatus_O);
};

//    extern void rename_file(Path_sp src, Path_sp dest);
//    extern bool delete_file(Path_sp dest);
}; // namespace core

namespace core {
Pathname_sp homedirPathname(T_sp head); // See ecl_homedir_pathname

/*! Return the current working directory as a string, if bool change_d_p_d then
     Change *default-pathname-defaults* to cwd */
Pathname_sp getcwd(bool change_d_p_d = false);
}; // namespace core
