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

namespace core {

SMART(Path);
class Path_O : public General_O {
  LISP_CLASS(core, CorePkg, Path_O, "path", General_O);

public:
  Path_O() = default;
  Path_O(const Path_O& ss) : Base(ss), _Path(ss._Path) {}

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
};
}; // namespace core

namespace core {
Pathname_sp homedirPathname(T_sp head); // See ecl_homedir_pathname

/*! Return the current working directory as a string, if bool change_d_p_d then
     Change *default-pathname-defaults* to cwd */
Pathname_sp getcwd(bool change_d_p_d = false);
}; // namespace core
