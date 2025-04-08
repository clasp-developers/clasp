/*
    File: fileSystem.cc
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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>

#include <sys/types.h>
#include <pwd.h>
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
#else
#include <uuid/uuid.h>
#endif

#include <clasp/core/corePackage.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/ql.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/pathname.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/unixfsys.h>
#include <string>

namespace boost {
void assertion_failed_msg(char const* expr, char const* msg, char const* function, char const* file, long line) {
  THROW_HARD_ERROR("boost::assertion_failed_msg was called with expr[{}] msg[{}] function[{}] file[{}] line[{}]", expr, msg,
                   function, file, line);
}
} // namespace boost

namespace core {

void rename_file(Path_sp rpath1, Path_sp rpath2) { return std::filesystem::rename(rpath1->getPath(), rpath2->getPath()); }

bool delete_file(Path_sp rpath) { return std::filesystem::remove(rpath->getPath()); }

int delete_all_files(Path_sp rpath) { return std::filesystem::remove_all(rpath->getPath()); }

/*! Rename src to dest but first check if dest already exists and create a backup
      of it.  If the backup already exists then it is deleted */
void safeRename(Path_sp src, Path_sp dest) {
  // If the OriginalFilePath exists then create a backup
  if (dest->exists()) {
    LOG("destination file[{}] exists", dest->asString());
    Path_sp destBackup = dest->copyPath();
    destBackup->appendToExtension("Backup");
    if (destBackup->exists()) {
      LOG("destination backup file[{}] exists - removing it", destBackup->asString());
      delete_file(destBackup);
    }
    LOG("Renaming dest[{}] to destBackup[{}]", dest->asString(), destBackup->asString());
    rename_file(dest, destBackup);
  }
  LOG("Renaming src[{}] to dest[{}]", src->asString(), dest->asString());
  rename_file(src, dest);
}

CL_LAMBDA(pathspec);
CL_DECLARE();
CL_DOCSTRING(R"dx(Look for <path> and return it. If it doesn't exist create every missing directory along the path.)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__ensure_directories_exist(T_sp pathspec) {
  Path_sp path_to_create;
  if (cl__stringp(pathspec)) {
    path_to_create = Path_O::create(gc::As_unsafe<String_sp>(pathspec)->get_std_string());
  } else if (Pathname_sp pn = pathspec.asOrNull<Pathname_O>()) {
    String_sp spn = gc::As<String_sp>(cl__namestring(pn));
    path_to_create = Path_O::create(spn->get_std_string());
  } else {
    TYPE_ERROR(pathspec, core::Cons_O::createList(cl::_sym_or, cl::_sym_string, cl::_sym_pathname));
  }
  std::filesystem::path parent = path_to_create->getPath().parent_path();
  try {
    std::filesystem::create_directories(parent);
  } catch (...) {
    SIMPLE_ERROR("Could not create_directories for path[{}]", parent.string());
  }
  return (Values(pathspec, _lisp->_true()));
}

Path_O::Path_O(Path_O const& other) : Base(other) { this->_Path = other._Path; }

void Path_O::initialize() { this->Base::initialize(); }

Path_sp Path_O::create(const string& path) {
  auto op = gctools::GC<Path_O>::allocate_with_default_constructor();
  op->setPathFromString(path);
  return op;
}

Path_sp Path_O::create(std::filesystem::path p) {
  auto op = gctools::GC<Path_O>::allocate_with_default_constructor();
  op->setPath(p);
  return op;
}

/*
  __BEGIN_DOC(classes.Path.!class.Path)
  \optionalKeyed{path:}{Text::path}

  Create a Path object that maintains a system independant path to a file in the file system.
  __END_DOC
*/

#define ARGS_af_makePath "(&rest parts)"
#define DECL_af_makePath ""
#define DOCS_af_makePath "make Path args: path"
Path_mv af_makePath(List_sp args) {
  Path_sp me(Path_O::create());
  while (args.notnilp()) {
    me->path_append(gc::As<String_sp>(oCar(args))->get_std_string());
    args = oCdr(args);
  }
  return (Values(me));
}

void Path_O::sxhash_equal(HashGenerator& hg) const {

  string ts = this->_Path._value.string();
  for (char const& c : ts)
    if (!(hg.addValue(c)))
      return;
}

#if 0
CL_LISPIFY_NAME("last_write_time");
CL_DEFMETHOD Integer_sp Path_O::last_write_time() const {
  auto ftime = std::filesystem::last_write_time(this->_Path._value);
  std::time_t cftime = std::chrono::system_clock::to_time_t(
                 std::chrono::file_clock::to_sys(ftime));    
  gc::Fixnum ui64 = cftime;
  return Integer_O::create(ui64);
}
#endif

CL_LISPIFY_NAME("path-append");
CL_DEFMETHOD Path_sp Path_O::path_append(string const& pp) {

  LOG("Appending string[{}] to the path", pp);
  this->_Path._value /= pp;
  return this->sharedThis<Path_O>();
}

void Path_O::setPath(const std::filesystem::path& path) { this->_Path = path; }

CL_LISPIFY_NAME("path-absolute");
CL_DEFMETHOD Path_sp Path_O::absolute() const {
  if (this->_Path._value.is_absolute())
    return this->copyPath();
  auto abs = gctools::GC<Path_O>::allocate_with_default_constructor();
  abs->_Path = std::filesystem::absolute(this->_Path._value);
  return abs;
}

CL_LISPIFY_NAME("copyPath");
CL_DEFMETHOD Path_sp Path_O::copyPath() const {

  auto copy = gctools::GC<Path_O>::copy(*this);
  return copy;
}

CL_LISPIFY_NAME("setPathFromString");
CL_DEFMETHOD void Path_O::setPathFromString(const string& pth) {
  std::filesystem::path p(pth);
  this->_Path._value = p;
}

CL_LAMBDA(self);
CL_DOCSTRING(R"dx(Returns a list of path parts as strings)dx");
CL_LISPIFY_NAME("path-parts");
CL_DEFMETHOD List_sp Path_O::parts() const {
  std::filesystem::path::iterator it;
  ql::list l;
  for (it = this->_Path._value.begin(); it != this->_Path._value.end(); ++it) {
    l << SimpleBaseString_O::make(it->native());
  }
  return l.cons();
}

CL_LISPIFY_NAME("path-asString");
CL_DEFMETHOD string Path_O::asString() const { return this->_Path._value.string(); }

string Path_O::__repr__() const {

  stringstream ss;
  ss << "#<" << _rep_(this->_instanceClass()->_className()) << " :string ";
  ss << this->_Path._value.string() << ">";
  return ss.str();
}

CL_LISPIFY_NAME("path-stem");
CL_DEFMETHOD string Path_O::stem() { return this->_Path._value.stem().string(); }

CL_LISPIFY_NAME("extension");
CL_DEFMETHOD string Path_O::extension() { return this->_Path._value.extension().string(); }

void Path_O::appendToExtension(string const& str) {

  stringstream newExtension;
  newExtension << this->extension() << str;
  this->replaceExtension(newExtension.str());
}

CL_LISPIFY_NAME("replaceExtension");
CL_DEFMETHOD Path_sp Path_O::replaceExtension(string const& str) {

  //	std::filesystem::path newExt(str);
  this->_Path._value.replace_extension(str);
  return this->sharedThis<Path_O>();
}

CL_LISPIFY_NAME("parent_path");
CL_DEFMETHOD Path_sp Path_O::parent_path() { return Path_O::create(this->_Path._value.parent_path()); }

CL_LISPIFY_NAME("path-fileName");
CL_DEFMETHOD string Path_O::fileName() const { return this->_Path._value.filename().string(); }

CL_LISPIFY_NAME("exists");
CL_DEFMETHOD bool Path_O::exists() { return std::filesystem::exists(this->_Path._value); }

DirectoryIterator_sp DirectoryIterator_O::create(Path_sp path) {
  auto di = gctools::GC<DirectoryIterator_O>::allocate_with_default_constructor();
  di->setPath(path);
  return di;
}

#define ARGS_af_makeDirectoryIterator "(path)"
#define DECL_af_makeDirectoryIterator ""
#define DOCS_af_makeDirectoryIterator "make DirectoryIterator args: path"
DirectoryIterator_mv af_makeDirectoryIterator(Path_sp path) {
  IMPLEMENT_MEF("What the heck was I doing below?");
#if 0
	DirectoryIterator_sp me(DirectoryIterator_O::create());
	SYMBOL_SC_(CorePkg,path);
	GlueEnvironment_sp env(GlueEnvironment_O::create((ql::list << _sym_path << path ).cons()));
	me->make_init(nil<core::Function_O>(),env->args(),env);
	return(Values(me));
#endif
}

void DirectoryIterator_O::initialize() {
  this->Base::initialize();
  this->_CurrentIterator._value = NULL;
}

void DirectoryIterator_O::setPath(Path_sp p) { this->_Path = p; }

void DirectoryIterator_O::setupCurrentIterator() {

  ASSERTNOTNULL(this->_Path);
  ASSERT(this->_Path.notnilp());
  delete (this->_CurrentIterator._value);
  try {
    this->_CurrentIterator._value = new std::filesystem::directory_iterator(this->_Path->getPath());
  } catch (std::filesystem::filesystem_error& err) {
    SIMPLE_ERROR("{}", err.what());
  }
}

void DirectoryIterator_O::first() { this->setupCurrentIterator(); }

void DirectoryIterator_O::next() {

  ASSERTF(this->_CurrentIterator._value != NULL, "The _CurrentIterator is NULL - it shouldn't be");
  (*(this->_CurrentIterator._value))++;
}

bool DirectoryIterator_O::isDone() {

  ASSERTF(this->_CurrentIterator._value != NULL, "The _CurrentIterator._value is NULL - it shouldn't be");
  return (*(this->_CurrentIterator._value) == this->_EndIterator._value);
}

T_sp DirectoryIterator_O::currentObject() {

  ASSERTF(this->_CurrentIterator._value != NULL, "The _CurrentIterator._value is NULL - it shouldn't be");
  if (this->isDone()) {
    LOG("The directory iteratory is done - returning nil");
    return nil<DirectoryEntry_O>();
  }
  LOG("Returning the next directory entry");
  DirectoryEntry_sp de = _lisp->create<DirectoryEntry_O>();
  de->setEntry(**(this->_CurrentIterator._value));
  return de;
}

DirectoryIterator_O::~DirectoryIterator_O() { delete this->_CurrentIterator._value; }

RecursiveDirectoryIterator_sp RecursiveDirectoryIterator_O::create(Path_sp path) {
  auto di = gctools::GC<RecursiveDirectoryIterator_O>::allocate_with_default_constructor();
  di->setPath(path);
  return di;
}

void RecursiveDirectoryIterator_O::initialize() {
  this->Base::initialize();
  this->_CurrentIterator._value = NULL;
  this->_EnterHidden = false;
}

void RecursiveDirectoryIterator_O::setPath(Path_sp p) { this->_Path = p; }

void RecursiveDirectoryIterator_O::setupCurrentIterator() {

  ASSERTNOTNULL(this->_Path);
  ASSERT(this->_Path.notnilp());
  delete (this->_CurrentIterator._value);
  try {
    this->_CurrentIterator._value = new std::filesystem::recursive_directory_iterator(this->_Path->getPath());
  } catch (std::filesystem::filesystem_error& err) {
    SIMPLE_ERROR("{}", err.what());
  }
}

void RecursiveDirectoryIterator_O::first() { this->setupCurrentIterator(); }

void RecursiveDirectoryIterator_O::next() {

  ASSERTF(this->_CurrentIterator._value != NULL, "The _CurrentIterator._value is NULL - it shouldn't be");
  (*(this->_CurrentIterator._value))++;
}

bool RecursiveDirectoryIterator_O::isDone() {

  ASSERTF(this->_CurrentIterator._value != NULL, "The _CurrentIterator._value is NULL - it shouldn't be");
  return (*(this->_CurrentIterator._value) == this->_EndIterator._value);
}

T_sp RecursiveDirectoryIterator_O::currentObject() {

  ASSERTF(this->_CurrentIterator._value != NULL, "The _CurrentIterator._value is NULL - it shouldn't be");
  if (this->isDone()) {
    LOG("The directory iteratory is done - returning nil");
    return nil<DirectoryEntry_O>();
  }
  LOG("Returning the next directory entry");
  DirectoryEntry_sp de = _lisp->create<DirectoryEntry_O>();
  de->setEntry(**(this->_CurrentIterator._value));
  return de;
}

RecursiveDirectoryIterator_O::~RecursiveDirectoryIterator_O() { delete this->_CurrentIterator._value; }

void DirectoryEntry_O::initialize() { this->Base::initialize(); }

void DirectoryEntry_O::setEntry(const std::filesystem::directory_entry& entry) {
  delete this->_Entry;
  this->_Entry = new std::filesystem::directory_entry(entry);
}

CL_LISPIFY_NAME("fileStatus");
CL_DEFMETHOD FileStatus_sp DirectoryEntry_O::fileStatus() {
  FileStatus_sp fs = _lisp->create<FileStatus_O>();
  fs->setFileStatus(this->_Entry->status());
  return fs;
}

CL_LISPIFY_NAME("symlinkStatus");
CL_DEFMETHOD FileStatus_sp DirectoryEntry_O::symlinkStatus() {

  FileStatus_sp fs = _lisp->create<FileStatus_O>();
  fs->setFileStatus(this->_Entry->symlink_status());
  return fs;
}

CL_LISPIFY_NAME("path");
CL_DEFMETHOD Path_sp DirectoryEntry_O::path() {

  Path_sp path = _lisp->create<Path_O>();
  path->setPath(this->_Entry->path());
  return path;
}

DirectoryEntry_O::~DirectoryEntry_O() { delete this->_Entry; }

void FileStatus_O::initialize() { this->Base::initialize(); }

void FileStatus_O::setFileStatus(const std::filesystem::file_status& fs) { this->_FileStatus = fs; }

CL_LISPIFY_NAME("exists");
CL_DEFMETHOD bool FileStatus_O::exists() { return std::filesystem::exists(this->_FileStatus); }
CL_LISPIFY_NAME("isRegularFile");
CL_DEFMETHOD bool FileStatus_O::isRegularFile() {

  try {
    return std::filesystem::is_regular_file(this->_FileStatus);
  } catch (...) {
    SIMPLE_ERROR("In {} std::filesystem signaled a c++ exception", __FUNCTION__);
  }
}

CL_LISPIFY_NAME("isDirectory");
CL_DEFMETHOD bool FileStatus_O::isDirectory() {

  try {
    return std::filesystem::is_directory(this->_FileStatus);
  } catch (...) {
    SIMPLE_ERROR("In {} std::filesystem signaled a c++ exception", __FUNCTION__);
  }
}
CL_LISPIFY_NAME("isSymlink");
CL_DEFMETHOD bool FileStatus_O::isSymlink() {

  try {
    return std::filesystem::is_symlink(this->_FileStatus);
  } catch (...) {
    SIMPLE_ERROR("In {} std::filesystem signaled a c++ exception", __FUNCTION__);
  }
}
CL_LISPIFY_NAME("isOther");
CL_DEFMETHOD bool FileStatus_O::isOther() {

  try {
    return std::filesystem::is_other(this->_FileStatus);
  } catch (...) {
    SIMPLE_ERROR("In {} std::filesystem signaled a c++ exception", __FUNCTION__);
  }
}

Pathname_sp getcwd(bool change_d_p_d) {
  Str8Ns_sp namestring = ext::ext__getcwd();
  // This commented out code adds a directory delimiter if none is there yet.
  // Not currently required because ext::ext__getcwd already does it.
  // However, ext::ext__getcwd() shouldn't, so leave this here to re-activate
  // on cleanup.
  // if (!IS_DIR_SEPARATOR(clasp_as_claspCharacter(namestring->rowMajorAref(i - 1))))
  //  namestring = SimpleBaseString_O::make(namestring->get() + DIR_SEPARATOR);
  Pathname_sp pathname = gc::As<Pathname_sp>(cl__parse_namestring(namestring));
  if (change_d_p_d && pathname.notnilp()) {
    cl::_sym_STARdefaultPathnameDefaultsSTAR->setf_symbolValue(pathname);
  }
  return pathname;
}

/*! Translated from ecl>homedir_pathname */
Pathname_sp homedirPathname(T_sp tuser) {
  int i;
  SimpleBaseString_sp namestring;
  const char* h;
  if (cl__stringp(tuser)) {
    String_sp user = gc::As_unsafe<String_sp>(tuser);
#ifdef HAVE_PWD_H
    struct passwd* pwent = NULL;
#endif
    std::string users = user->get_std_string();
    const char* p = users.c_str();
    i = user->length();
    if (i > 0 && *p == '~') {
      p++;
      i--;
    }
    if (i == 0)
      return homedirPathname(nil<T_O>());
#ifdef HAVE_PWD_H
    pwent = getpwnam(p);
    if (pwent == NULL) {
      SIMPLE_ERROR("Unknown user {}.", p);
    }
    namestring = SimpleBaseString_O::make(pwent->pw_dir);
#endif
    SIMPLE_ERROR("Unknown user {}.", p);
  } else if ((h = getenv("HOME"))) {
    namestring = SimpleBaseString_O::make(h);
#if 0 // defined(CLASP_MS_WINDOWS_HOST)
	} else if ((h = getenv("HOMEPATH")) && (d = getenv("HOMEDRIVE"))) {
	    namestring =
		si_base_string_concatenate(2,
					   make_constant_base_string(d),
					   make_constant_base_string(h));
#endif
  } else {
    namestring = SimpleBaseString_O::make("/");
  }
  if (namestring[0] == '~') {
    SIMPLE_ERROR("Not a valid home pathname {}", _rep_(namestring));
  }
  i = namestring->length();
  if (!IS_DIR_SEPARATOR(namestring->rowMajorAref(i - 1).unsafe_character()))
    namestring = SimpleBaseString_O::make(namestring->get_std_string() + DIR_SEPARATOR);
  return gc::As<Pathname_sp>(cl__parse_namestring(namestring));
}
}; // namespace core
