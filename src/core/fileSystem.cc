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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>

#include <sys/types.h>
#include <pwd.h>
#ifdef _TARGET_OS_LINUX
#else
#include <uuid/uuid.h>
#endif

#include <clasp/core/corePackage.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/ql.h>
#include <clasp/core/useBoostRegex.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/environment.h>
#include <clasp/core/pathname.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/str.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/wrappers.h>
#include <string>

namespace bf = boost_filesystem;

namespace boost {
void assertion_failed_msg(char const *expr, char const *msg,
                          char const *function, char const *file, long line) {
  THROW_HARD_ERROR(BF("boost::assertion_failed_msg was called with expr[%s] msg[%s] function[%s] file[%s] line[%d]") % expr % msg % function % file % line);
}
} // namespace boost

namespace core {

/*! Return a list of Path objects representing the contents of the directory
  that match the fileNameRegex */
List_sp directory(Path_sp rpath, const string &fileNameRegex) {
  bf::path p(rpath->getPath());
  Cons_sp first;
  List_sp tail;
  Str_sp fileName;
  boost::regex ex(fileNameRegex);
  first = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  tail = first;
  bf::directory_iterator end_iter;
  for (bf::directory_iterator itr(p); itr != end_iter; itr++) {
    // the fileName is all we match to
    string testName = itr->path().filename().string();
    LOG(BF("Checking name: %s") % testName.c_str());
    if (regex_match(testName, ex)) {
      LOG(BF("It matches"));
      tail.asCons()->setCdr(Cons_O::create(Path_O::create(itr->path()),
                                           _Nil<T_O>()));
      tail = oCdr(tail);
    } else {
      LOG(BF("It doesnt match"));
    }
  }
  return oCdr(first);
}

void rename_file(Path_sp rpath1, Path_sp rpath2) {
  return bf::rename(rpath1->getPath(), rpath2->getPath());
}

bool delete_file(Path_sp rpath) {
  return bf::remove(rpath->getPath());
}

int delete_all_files(Path_sp rpath) {
  return bf::remove_all(rpath->getPath());
}

/*! Rename src to dest but first check if dest already exists and create a backup
      of it.  If the backup already exists then it is deleted */
void safeRename(Path_sp src, Path_sp dest) {
  // If the OriginalFilePath exists then create a backup
  if (dest->exists()) {
    LOG(BF("destination file[%s] exists") % dest->asString());
    Path_sp destBackup = dest->copyPath();
    destBackup->appendToExtension("Backup");
    if (destBackup->exists()) {
      LOG(BF("destination backup file[%s] exists - removing it") % destBackup->asString());
      delete_file(destBackup);
    }
    LOG(BF("Renaming dest[%s] to destBackup[%s]") % dest->asString() % destBackup->asString());
    rename_file(dest, destBackup);
  }
  LOG(BF("Renaming src[%s] to dest[%s]") % src->asString() % dest->asString());
  rename_file(src, dest);
}

CL_LAMBDA(pathspec);
CL_DECLARE();
CL_DOCSTRING("Look for <path> and return it. If it doesn't exist create every missing directory along the path.");
CL_DEFUN T_mv cl__ensure_directories_exist(T_sp pathspec) {
  Path_sp path_to_create = coerce::pathDesignator(pathspec);
  bf::path parent = path_to_create->getPath().parent_path();
  try {
    bf::create_directories(parent);
  } catch (...) {
    SIMPLE_ERROR(BF("Could not create_directories for path[%s]") % parent.string());
  }
  return (Values(pathspec, _lisp->_true()));
}

Path_O::Path_O(Path_O const &other) : Base(other) {
  this->_Path = other._Path;
}

void Path_O::initialize() {
  this->Base::initialize();
}

Path_sp Path_O::create(const string &path) {
  GC_ALLOCATE(Path_O, op);
  op->setPathFromString(path);
  return op;
}

Path_sp Path_O::create(boost_filesystem::path p) {
  GC_ALLOCATE(Path_O, op);
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
    me->path_append(gc::As<Str_sp>(oCar(args))->get());
    args = oCdr(args);
  }
  return (Values(me));
}

bool Path_O::lt(T_sp obj) const {
  if (!gc::IsA<Path_sp>(obj)) {
    return this->Base::operator<(obj);
  }
  Path_sp other = gc::As<Path_sp>(obj);
  return this->asString() < other->asString();
}

Path_O &Path_O::operator/=(string const &pp) {
  this->_Path /= pp;
  return *this;
}

void Path_O::sxhash_(HashGenerator &hg) const {
  _OF();
  string ts = this->_Path.string();
  Bignum bn = Str_O::stringToBignum(ts.c_str());
  hg.addPart(bn);
}

CL_LISPIFY_NAME("last_write_time");
CL_DEFMETHOD Integer_sp Path_O::last_write_time() const {
  std::time_t ttime = boost_filesystem::last_write_time(this->_Path);
  gc::Fixnum ui64 = ttime;
  return Integer_O::create(ui64);
}

CL_LISPIFY_NAME("path-append");
CL_DEFMETHOD Path_sp Path_O::path_append(string const &pp) {
  _OF();
  LOG(BF("Appending string[%s] to the path") % pp);
  this->_Path /= pp;
  return this->sharedThis<Path_O>();
}

#if defined(XML_ARCHIVE)
void Path_O::archiveBase(ArchiveP node) {
  if (node->loading()) {
    VectorStrings vec;
    this->_Path.clear();
    node->archiveVectorStrings("parts", vec);
    for (VectorStrings::iterator si = vec.begin(); si != vec.end(); si++) {
      this->_Path /= *si;
    }
  } else {
    VectorStrings vec;
    for (boost_filesystem::path::iterator pi = this->_Path.begin();
         pi != this->_Path.end(); pi++) {
      vec.push_back((*pi).string());
    }
    node->archiveVectorStrings("parts", vec);
  }
}
#endif // defined(XML_ARCHIVE)

void Path_O::setPath(const boost_filesystem::path &path) {
  _OF();
  this->_Path = path;
}

CL_LISPIFY_NAME("path-absolute");
CL_DEFMETHOD Path_sp Path_O::absolute() const {
  if (this->_Path.is_absolute())
    return this->copyPath();
  GC_ALLOCATE(Path_O, abs);
  abs->_Path = boost_filesystem::absolute(this->_Path);
  return abs;
}

CL_LISPIFY_NAME("copyPath");
CL_DEFMETHOD Path_sp Path_O::copyPath() const {
  _OF();
  GC_COPY(Path_O, copy, *this);
  return copy;
}

CL_LISPIFY_NAME("setPathFromString");
CL_DEFMETHOD void Path_O::setPathFromString(const string &pth) {
  bf::path p(pth);
  this->_Path = p;
}

CL_LAMBDA(self);
CL_DOCSTRING("Returns a list of path parts as strings");
CL_LISPIFY_NAME("path-parts");
CL_DEFMETHOD List_sp Path_O::parts() const {
  bf::path::iterator it;
  ql::list l(_lisp);
  for (it = this->_Path.begin(); it != this->_Path.end(); ++it) {
    l << Str_O::create(it->native());
  }
  return l.cons();
}

CL_LISPIFY_NAME("asString");
CL_DEFMETHOD string Path_O::asString() const {
  _OF();
  return this->_Path.string();
}

string Path_O::__repr__() const {
  _OF();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " :string ";
  ss << this->_Path.string() << ">";
  return ss.str();
}

CL_LISPIFY_NAME("path-stem");
CL_DEFMETHOD string Path_O::stem() {
  return this->_Path.stem().string();
}

CL_LISPIFY_NAME("extension");
CL_DEFMETHOD string Path_O::extension() {
  return this->_Path.extension().string();
}

void Path_O::appendToExtension(string const &str) {
  _OF();
  stringstream newExtension;
  newExtension << this->extension() << str;
  this->replaceExtension(newExtension.str());
}

CL_LISPIFY_NAME("replaceExtension");
CL_DEFMETHOD Path_sp Path_O::replaceExtension(string const &str) {
  _OF();
  //	bf::path newExt(str);
  this->_Path.replace_extension(str);
  return this->sharedThis<Path_O>();
}

CL_LISPIFY_NAME("parent_path");
CL_DEFMETHOD Path_sp Path_O::parent_path() {
  _OF();
  return Path_O::create(this->_Path.parent_path());
}

CL_LISPIFY_NAME("path-fileName");
CL_DEFMETHOD string Path_O::fileName() const {
  return this->_Path.filename().string();
}

CL_LISPIFY_NAME("exists");
CL_DEFMETHOD bool Path_O::exists() {
  return boost_filesystem::exists(this->_Path);
}






DirectoryIterator_sp DirectoryIterator_O::create(Path_sp path) {
  GC_ALLOCATE(DirectoryIterator_O, di);
  di->setPath(path);
  return di;
}






#if 0
    T_sp DirectoryIterator_O::make_init(Function_sp exec, Cons_sp args, T_sp bargs)
    {
	Path_sp path = coerce::pathDesignator(af_interpreter_lookup_variable(_sym_path,bargs));
	if ( path.nilp() )
	{
	    SIMPLE_ERROR(BF("You must specify the path"));
	}
	this->setPath(path);
	this->first();
	return _Nil<T_O>();
    }
#endif

#define ARGS_af_makeDirectoryIterator "(path)"
#define DECL_af_makeDirectoryIterator ""
#define DOCS_af_makeDirectoryIterator "make DirectoryIterator args: path"
DirectoryIterator_mv af_makeDirectoryIterator(Path_sp path) {
  IMPLEMENT_MEF(BF("What the heck was I doing below?"));
#if 0
	DirectoryIterator_sp me(DirectoryIterator_O::create());
	SYMBOL_SC_(CorePkg,path);
	GlueEnvironment_sp env(GlueEnvironment_O::create((ql::list(_lisp) << _sym_path << path ).cons()));
	me->make_init(_Nil<core::Function_O>(),env->args(),env);
	return(Values(me));
#endif
}

void DirectoryIterator_O::initialize() {
  this->Base::initialize();
  this->_CurrentIterator = NULL;
}

void DirectoryIterator_O::setPath(Path_sp p) {
  _OF();
  this->_Path = p;
}

void DirectoryIterator_O::setupCurrentIterator() {
  _OF();
  ASSERTNOTNULL(this->_Path);
  ASSERT(this->_Path.notnilp());
  if (this->_CurrentIterator != NULL) {
    delete (this->_CurrentIterator);
  }
  TRY() {
    this->_CurrentIterator = new boost_filesystem::directory_iterator(this->_Path->getPath());
  }
  catch (boost_filesystem::filesystem_error &err) {
    SIMPLE_ERROR(BF("%s") % err.what());
  }
}

void DirectoryIterator_O::first() {
  _OF();
  this->setupCurrentIterator();
}

void DirectoryIterator_O::next() {
  _OF();
  ASSERTF(this->_CurrentIterator != NULL, BF("The _CurrentIterator is NULL - it shouldn't be"));
  (*(this->_CurrentIterator))++;
}

bool DirectoryIterator_O::isDone() {
  _OF();
  ASSERTF(this->_CurrentIterator != NULL, BF("The _CurrentIterator is NULL - it shouldn't be"));
  return (*(this->_CurrentIterator) == this->_EndIterator);
}

T_sp DirectoryIterator_O::currentObject() {
  _OF();
  ASSERTF(this->_CurrentIterator != NULL, BF("The _CurrentIterator is NULL - it shouldn't be"));
  if (this->isDone()) {
    LOG(BF("The directory iteratory is done - returning nil"));
    return _Nil<DirectoryEntry_O>();
  }
  LOG(BF("Returning the next directory entry"));
  DirectoryEntry_sp de = _lisp->create<DirectoryEntry_O>();
  de->setEntry(**(this->_CurrentIterator));
  return de;
}

DirectoryIterator_O::~DirectoryIterator_O() {
  if (this->_CurrentIterator != NULL) {
    delete this->_CurrentIterator;
  }
}

RecursiveDirectoryIterator_sp RecursiveDirectoryIterator_O::create(Path_sp path) {
  GC_ALLOCATE(RecursiveDirectoryIterator_O, di);
  di->setPath(path);
  return di;
}






#if 0
#define ARGS_af_makeRecursiveDirectoryIterator "(path)"
#define DECL_af_makeRecursiveDirectoryIterator ""
#define DOCS_af_makeRecursiveDirectoryIterator "make RecursiveDirectoryIterator args: path"
    RecursiveDirectoryIterator_mv af_makeRecursiveDirectoryIterator(Path_sp path)
    {
	RecursiveDirectoryIterator_sp me(RecursiveDirectoryIterator_O::create());
	GlueEnvironment_sp env(GlueEnvironment_O::create((ql::list(_lisp) << _sym_path << path).cons()) );
	me->make_init__(core::_Nil<Function_O>(),env->args(),env,_lisp);
	return me;
    }


    T_sp RecursiveDirectoryIterator_O::make_init__(Function_sp exec, Cons_sp args, Environment_sp bargs, Lisp_sp lisp)
    {
	Path_sp path = coerce::pathDesignator(bargs->lookup(_sym_path));
	if ( path.nilp() )
	{
	    SIMPLE_ERROR(BF("You must specify the path"));
	}
	this->setPath(path);
	this->first();
	return _Nil<T_O>();
    }
#endif

void RecursiveDirectoryIterator_O::initialize() {
  this->Base::initialize();
  this->_CurrentIterator = NULL;
  this->_EnterHidden = false;
}

void RecursiveDirectoryIterator_O::setPath(Path_sp p) {
  _OF();
  this->_Path = p;
}

void RecursiveDirectoryIterator_O::setupCurrentIterator() {
  _OF();
  ASSERTNOTNULL(this->_Path);
  ASSERT(this->_Path.notnilp());
  if (this->_CurrentIterator != NULL) {
    delete (this->_CurrentIterator);
  }
  TRY() {
    this->_CurrentIterator = new boost_filesystem::recursive_directory_iterator(this->_Path->getPath());
  }
  catch (boost_filesystem::filesystem_error &err) {
    SIMPLE_ERROR(BF("%s") % err.what());
  }
}

void RecursiveDirectoryIterator_O::first() {
  _OF();
  this->setupCurrentIterator();
}

void RecursiveDirectoryIterator_O::next() {
  _OF();
  ASSERTF(this->_CurrentIterator != NULL, BF("The _CurrentIterator is NULL - it shouldn't be"));
  (*(this->_CurrentIterator))++;
}

bool RecursiveDirectoryIterator_O::isDone() {
  _OF();
  ASSERTF(this->_CurrentIterator != NULL, BF("The _CurrentIterator is NULL - it shouldn't be"));
  return (*(this->_CurrentIterator) == this->_EndIterator);
}

T_sp RecursiveDirectoryIterator_O::currentObject() {
  _OF();
  ASSERTF(this->_CurrentIterator != NULL, BF("The _CurrentIterator is NULL - it shouldn't be"));
  if (this->isDone()) {
    LOG(BF("The directory iteratory is done - returning nil"));
    return _Nil<DirectoryEntry_O>();
  }
  LOG(BF("Returning the next directory entry"));
  DirectoryEntry_sp de = _lisp->create<DirectoryEntry_O>();
  de->setEntry(**(this->_CurrentIterator));
  return de;
}

RecursiveDirectoryIterator_O::~RecursiveDirectoryIterator_O() {
  if (this->_CurrentIterator != NULL) {
    delete this->_CurrentIterator;
  }
}





void DirectoryEntry_O::initialize() {
  this->Base::initialize();
}

void DirectoryEntry_O::setEntry(const boost_filesystem::directory_entry &entry) {
  _OF();
  if (this->_Entry != NULL) {
    delete this->_Entry;
  }
  boost_filesystem::path p = entry.path();
  boost_filesystem::file_status s = entry.status();
  boost_filesystem::file_status ss = entry.symlink_status();
  this->_Entry = new boost_filesystem::directory_entry(p, s, ss);
}

CL_LISPIFY_NAME("fileStatus");
CL_DEFMETHOD FileStatus_sp DirectoryEntry_O::fileStatus() {
  _OF();
  FileStatus_sp fs = _lisp->create<FileStatus_O>();
  fs->setFileStatus(this->_Entry->status());
  return fs;
}

CL_LISPIFY_NAME("symlinkStatus");
CL_DEFMETHOD FileStatus_sp DirectoryEntry_O::symlinkStatus() {
  _OF();
  FileStatus_sp fs = _lisp->create<FileStatus_O>();
  fs->setFileStatus(this->_Entry->symlink_status());
  return fs;
}

CL_LISPIFY_NAME("path");
CL_DEFMETHOD Path_sp DirectoryEntry_O::path() {
  _OF();
  Path_sp path = _lisp->create<Path_O>();
  path->setPath(this->_Entry->path());
  return path;
}

DirectoryEntry_O::~DirectoryEntry_O() {
  if (this->_Entry != NULL)
    delete this->_Entry;
}





void FileStatus_O::initialize() {
  this->Base::initialize();
}

void FileStatus_O::setFileStatus(const boost_filesystem::file_status &fs) {
  _OF();
  this->_FileStatus = fs;
}

CL_LISPIFY_NAME("exists");
CL_DEFMETHOD bool FileStatus_O::exists() {
  _OF();
  return boost_filesystem::exists(this->_FileStatus);
}
CL_LISPIFY_NAME("isRegularFile");
CL_DEFMETHOD bool FileStatus_O::isRegularFile() {
  _OF();
  return boost_filesystem::is_regular_file(this->_FileStatus);
}
CL_LISPIFY_NAME("isDirectory");
CL_DEFMETHOD bool FileStatus_O::isDirectory() {
  _OF();
  return boost_filesystem::is_directory(this->_FileStatus);
}
CL_LISPIFY_NAME("isSymlink");
CL_DEFMETHOD bool FileStatus_O::isSymlink() {
  _OF();
  return boost_filesystem::is_symlink(this->_FileStatus);
}
CL_LISPIFY_NAME("isOther");
CL_DEFMETHOD bool FileStatus_O::isOther() {
  _OF();
  return boost_filesystem::is_other(this->_FileStatus);
}

Pathname_sp getcwd(bool change_d_p_d) {
  boost::filesystem::path cwd = boost::filesystem::current_path();
  Str_sp namestring = Str_O::create(cwd.string());
  size_t i = namestring->length();
  if (!IS_DIR_SEPARATOR(namestring->schar(i - 1)))
    namestring = Str_O::create(namestring->get() + DIR_SEPARATOR);
  Pathname_sp pathname = cl__parse_namestring(namestring);
  if (change_d_p_d) {
    cl::_sym_STARdefaultPathnameDefaultsSTAR->setf_symbolValue(pathname);
  }
  return pathname;
}

/*! Translated from ecl>homedir_pathname */
Pathname_sp homedirPathname(T_sp tuser) {
  int i;
  Str_sp namestring;
  const char *h;
  if (Str_sp user = tuser.asOrNull<Str_O>()) {
#ifdef HAVE_PWD_H
    struct passwd *pwent = NULL;
#endif
    const char *p = user->get().c_str();
    i = user->length();
    if (i > 0 && *p == '~') {
      p++;
      i--;
    }
    if (i == 0)
      return homedirPathname(_Nil<T_O>());
#ifdef HAVE_PWD_H
    pwent = getpwnam(p);
    if (pwent == NULL) {
      SIMPLE_ERROR(BF("Unknown user %s.") % p);
    }
    namestring = Str_O::create(pwent->pw_dir);
#endif
    SIMPLE_ERROR(BF("Unknown user %s.") % p);
  } else if ((h = getenv("HOME"))) {
    namestring = Str_O::create(h);
#if 0 //defined(CLASP_MS_WINDOWS_HOST)
	} else if ((h = getenv("HOMEPATH")) && (d = getenv("HOMEDRIVE"))) {
	    namestring =
		si_base_string_concatenate(2,
					   make_constant_base_string(d),
					   make_constant_base_string(h));
#endif
  } else {
    namestring = Str_O::create("/");
  }
  if ((*namestring)[0] == '~') {
    SIMPLE_ERROR(BF("Not a valid home pathname %s") % _rep_(namestring));
  }
  i = namestring->length();
  if (!IS_DIR_SEPARATOR(namestring->schar(i - 1)))
    namestring = Str_O::create(namestring->get() + DIR_SEPARATOR);
  return cl__parse_namestring(namestring);
}
};
