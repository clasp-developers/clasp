/*
    File: unixfsys.cc
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
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    unixfsys.c  -- Unix file system interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister.  - converted to C++


    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <stdio.h>
#include <limits.h>

#ifndef _MSC_VER
#include <unistd.h>
#else
#include <io.h>
#include <direct.h>
#define access _access
#define F_OK 0
typedef int mode_t;
#endif

#include <sys/types.h>

#include <pwd.h>

#ifdef _TARGET_OS_LINUX
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#else
#include <uuid/uuid.h>
#endif

#include <sys/stat.h>
#include <stdlib.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#else
#if !defined(_MSC_VER)
#include <sys/dir.h>
#endif
#endif
#if defined(ECL_MS_WINDOWS_HOST)
#include <windows.h>
#undef ERROR
#endif
#include <fcntl.h>
#include <errno.h>

#include <clasp/core/foundation.h>
#include <clasp/core/pathname.h>
#include <clasp/core/str.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/wrappers.h>

SYMBOL_EXPORT_SC_(KeywordPkg, absolute);
SYMBOL_EXPORT_SC_(KeywordPkg, default);
SYMBOL_EXPORT_SC_(KeywordPkg, defaults);
SYMBOL_EXPORT_SC_(KeywordPkg, directory);
SYMBOL_EXPORT_SC_(KeywordPkg, error);
SYMBOL_EXPORT_SC_(KeywordPkg, file);
SYMBOL_EXPORT_SC_(KeywordPkg, link);
SYMBOL_EXPORT_SC_(KeywordPkg, local);
SYMBOL_EXPORT_SC_(KeywordPkg, name);
SYMBOL_EXPORT_SC_(KeywordPkg, newest);
SYMBOL_EXPORT_SC_(KeywordPkg, pathname);
SYMBOL_EXPORT_SC_(KeywordPkg, relative);
SYMBOL_EXPORT_SC_(KeywordPkg, special);
SYMBOL_EXPORT_SC_(KeywordPkg, supersede);
SYMBOL_EXPORT_SC_(KeywordPkg, type);
SYMBOL_EXPORT_SC_(KeywordPkg, up);
SYMBOL_EXPORT_SC_(KeywordPkg, version);
SYMBOL_EXPORT_SC_(KeywordPkg, wild);

namespace core {

static Str_sp brcl_strerror(int e) {
  return Str_O::create(strerror(e));
}

static Str_sp
coerce_to_posix_filename(T_sp pathname) {
  /* This converts a pathname designator into a namestring, with the
	 * particularity that directories do not end with a slash '/', because
	 * this is not supported on all POSIX platforms (most notably Windows)
	 */
  ASSERT(pathname);
  Str_sp sfilename = af_coerceToFilename(pathname).as<Str_O>();
  return cl_stringRightTrim(Str_O::create(DIR_SEPARATOR), sfilename);
}

static int
safe_chdir(const char *path, Str_sp prefix) {
  if (prefix.notnilp()) {
    stringstream ss;
    ss << prefix.as<Str_O>()->get() << path;
    return safe_chdir(ss.str().c_str(), _Nil<Str_O>());
  } else {
    int output;
    brcl_disable_interrupts();
    output = chdir((char *)path);
    brcl_enable_interrupts();
    return output;
  }
}

#define ARGS_af_fork "()"
#define DECL_af_fork ""
#define DOCS_af_fork "fork"
T_sp af_fork() {
  _G();
  Fixnum_sp pid = Fixnum_O::create(fork());
  return pid;
};

#define ARGS_af_waitpid "(pid options)"
#define DECL_af_waitpid ""
#define DOCS_af_waitpid "waitpid - see unix waitpid - returns status"
int af_waitpid(Fixnum_sp pid, Fixnum_sp options) {
  _G();
  pid_t p = pid->get();
  int status(0);
  int iopts = options->get();
  waitpid(p, &status, iopts);
  return status;
};

#define ARGS_af_getpid "()"
#define DECL_af_getpid ""
#define DOCS_af_getpid "getpid"
T_sp af_getpid() {
  _G();
  Fixnum_sp pid = Fixnum_O::create(getpid());
  return pid;
};

#define ARGS_af_getppid "()"
#define DECL_af_getppid ""
#define DOCS_af_getppid "getppid"
T_sp af_getppid() {
  _G();
  Fixnum_sp pid = Fixnum_O::create(getppid());
  return pid;
};

#define ARGS_af_chdir "(pathname)"
#define DECL_af_chdir ""
#define DOCS_af_chdir "chdir"
T_sp af_chdir(Pathname_sp dir) {
  _G();
  Str_sp sdir = brcl_namestring(dir, true);
  return Integer_O::create(safe_chdir(sdir->get().c_str(), _Nil<Str_O>()));
};

static int
safe_stat(const char *path, struct stat *sb) {
  int output;
  brcl_disable_interrupts();
  output = stat(path, sb);
  brcl_enable_interrupts();
  return output;
}

#ifdef HAVE_LSTAT
static int
safe_lstat(const char *path, struct stat *sb) {
  int output;
  brcl_disable_interrupts();
  output = lstat(path, sb);
  brcl_enable_interrupts();
  return output;
}
#endif

#if 0
/*
 * string_to_pathanme, to be used when s is a real pathname
 */
T_sp
ecl_cstring_to_pathname(char *s)
{
	T_sp string = ecl_make_simple_base_string(s, -1);
	return cl_parse_namestring(1, string);
}
#endif

/*
 * Finds current directory by using getcwd() with an adjustable
 * string which grows until it can host the whole path.
 */

#define ARGS_af_currentDir "()"
#define DECL_af_currentDir ""
#define DOCS_af_currentDir "currentDir"
Str_sp af_currentDir() {
  _G();
  const char *ok;
  size_t size = 128;
  StrWithFillPtr_sp output(StrWithFillPtr_O::create(' ', 1, 0, true));
  do {
    output->setSize(size);
    brcl_disable_interrupts();
    ok = ::getcwd((char *)output->addressOfBuffer(), size - 1);
    brcl_enable_interrupts();
    size += 256;
  } while (ok == NULL);
  size = strlen((char *)output->addressOfBuffer());
  if ((size + 1 /* / */ + 1 /* 0 */) >= output->size()) {
    /* Too large to host the trailing '/' */
    output->adjustSize(2);
  }
#ifdef _MSC_VER
  for (c = output->base_string.self; *c; c++)
    if (*c == '\\')
      *c = '/';
#endif
  if ((*output)[size - 1] != DIR_SEPARATOR_CHAR) {
    (*output)[size++] = DIR_SEPARATOR_CHAR;
    (*output)[size] = 0;
  }
  output->setFillPointer(size);
  return output;
}

/*
 * Using a certain path, guess the type of the object it points to.
 */

static Symbol_sp
file_kind(char *filename, bool follow_links) {
  Symbol_sp output;
  struct stat buf;
#ifdef HAVE_LSTAT
  if ((follow_links ? safe_stat : safe_lstat)(filename, &buf) < 0)
#else
  if (safe_stat(filename, &buf) < 0)
#endif
    output = _Nil<Symbol_O>();
#ifdef HAVE_LSTAT
  else if (S_ISLNK(buf.st_mode))
    output = kw::_sym_link;
#endif
  else if (S_ISDIR(buf.st_mode))
    output = kw::_sym_directory;
  else if (S_ISREG(buf.st_mode))
    output = kw::_sym_file;
  else
    output = kw::_sym_special;
  return output;
}

#define ARGS_af_file_kind "(filename follow-links)"
#define DECL_af_file_kind ""
#define DOCS_af_file_kind "file_kind"
Symbol_sp af_file_kind(T_sp filename, bool follow_links) {
  _G();
  ASSERT(filename);
  Str_sp sfilename = coerce_to_posix_filename(filename);
  return file_kind((char *)(sfilename->c_str()), follow_links);
}

#if defined(HAVE_LSTAT) && !defined(ECL_MS_WINDOWS_HOST)
static T_sp
si_readlink(Str_sp filename) {
  /* Given a filename which is a symlink, this routine returns
	 * the value of this link in the form of a pathname. */
  size_t size = 128, written;
  StrWithFillPtr_sp output;
  Symbol_sp kind;
  do {
    output = StrWithFillPtr_O::create(' ', size, 0, true);
    brcl_disable_interrupts();
    written = readlink((char *)filename->c_str(),
                       (char *)output->c_str(), size);
    brcl_enable_interrupts();
    size += 256;
  } while (written == size);
  (*output)[written] = '\0';
  kind = file_kind((char *)output->c_str(), false);
  if (kind == kw::_sym_directory) {
    (*output)[written++] = DIR_SEPARATOR_CHAR;
    (*output)[written] = '\0';
  }
  output->setFillPointer(written);
  return output;
}
#endif /* HAVE_LSTAT */

static Pathname_sp
enter_directory(Pathname_sp base_dir, T_sp subdir, bool ignore_if_failure) {
  /* Assuming we start in "base_dir", enter a subdirectory named by
 * "subdir", which may be a string, :UP, :ABSOLUTE or :RELATIVE.
 * If the operation succeeds, return the truename of the resulting
 * path -- resolving any links in the process. */
  Str_sp aux;
  Pathname_sp output;
  Symbol_sp kind;
  if (subdir == kw::_sym_absolute) {
    return eval::funcall(cl::_sym_makePathname,
                         kw::_sym_directory, Cons_O::createList(subdir),
                         kw::_sym_defaults, base_dir).as<Pathname_O>();
  } else if (subdir == kw::_sym_relative) {
    /* Nothing to do */
    return base_dir;
  } else if (subdir == kw::_sym_up) {
    aux = Str_O::create("..");
  } else if (!af_stringP(subdir)) {
    SIMPLE_ERROR(BF("Directory component %s found in pathname %s"
                    "is not allowed in TRUENAME or DIRECTORY") %
                 _rep_(subdir) % _rep_(base_dir));
  } else {
    aux = subdir.as<Str_O>();
  }
  /* We now compose a new path based on the base directory and
 * the new component. We have to verify that the new pathname is
 * a directory and if it is a link recover the true name. */
  T_sp ldir = Cons_O::append(base_dir->_Directory, Cons_O::createList(aux));
  output = eval::funcall(cl::_sym_makePathname,
                         kw::_sym_directory, ldir,
                         kw::_sym_defaults, base_dir).as<Pathname_O>();
  aux = brcl_namestring(output, BRCL_NAMESTRING_FORCE_BASE_STRING);
  aux = Str_O::create(aux->substr(0, aux->length() - 1));
  //    aux->_contents()[aux->base_string.fillp-1] = 0;
  kind = file_kind((char *)aux->c_str(), false);
  if (kind.nilp()) {
    if (ignore_if_failure)
      return _Nil<Pathname_O>();
    CANNOT_OPEN_FILE_ERROR(aux);
//	FEcannot_open(aux);
#ifdef HAVE_LSTAT
  } else if (kind == kw::_sym_link) {
    output = af_truename(af_mergePathnames(si_readlink(aux),
                                           base_dir, kw::_sym_default));
    if (output->_Name.notnilp() ||
        output->_Type.notnilp())
      goto WRONG_DIR;
    return output;
#endif
  } else if (kind != kw::_sym_directory) {
  WRONG_DIR:
    if (ignore_if_failure)
      return _Nil<Pathname_O>();
    SIMPLE_ERROR(BF("The directory %s in pathname %s actually points to a file or special device.") % _rep_(subdir) % _rep_(base_dir));
  }
  if (subdir == kw::_sym_up) {
    T_sp newdir = output->_Directory;
    newdir = cl_nbutlast(newdir, Fixnum_O::create(2));
    if (Null(newdir)) {
      if (ignore_if_failure)
        return _Nil<Pathname_O>();
      SIMPLE_ERROR(BF("Pathname contained an :UP component  "
                      "that goes above the base directory:"
                      "%s") %
                   _rep_(output));
    }
    output->_Directory = newdir;
  }
  return output;
}

static Pathname_sp
make_absolute_pathname(T_sp orig_pathname) {
  Pathname_sp base_dir = getcwd(false);
  Pathname_sp pathname = af_coerceToFilePathname(orig_pathname);
  Pathname_sp result = brcl_mergePathnames(pathname, base_dir, kw::_sym_default);
  return result;
}

static Pathname_sp
make_base_pathname(Pathname_sp pathname) {
  return Pathname_O::makePathname(pathname->_Host,                       // host
                                  pathname->_Device,                     // device
                                  Cons_O::createList(kw::_sym_absolute), // dir
                                  _Nil<T_O>(),                           // name
                                  _Nil<T_O>(),                           // type
                                  _Nil<T_O>(),                           // version
                                  kw::_sym_local);
}

#define FOLLOW_SYMLINKS 1

static Pathname_mv
file_truename(Pathname_sp pathname, Str_sp filename, int flags) {
  Symbol_sp kind;
  if (Null(pathname)) {
    if (Null(filename)) {
      INTERNAL_ERROR(BF("file_truename:"
                        " both FILENAME and PATHNAME are null!"));
    }
    pathname = cl_pathname(filename);
  } else if (Null(filename)) {
    filename = brcl_namestring(pathname, BRCL_NAMESTRING_FORCE_BASE_STRING);
    if (Null(filename)) {
      SIMPLE_ERROR(BF("Unprintable pathname %s found in TRUENAME") % _rep_(pathname));
    }
  }
  kind = file_kind((char *)filename->c_str(), false);
  if (kind.nilp()) {
    CANNOT_OPEN_FILE_ERROR(filename);
#ifdef HAVE_LSTAT
  } else if (kind == kw::_sym_link && (flags & FOLLOW_SYMLINKS)) {
    /* The link might be a relative pathname. In that case we have
	 * to merge with the original pathname */
    T_sp tfilename = si_readlink(filename);
    pathname = Pathname_O::makePathname(pathname->_Host,
                                        pathname->_Device,
                                        pathname->_Directory,
                                        _Nil<T_O>(),
                                        _Nil<T_O>(),
                                        _Nil<T_O>(),
                                        kw::_sym_local);
    pathname = brcl_mergePathnames(tfilename, pathname, kw::_sym_default);
    return Values(af_truename(pathname), kind);
#endif
  } else if (kind == kw::_sym_directory) {
    /* If the pathname is a directory but we have supplied
	   a file name, correct the type by appending a directory
	   separator and re-parsing again the namestring */
    if (pathname->_Name.notnilp() ||
        pathname->_Type.notnilp()) {
      Str_sp spathname = (*filename) + DIR_SEPARATOR;
      pathname = af_truename(spathname);
    }
  }
  /* ECL does not contemplate version numbers
       in directory pathnames */
  if (pathname->_Name.nilp() &&
      pathname->_Type.nilp()) {
    /* We have to destructively change the
	 * pathname version here. Otherwise
	 * merge_pathnames will not do it. It is
	 * safe because coerce_to_file_pathname
	 * created a copy. */
    pathname->_Version = _Nil<T_O>();
  } else {
    pathname->_Version = kw::_sym_newest;
  }
  return Values(pathname, kind);
}

/*
 * Search the actual name of the directory of a pathname,
 * going through links if they exist. Default is
 * current directory
 */

#define ARGS_af_truename "(orig-pathname)"
#define DECL_af_truename ""
#define DOCS_af_truename "truename"
Pathname_sp af_truename(T_sp orig_pathname) {
  Pathname_sp pathname = make_absolute_pathname(orig_pathname);
  Pathname_sp base_dir = make_base_pathname(pathname);
  Cons_sp dir;
  /* We process the directory part of the filename, removing all
     * possible symlinks. To do so, we inspect recursively the
     * directory which contains our file, and come back. We also have to
     * ensure that the filename itself does not point to a symlink: if so,
     * then we resolve the value of the symlink and continue traversing
     * the filesystem.
     */
  for (dir = pathname->_Directory.as<Cons_O>(); dir.notnilp(); dir = cCdr(dir)) {
    base_dir = enter_directory(base_dir, oCar(dir), false);
  }
  pathname = brcl_mergePathnames(base_dir, pathname, kw::_sym_default);
  return file_truename(pathname, _Nil<Str_O>(), FOLLOW_SYMLINKS);
}

int clasp_backup_open(const char *filename, int option, int mode) {
  stringstream sbackup;
  sbackup << filename << ".BAK";
  string backupfilename = sbackup.str();
  brcl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
  /* Windows' rename doesn't replace an existing file */
  if (access(backupfilename, F_OK) == 0 && unlink(backupfilename)) {
    brcl_enable_interrupts();
    FElibc_error("Cannot remove the file ~S", 1,
                 ecl_make_constant_base_string(backupfilename, -1));
  }
#endif
  if (rename(filename, backupfilename.c_str())) {
    brcl_enable_interrupts();
    SIMPLE_ERROR(BF("Cannot rename the file %s to %s.") % Str_O::create(filename) % Str_O::create(backupfilename));
  }
  brcl_enable_interrupts();
  return open(filename, option, mode);
}

Integer_sp
clasp_file_len(int f) {
  struct stat filestatus;
  brcl_disable_interrupts();
  fstat(f, &filestatus);
  brcl_enable_interrupts();
  return Integer_O::create(static_cast<uint>(filestatus.st_size));
}

#define ARGS_cl_renameFile "(oldn newn &key (if-exists :error))"
#define DECL_cl_renameFile ""
#define DOCS_cl_renameFile "renameFile"
T_mv cl_renameFile(T_sp oldn, T_sp newn, T_sp if_exists) {
  _G();
  Str_sp old_filename, new_filename;
  Pathname_sp old_truename, new_truename;

  /* 1) Get the old filename, and complain if it has wild components,
 *    or if it does not exist. Notice that the filename to be renamed
 *    is not the truename, because we might be renaming a symbolic link.
 */
  old_truename = af_truename(oldn);
  old_filename = coerce_to_posix_filename(old_truename);

  /* 2) Create the new file name. */
  Pathname_sp pnewn = brcl_mergePathnames(newn, oldn, kw::_sym_newest);
  new_filename = af_coerceToFilename(pnewn);

  while (if_exists == kw::_sym_error || if_exists.nilp()) {
    if (af_probe_file(new_filename).nilp()) {
      if_exists = _lisp->_true();
      break;
    }
    /* if the file already exists */
    if (if_exists == kw::_sym_error) {
      const char *msg = "When trying to rename ~S, ~S already exists";
      if_exists = eval::funcall(_sym_signalSimpleError,
                                cl::_sym_fileError, /* condition */
                                kw::_sym_supersede, /* continuable */
                                /* format */
                                Str_O::create(msg),
                                Cons_O::createList(oldn, new_filename), /* format args */
                                kw::_sym_pathname,                      /* file-error options */
                                new_filename);
      if (if_exists == _lisp->_true())
        if_exists = kw::_sym_error;
    }
    if (if_exists.nilp()) {
      return Values(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>());
    }
  }
  if (UNLIKELY(if_exists != kw::_sym_supersede && if_exists != _lisp->_true())) {
    /* invalid key */
    SIMPLE_ERROR(BF("%s is an illegal IF-EXISTS option for RENAME-FILE.") % if_exists)
  }
  {
    brcl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
    int error = SetErrorMode(0);
    if (MoveFile((char *)old_filename->base_string.self,
                 (char *)new_filename->base_string.self)) {
      SetErrorMode(error);
      goto SUCCESS;
    }
    switch (GetLastError()) {
    case ERROR_ALREADY_EXISTS:
    case ERROR_FILE_EXISTS:
      break;
    default:
      goto FAILURE_CLOBBER;
    };
    if (MoveFileEx((char *)old_filename->base_string.self,
                   (char *)new_filename->base_string.self,
                   MOVEFILE_REPLACE_EXISTING)) {
      SetErrorMode(error);
      goto SUCCESS;
    }
    /* hack for win95/novell */
    chmod((char *)old_filename->base_string.self, 0777);
    chmod((char *)new_filename->base_string.self, 0777);
    SetFileAttributesA((char *)new_filename->base_string.self,
                       FILE_ATTRIBUTE_NORMAL);
    SetFileAttributesA((char *)new_filename->base_string.self,
                       FILE_ATTRIBUTE_TEMPORARY);
    if (MoveFile((char *)old_filename->base_string.self,
                 (char *)new_filename->base_string.self)) {
      SetErrorMode(error);
      goto SUCCESS;
    }
    /* fallback on old behavior */
    (void)DeleteFileA((char *)new_filename->base_string.self);
    if (MoveFile((char *)old_filename->base_string.self,
                 (char *)new_filename->base_string.self)) {
      SetErrorMode(error);
      goto SUCCESS;
    }
/* fall through */
#else
    if (rename((char *)old_filename->c_str(),
               (char *)new_filename->c_str()) == 0) {
      goto SUCCESS;
    }
#endif
  }
#if defined(ECL_MS_WINDOWS_HOST)
FAILURE_CLOBBER:
#endif
  brcl_enable_interrupts();
  {
    T_sp c_error = brcl_strerror(errno);
    const char *msg = "Unable to rename file ~S to ~S.~%C library error: ~S";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_fileError,                      /* condition */
                  _Nil<T_O>(),                             /* continuable */
                  Str_O::create(msg),                      /* format */
                  Cons_O::createList(oldn, newn, c_error), /* format args */
                  kw::_sym_pathname,                       /* file-error options */
                  oldn);
  }

SUCCESS:
  brcl_enable_interrupts();
  new_truename = af_truename(newn);
  return Values(newn, old_truename, new_truename);
}

static int
directory_pathname_p(Pathname_sp path) {
  return (path->_Name.nilp()) &&
         (path->_Type.nilp());
}

#define ARGS_af_deleteFile "(file)"
#define DECL_af_deleteFile ""
#define DOCS_af_deleteFile "deleteFile"
T_sp af_deleteFile(T_sp file) {
  _G();
  Pathname_sp path = cl_pathname(file);
  int isdir = directory_pathname_p(path);
  Str_sp filename = coerce_to_posix_filename(path);
  int ok;

  brcl_disable_interrupts();
  ok = (isdir ? rmdir : unlink)((char *)filename->c_str());
  brcl_enable_interrupts();

  if (ok < 0) {
    const char *msg =
        isdir ? "Cannot delete the file ~S.~%C library error: ~S" : "Cannot delete the directory ~S.~%C library error: ~S";
    T_sp c_error = brcl_strerror(errno);
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_fileError,
                  _lisp->_true(),                    // continuable
                  Str_O::create(msg),                // format
                  Cons_O::createList(file, c_error), // format args
                  kw::_sym_pathname,                 /* file-error options */
                  file);
  }
  return _lisp->_true();
}

#define ARGS_af_probe_file "(filespec)"
#define DECL_af_probe_file ""
#define DOCS_af_probe_file "probe_file"
Pathname_sp af_probe_file(T_sp filespec) {
  _G();
  Pathname_sp pfile = cl_pathname(filespec);
  /* INV: Both SI:FILE-KIND and TRUENAME complain if "file" has wildcards */
  return (af_file_kind(pfile, true).notnilp() ? af_truename(pfile) : _Nil<Pathname_O>());
}

#define ARGS_af_file_write_date "(pathspec)"
#define DECL_af_file_write_date ""
#define DOCS_af_file_write_date "file_write_date"
Number_sp af_file_write_date(T_sp pathspec) {
  _G();
  Number_sp time;
  Pathname_sp pathname = cl_pathname(pathspec);
  Str_sp filename = coerce_to_posix_filename(pathname);
  struct stat filestatus;
  time = _Nil<Number_O>();
  if (safe_stat((char *)filename->c_str(), &filestatus) >= 0) {
    Number_sp accJan1st1970UT(Integer_O::create(24 * 60 * 60));
    accJan1st1970UT = contagen_mul(accJan1st1970UT, Integer_O::create(17 + 365 * 70));
    time = Integer_O::create(static_cast<uint64_t>(filestatus.st_mtime));
    time = contagen_add(time, accJan1st1970UT);
  }
  return time;
}

#define ARGS_cl_fileAuthor "(file)"
#define DECL_cl_fileAuthor ""
#define DOCS_cl_fileAuthor "file_author"
T_sp cl_fileAuthor(T_sp file) {
  _G();
  T_sp output;
  Pathname_sp pn = cl_pathname(file);
  Str_sp filename = coerce_to_posix_filename(pn);
  struct stat filestatus;
  if (safe_stat((char *)filename->c_str(), &filestatus) < 0) {
    const char *msg = "Unable to read file author for ~S."
                      "~%C library error: ~S";
    T_sp c_error = brcl_strerror(errno);
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_fileError,                /* condition */
                  _lisp->_true(),                    /* continuable */
                  Str_O::create(msg),                /* format */
                  Cons_O::createList(file, c_error), /* format args */
                  kw::_sym_pathname,                 /* file-error options */
                  file);
  }
#ifdef HAVE_PWD_H
  {
    struct passwd *pwent;
    brcl_disable_interrupts();
    pwent = ::getpwuid(filestatus.st_uid);
    brcl_enable_interrupts();
    output = Str_O::create(pwent->pw_name);
  }
#else
  output = make_constant_base_string("UNKNOWN");
#endif
  return output;
}

Pathname_sp brcl_homedir_pathname(Str_sp user) {
  size_t i;
  Str_sp namestring;
  const char *h;
#if defined(ECL_MS_WINDOWS_HOST)
  const char *d;
#endif
  if (!Null(user)) {
#ifdef HAVE_PWD_H
    struct passwd *pwent = NULL;
#endif
    char *p;
    /* This ensures that our string has the right length
	   and it is terminated with a '\0' */
    user = Str_O::create(user->get());
    p = (char *)user->c_str();
    i = user->length();
    if (i > 0 && *p == '~') {
      p++;
      i--;
    }
    if (i == 0)
      return brcl_homedir_pathname(_Nil<Str_O>());
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
#if defined(ECL_MS_WINDOWS_HOST)
  } else if ((h = getenv("HOMEPATH")) && (d = getenv("HOMEDRIVE"))) {
    namestring =
        si_base_string_concatenate(2,
                                   make_constant_base_string(d),
                                   make_constant_base_string(h));
#endif
  } else {
    namestring = Str_O::create("/");
  }
  if (namestring->c_str()[0] == '~') {
    SIMPLE_ERROR(BF("Not a valid home pathname %s") % namestring->get());
  }
  i = namestring->length();
  if (!IS_DIR_SEPARATOR(namestring->c_str()[i - 1]))
    namestring = Str_O::create(namestring->get() + DIR_SEPARATOR);
  return af_parseNamestring(namestring);
}

#define ARGS_af_userHomedirPathname "(&optional host)"
#define DECL_af_userHomedirPathname ""
#define DOCS_af_userHomedirPathname "userHomedirPathname"
Pathname_sp af_userHomedirPathname(T_sp host) {
  _G();
  /* Ignore optional host argument. */
  return brcl_homedir_pathname(_Nil<Str_O>());
}

static bool
string_match(const char *s, T_sp pattern) {
  if (pattern.nilp() || pattern == kw::_sym_wild) {
    return 1;
  } else {
    int ls = strlen(s);
    Str_sp strng = Str_O::create(s, strlen(s));
    return clasp_stringMatch(strng, 0, ls,
                             pattern, 0, cl_length(pattern));
  }
}

/*
 * list_current_directory() lists the files and directories which are contained
 * in the current working directory (as given by current_dir()). If ONLY_DIR is
 * true, the list is made of only the directories -- a propert which is checked
 * by following the symlinks.
 */
static T_sp
list_directory(T_sp base_dir, T_sp text_mask, T_sp pathname_mask,
               int flags) {
  T_sp out = _Nil<T_O>();
  T_sp prefix = brcl_namestring(base_dir, BRCL_NAMESTRING_FORCE_BASE_STRING);
  T_sp component, component_path, kind;
  char *text;
#if defined(HAVE_DIRENT_H)
  DIR *dir;
  struct dirent *entry;

  brcl_disable_interrupts();
  dir = opendir((char *)prefix.as<Str_O>()->c_str());
  if (dir == NULL) {
    out = _Nil<T_O>();
    goto OUTPUT;
  }

  while ((entry = readdir(dir))) {
    text = entry->d_name;
#else
#ifdef ECL_MS_WINDOWS_HOST
  WIN32_FIND_DATA fd;
  HANDLE hFind = NULL;
  BOOL found = false;

  brcl_disable_interrupts();
  for (;;) {
    if (hFind == NULL) {
      T_sp aux = make_constant_base_string(".\\*");
      T_sp mask = af_base_string_concatenate(Cons_O::createList(prefix, aux));
      hFind = FindFirstFile((char *)mask->c_str(), &fd);
      if (hFind == INVALID_HANDLE_VALUE) {
        out = _Nil<T_O>();
        goto OUTPUT;
      }
      found = true;
    } else {
      found = FindNextFile(hFind, &fd);
    }
    if (!found)
      break;
    text = fd.cFileName;
#else  /* sys/dir.h as in SYSV */
  FILE *fp;
  char iobuffer[BUFSIZ];
  DIRECTORY dir;

  brcl_disable_interrupts();
  fp = fopen((char *)prefix->c_str(), OPEN_R);
  if (fp == NULL) {
    out = _Nil<T_O>();
    goto OUTPUT;
  }
  setbuf(fp, iobuffer);
  for (;;) {
    if (fread(&dir, sizeof(DIRECTORY), 1, fp) <= 0)
      break;
    if (dir.d_ino == 0)
      continue;
    text = dir.d_name;
#endif /* !ECL_MS_WINDOWS_HOST */
#endif /* !HAVE_DIRENT_H */
    if (text[0] == '.' &&
        (text[1] == '\0' ||
         (text[1] == '.' && text[2] == '\0')))
      continue;
    if (!string_match(text, text_mask))
      continue;
    component = Str_O::create(text);
    component = af_base_string_concatenate(Cons_O::createList(prefix, component));
    component_path = cl_pathname(component);
    if (!pathname_mask.nilp()) {
      if (!af_pathnameMatchP(component, pathname_mask)) // should this not be inverted?
        continue;
    }
    T_mv component_path_mv = file_truename(component_path, component, flags);
    component_path = component_path_mv;
    kind = component_path_mv.valueGet(1);
    out = Cons_O::create(Cons_O::create(component_path, kind), out);
  }
#ifdef HAVE_DIRENT_H
  closedir(dir);
#else
#ifdef ECL_MS_WINDOWS_HOST
  FindClose(hFind);
#else
  fclose(fp);
#endif /* !ECL_MS_WINDOWS_HOST */
#endif /* !HAVE_DIRENT_H */
  brcl_enable_interrupts();
OUTPUT:
  return cl_nreverse(out);
}

#define ARGS_core_mkstemp "(template)"
#define DECL_core_mkstemp ""
#define DOCS_core_mkstemp "mkstemp"
T_sp core_mkstemp(Str_sp thetemplate) {
  cl_index l;
  int fd;

#if defined(ECL_MS_WINDOWS_HOST)
  T_sp phys, dir, file;
  char strTempDir[MAX_PATH];
  char strTempFileName[MAX_PATH];
  char *s;
  int ok;

  phys = cl_translate_logical_pathname(1, thetemplate);
  dir = cl_make_pathname(8,
                         kw::_sym_type, _Nil<T_O>(),
                         kw::_sym_name, _Nil<T_O>(),
                         kw::_sym_version, _Nil<T_O>(),
                         kw::_sym_defaults, phys);
  dir = af_coerceToFilename(dir);
  file = cl_file_namestring(phys);

  l = dir->base_string.fillp;
  memcpy(strTempDir, dir->c_str(), l);
  strTempDir[l] = 0;
  for (s = strTempDir; *s; s++)
    if (*s == '/')
      *s = '\\';

  brcl_disable_interrupts();
  ok = GetTempFileName(strTempDir, (char *)file->c_str(), 0,
                       strTempFileName);
  brcl_enable_interrupts();
  if (!ok) {
    output = _Nil<T_O>();
  } else {
    l = strlen(strTempFileName);
    output = ecl_alloc_simple_base_string(l);
    memcpy(output->c_str(), strTempFileName, l);
  }
#else
  thetemplate = af_coerceToFilename(thetemplate);
  stringstream outss;
  outss << thetemplate->get();
  outss << "XXXXXX";
  string outname = outss.str();
  std::vector<char> dst_path(outname.begin(), outname.end());
  dst_path.push_back('\0');
  clasp_disable_interrupts();
#ifdef HAVE_MKSTEMP
  fd = mkstemp(&dst_path[0]);
  outname.assign(dst_path.begin(), dst_path.end() - 1);
#else
  if (mktemp(&dst_path[0])) {
    outname.assign(dst_path.begin(), dst_path.end() - 1);
    fd = open(outname.c_str(), O_CREAT | O_TRUNC, 0666);
  } else {
    fd = -1;
  }
#endif
  clasp_enable_interrupts();
  T_sp output;
  if (fd < 0) {
    output = _Nil<T_O>();
  } else {
    close(fd);
    output = af_truename(Str_O::create(outname));
  }
#endif
  return output;
}

#if 0 // working



@(defun ext::getcwd (&optional (change_d_p_d ECL_NIL))
	T_sp output;
@
	output = cl_parse_namestring(3, current_dir(), _Nil<T_O>(), _Nil<T_O>());
	if (!Null(change_d_p_d)) {
		ECL_SETQ(the_env, @'*default-pathname-defaults*', output);
	}
	@(return output)
@)

T_sp
si_get_library_pathname(void)
{
        T_sp s = cl_core.library_pathname;
        if (!Null(s)) {
                goto OUTPUT_UNCHANGED;
        } else {
                const char *v = getenv("ECLDIR");
                if (v) {
                        s = make_constant_base_string(v);
                        goto OUTPUT;
                }
        }
#if defined(ECL_MS_WINDOWS_HOST)
	{
        char *buffer;
	HMODULE hnd;
	cl_index len, ep;
        s = ecl_alloc_adjustable_base_string(cl_core.path_max);
        buffer = (char*)s->c_str();
	brcl_disable_interrupts();
	hnd = GetModuleHandle("ecl.dll");
	len = GetModuleFileName(hnd, buffer, cl_core.path_max-1);
	brcl_enable_interrupts();
	if (len == 0) {
		FEerror("GetModuleFileName failed (last error = ~S)",
			1, ecl_make_fixnum(GetLastError()));
	}
	s->base_string.fillp = len;
        /* GetModuleFileName returns a file name. We have to strip
         * the directory component. */
        s = cl_make_pathname(8, kw::_sym_name, _Nil<T_O>(), kw::_sym_type, _Nil<T_O>(),
			     kw::_sym_version, _Nil<T_O>(),
                             kw::_sym_defaults, s);
        s = ecl_namestring(s, BRCL_NAMESTRING_FORCE_BASE_STRING);
	}
#else
        s = make_constant_base_string(ECLDIR "/");
#endif
 OUTPUT:
        {
                T_sp true_pathname = af_probe_file(s);
                if (Null(true_pathname)) {
                        s = current_dir();
                } else {
                        /* Produce a string */
                        s = ecl_namestring(s, BRCL_NAMESTRING_FORCE_BASE_STRING);
                }
        }
        cl_core.library_pathname = s;
 OUTPUT_UNCHANGED:
        @(return s);
}

@(defun ext::chdir (directory &optional (change_d_p_d ECL_T))
	T_sp previous = si_getcwd(0);
	T_sp namestring;
@
	/* This will fail if the new directory does not exist */
	directory = cl_truename(directory);
	if (directory->_Name.notnilp() ||
	    directory->_Type.notnilp())
		FEerror("~A is not a directory pathname.", 1, directory);
	namestring = ecl_namestring(directory,
                                    BRCL_NAMESTRING_TRUNCATE_IF_ERROR |
                                    BRCL_NAMESTRING_FORCE_BASE_STRING);
	if (safe_chdir((char*)namestring->c_str(), _Nil<T_O>()) < 0) {
		T_sp c_error = brcl_strerror(errno);
		const char *msg = "Can't change the current directory to ~A."
			"~%C library error: ~S";
		eval::funcall(_sym_signalSimpleError,
			      cl::_sym_fileError, /* condition */
			      _lisp->_true(), /* continuable */
			      /* format */
			      Str_O::create(msg),
			      Cons_O::createList( directory, c_error), /* format args */
			      kw::_sym_pathname, /* file-error options */
			      directory);
	} else if (change_d_p_d.notnilp()) {
		ECL_SETQ(the_env, @'*default-pathname-defaults*', directory);
	}
	@(return previous)
@)


T_sp core_mkstemp(T_sp template)
{
    T_sp output;
    cl_index l;
    int fd;

#if defined(ECL_MS_WINDOWS_HOST)
    T_sp phys, dir, file;
    char strTempDir[MAX_PATH];
    char strTempFileName[MAX_PATH];
    char *s;
    int ok;

    phys = cl_translate_logical_pathname(1, template);
    dir = cl_make_pathname(8,
			   kw::_sym_type, _Nil<T_O>(),
			   kw::_sym_name, _Nil<T_O>(),
			   kw::_sym_version, _Nil<T_O>(),
			   kw::_sym_defaults, phys);
    dir = af_coerceToFilename(dir);
    file = cl_file_namestring(phys);

    l = dir->base_string.fillp;
    memcpy(strTempDir, dir->c_str(), l);
    strTempDir[l] = 0;
    for (s = strTempDir; *s; s++)
	if (*s == '/')
	    *s = '\\';

    brcl_disable_interrupts();
    ok = GetTempFileName(strTempDir, (char*)file->c_str(), 0,
			 strTempFileName);
    brcl_enable_interrupts();
    if (!ok) {
	output = _Nil<T_O>();
    } else {
	l = strlen(strTempFileName);
	output = ecl_alloc_simple_base_string(l);
	memcpy(output->c_str(), strTempFileName, l);
    }
#else
    template = af_coerceToFilename(template);
    l = template->base_string.fillp;
    output = ecl_alloc_simple_base_string(l + 6);
    memcpy(output->c_str(), template->c_str(), l);
    memcpy(output->c_str() + l, "XXXXXX", 6);

    brcl_disable_interrupts();
#ifdef HAVE_MKSTEMP
    fd = mkstemp((char*)output->c_str());
#else
    if (mktemp((char*)output->c_str())) {
	fd = open((char*)output->c_str(), O_CREAT|O_TRUNC, 0666);
    } else {
	fd = -1;
    }
#endif
    brcl_enable_interrupts();

    if (fd < 0) {
	output = _Nil<T_O>();
    } else {
	close(fd);
    }
#endif
    @(return (Null(output)? output : cl_truename(output)))
	}

T_sp
si_rmdir(T_sp directory)
{
    return af_deleteFile(cl_make_pathname(6, kw::_sym_name, _Nil<T_O>(),
					   kw::_sym_type, _Nil<T_O>(),
					   kw::_sym_defaults, directory));
}


T_sp
si_chmod(T_sp file, T_sp mode)
{
	mode_t code = ecl_to_uint32_t(mode);
	T_sp filename = coerce_to_posix_filename(file);
	unlikely_if (chmod((char*)filename->c_str(), code)) {
		T_sp c_error = brcl_strerror(errno);
		const char *msg = "Unable to change mode of file ~S to value ~O"
			"~%C library error: ~S";
		eval::funcall(_sym_signalSimpleError,
			      cl::_sym_fileError, /* condition */
			      _lisp->_true(), /* continuable */
			      /* format */
			      Str_O::create(msg),
			      Cons_O::createList( file, mode, c_error), /* format args */
			      kw::_sym_pathname, /* file-error options */
			      file);
	}
	@(return)
}
#endif // working

#define ARGS_core_copy_file "(orig dest)"
#define DECL_core_copy_file ""
#define DOCS_core_copy_file "copy_file"
T_sp core_copy_file(T_sp orig, T_sp dest) {
  FILE *in, *out;
  int ok = 0;
  Str_sp sorig = af_coerceToFilename(orig);
  Str_sp sdest = af_coerceToFilename(dest);
  brcl_disable_interrupts();
  in = fopen(sorig->c_str(), "r");
  if (in) {
    out = fopen(sdest->c_str(), "w");
    if (out) {
      unsigned char *buffer = (unsigned char *)malloc(1024);
      cl_index size;
      do {
        size = fread(buffer, 1, 1024, in);
        fwrite(buffer, 1, size, out);
      } while (size == 1024);
      ok = 1;
      fclose(out);
      free(buffer);
    }
    fclose(in);
  }
  brcl_enable_interrupts();
  if (ok)
    return _lisp->_true();
  return _Nil<T_O>();
}

/*
 * dir_files() lists all files which are contained in the current directory and
 * which match the masks in PATHNAME. This routine is essentially a wrapper for
 * list_current_directory(), which transforms the list of strings into a list
 * of pathnames. BASEDIR is the truename of the current directory and it is
 * used to build these pathnames.
 */
static T_sp
dir_files(T_sp base_dir, T_sp tpathname, int flags) {
  T_sp all_files, output = _Nil<T_O>();
  T_sp mask;
  Pathname_sp pathname = tpathname.as<Pathname_O>();
  T_sp name = pathname->_Name;
  T_sp type = pathname->_Type;
  if (name.nilp() && type.nilp()) {
    return Cons_O::create(base_dir);
  }
  mask = af_makePathname(_Nil<T_O>(), false,
                         _Nil<T_O>(), false,
                         _Nil<T_O>(), false,
                         name, true,
                         type, true,
                         pathname->_Version, true,
                         kw::_sym_local);
  for (all_files = list_directory(base_dir, _Nil<T_O>(), mask, flags);
       !Null(all_files);
       all_files = oCdr(all_files)) {
    T_sp record = oCar(all_files);
    T_sp nw = oCar(record);
    T_sp kind = oCdr(record);
    if (kind != kw::_sym_directory) {
      output = Cons_O::create(nw, output);
    }
  }
  return output;
}

/*
 * dir_recursive() performs the dirty job of DIRECTORY. The routine moves
 * through the filesystem looking for files and directories which match
 * the masks in the arguments PATHNAME and DIRECTORY, collecting them in a
 * list.
 */
static T_sp
dir_recursive(T_sp base_dir, T_sp directory, T_sp filemask, int flags) {
  T_sp item, output = _Nil<T_O>();
AGAIN:
  /* There are several possibilities here:
     *
     * 1) The list of subdirectories DIRECTORY is empty, and only PATHNAME
     * remains to be inspected. If there is no file name or type, then
     * we simply output the truename of the current directory. Otherwise
     * we have to find a file which corresponds to the description.
     */
  if (directory.nilp()) {
    return clasp_nconc(dir_files(base_dir, filemask, flags), output);
  }
  /*
     * 2) We have not yet exhausted the DIRECTORY component of the
     * pathname. We have to enter some subdirectory, determined by
     * CAR(DIRECTORY) and scan it.
     */
  item = oCar(directory);

  if (item == kw::_sym_wild || brcl_wild_string_p(item)) {
    /*
         * 2.1) If CAR(DIRECTORY) is a string or :WILD, we have to
         * enter & scan all subdirectories in our curent directory.
         */
    T_sp next_dir = list_directory(base_dir, item, _Nil<T_O>(), flags);
    for (; !Null(next_dir); next_dir = oCdr(next_dir)) {
      T_sp record = oCar(next_dir);
      T_sp component = oCar(record);
      T_sp kind = oCdr(record);
      if (kind != kw::_sym_directory)
        continue;
      item = dir_recursive(cl_pathname(component),
                           oCdr(directory),
                           filemask, flags);
      output = clasp_nconc(item, output);
    }
  } else if (item == kw::_sym_wild_inferiors) {
    /*
         * 2.2) If CAR(DIRECTORY) is :WILD-INFERIORS, we have to do
         * scan all subdirectories from _all_ levels, looking for a
         * tree that matches the remaining part of DIRECTORY.
         */
    T_sp next_dir = list_directory(base_dir, _Nil<T_O>(), _Nil<T_O>(), flags);
    for (; !Null(next_dir); next_dir = oCdr(next_dir)) {
      T_sp record = oCar(next_dir);
      T_sp component = oCar(record);
      T_sp kind = oCdr(record);
      if (kind != kw::_sym_directory)
        continue;
      item = dir_recursive(cl_pathname(component),
                           directory, filemask, flags);
      output = clasp_nconc(item, output);
    }
    directory = oCdr(directory);
    goto AGAIN;
  } else { /* :ABSOLUTE, :RELATIVE, :UP, component without wildcards */
    /*
         * 2.2) If CAR(DIRECTORY) is :ABSOLUTE, :RELATIVE or :UP we update
         * the directory to reflect the root, the current or the parent one.
         */
    base_dir = enter_directory(base_dir, item, 1);
    /*
         * If enter_directory() fails, we simply ignore this path. This is
         * what other implementations do and is consistent with the behavior
         * for the file part.
         */
    if (Null(base_dir))
      return _Nil<T_O>();
    directory = oCdr(directory);
    goto AGAIN;
  }
  return output;
}

#define ARGS_cl_directory "(mask &key (resolve-symlinks t) &allow-other-keys)"
#define DECL_cl_directory ""
#define DOCS_cl_directory "directory"
T_sp cl_directory(T_sp mask, T_sp resolveSymlinks) {
  _G();
  T_sp base_dir;
  T_sp output;
  mask = af_coerceToFilePathname(mask);
  mask = make_absolute_pathname(mask); // in this file
  base_dir = make_base_pathname(mask);
  output = dir_recursive(base_dir, af_pathnameDirectory(mask), mask,
                         resolveSymlinks.nilp() ? 0 : FOLLOW_SYMLINKS);
  return output;
};

#define ARGS_af_unixDaylightSavingTime "(unix-time)"
#define DECL_af_unixDaylightSavingTime ""
#define DOCS_af_unixDaylightSavingTime "unixDaylightSavingTime return true if in daylight saving time"
bool af_unixDaylightSavingTime(Integer_sp unix_time) {
  _G();
  time_t when = unix_time->as_uint64();
  struct tm *ltm = localtime(&when);
  return ltm->tm_isdst;
}

#define ARGS_af_unixGetLocalTimeZone "()"
#define DECL_af_unixGetLocalTimeZone ""
#define DOCS_af_unixGetLocalTimeZone "unixGetLocalTimeZone"
Ratio_sp af_unixGetLocalTimeZone() {
  _G();
  cl_fixnum mw;
#if 0 && defined(HAVE_TZSET)
  tzset();
  mw = timezone/60;
#else
  struct tm ltm, gtm;
  time_t when = time(0) /*0L*/;

  ltm = *localtime(&when);
  gtm = *gmtime(&when);

  mw = (gtm.tm_min + 60 * gtm.tm_hour) - (ltm.tm_min + 60 * ltm.tm_hour);

  if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
    mw -= 24 * 60;
  else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
    mw += 24 * 60;
#endif
  return Ratio_O::create(Fixnum_O::create(mw), Fixnum_O::create(60));
}

#define ARGS_core_mkdir "(dir mode)"
#define DECL_core_mkdir ""
#define DOCS_core_mkdir "mkdir"
T_sp core_mkdir(T_sp directory, T_sp mode) {
  int modeint = 0;
  int ok;
  Str_sp filename = coerce::stringDesignator(directory);
  if (Fixnum_sp fn = mode.asOrNull<Fixnum_O>()) {
    modeint = fn->get();
    if (modeint < 0 || modeint > 0777) {
      QERROR_WRONG_TYPE_NTH_ARG(2, mode, cl::_sym_Fixnum_O);
    }
  }
  {
    /* Ensure a clean string, without trailing slashes,
         * and null terminated. */
    int last = cl_length(filename);
    if (last > 1) {
      claspChar c = filename->schar(last - 1);
      if (IS_DIR_SEPARATOR(c))
        last--;
    }
    filename = filename->subseq(0, Fixnum_O::create(last));
  }
//    brcl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
  ok = mkdir((char *)filename->c_str());
#else
  ok = mkdir((char *)filename->c_str(), modeint);
#endif
  //    brcl_enable_interrupts();

  if (UNLIKELY(ok < 0)) {
    T_sp c_error = brcl_strerror(errno);
    const char *msg = "Could not create directory ~S"
                      "~%C library error: ~S";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_fileError, /* condition */
                  _lisp->_true(),     /* continuable */
                  /* format */
                  Str_O::create(msg),
                  Cons_O::createList(filename, c_error), /* format args */
                  kw::_sym_pathname,                     /* file-error options */
                  filename);
  }
  return filename;
}

void initialize_unixfsys() {
  Defun(chdir);
  Defun(unixGetLocalTimeZone);
  Defun(unixDaylightSavingTime);
  SYMBOL_EXPORT_SC_(CorePkg, currentDir);
  Defun(currentDir);
  SYMBOL_EXPORT_SC_(CorePkg, file_kind);
  Defun(file_kind);
  SYMBOL_EXPORT_SC_(ClPkg, truename);
  Defun(truename);
  SYMBOL_EXPORT_SC_(ClPkg, probe_file);
  Defun(probe_file);
  SYMBOL_EXPORT_SC_(ClPkg, deleteFile);
  Defun(deleteFile);
  SYMBOL_EXPORT_SC_(ClPkg, file_write_date);
  Defun(file_write_date);
  SYMBOL_EXPORT_SC_(ClPkg, userHomedirPathname);
  Defun(userHomedirPathname);
  Defun(fork);
  Defun(getpid);
  Defun(getppid);
  Defun(waitpid);
  ClDefun(fileAuthor);
  CoreDefun(mkdir);
  ClDefun(directory);
  ClDefun(renameFile);
  CoreDefun(mkstemp);
  CoreDefun(copy_file);
};
};
