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

#include <clasp/core/foundation.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <pthread.h> // TODO: PORTING - frgo, 2017-08-04
#include <signal.h>  // TODO: PORTING - frgo, 2017-08-04

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

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
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
#if defined(CLASP_MS_WINDOWS_HOST)
#include <windows.h>
#undef ERROR
#endif
#include <fcntl.h>
#include <errno.h>

#include <clasp/core/pathname.h>
#include <clasp/core/array.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/character.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bformat.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispList.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/wrappers.h>

#if defined(DEBUG_LEVEL_FULL)
#define DEBUG_PRINT(_msg_) fprintf(stderr, "%s", (_msg_).str().c_str())
#else
#define DEBUG_PRINT(msg)
#endif

SYMBOL_EXPORT_SC_(KeywordPkg, absolute);
SYMBOL_EXPORT_SC_(KeywordPkg, default);
SYMBOL_EXPORT_SC_(KeywordPkg, defaults);
SYMBOL_EXPORT_SC_(KeywordPkg, directory);
SYMBOL_EXPORT_SC_(KeywordPkg, error);
SYMBOL_EXPORT_SC_(KeywordPkg, file);
SYMBOL_EXPORT_SC_(KeywordPkg, link);
SYMBOL_EXPORT_SC_(KeywordPkg, broken_link);
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

String_sp clasp_strerror(int e) { return SimpleBaseString_O::make(std::string(strerror(e))); }

void rmtree(const char* path) {
  size_t path_len;
  DIR* dir;
  struct stat stat_path, stat_entry;
  struct dirent* entry;

  // stat for the path
  stat(path, &stat_path);

  // if path does not exists or is not dir - exit with status -1
  if (S_ISDIR(stat_path.st_mode) == 0) {
    if (S_ISREG(stat_path.st_mode) != 0 || S_ISLNK(stat_path.st_mode) != 0) {
      fprintf(stderr, "Removing file or link %s\n", path);
      unlink(path);
      return;
    }
    fprintf(stderr, "%s: %s\n", "Is not directory", path);
    return;
  }

  // if not possible to read the directory for this user
  if ((dir = opendir(path)) == NULL) {
    fprintf(stderr, "%s: %s\n", "Can`t open directory", path);
    return;
  }

  // the length of the path
  path_len = strlen(path);

  // iteration through entries in the directory
  while ((entry = readdir(dir)) != NULL) {

    // skip entries "." and ".."
    if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, ".."))
      continue;

    // determinate a full path of an entry
    std::string full_path = std::string(path) + "/" + std::string(entry->d_name);
    // stat for the entry
    stat(full_path.c_str(), &stat_entry);

    // recursively remove a nested directory
    if (S_ISDIR(stat_entry.st_mode) != 0) {
      rmtree(full_path.c_str());
      continue;
    }

    // remove a file object
    if (unlink(full_path.c_str()) != 0)
      printf("Can`t remove a file: %s error: %s\n", full_path.c_str(), std::strerror(errno));
  }

  // remove the devastated directory and close the object of it
  if (rmdir(path) != 0)
    printf("Can`t remove a directory: %s\n", path);

  closedir(dir);
}

static String_sp coerce_to_posix_filename(T_sp pathname) {
  /* This converts a pathname designator into a namestring, with the
   * particularity that directories do not end with a slash '/', because
   * this is not supported on all POSIX platforms (most notably Windows)
   */
  ASSERT(pathname);
  if (pathname.nilp())
    SIMPLE_ERROR("In {} the pathname is NIL", __FUNCTION__);
  String_sp sfilename = core__coerce_to_filename(pathname);
  return cl__string_right_trim(SimpleBaseString_O::make(DIR_SEPARATOR), sfilename);
}

static int safe_chdir(const char* path, T_sp tprefix) {
  if (cl__stringp(tprefix)) {
    String_sp prefix = gc::As_unsafe<String_sp>(tprefix);
    stringstream ss;
    ss << prefix->get_std_string() << path;
    return safe_chdir(ss.str().c_str(), nil<T_O>());
  } else {
    int output;
    output = chdir(path);
    return output;
  }
}

CL_LAMBDA(&optional return-stream);
CL_DECLARE();
CL_DOCSTRING(R"dx(fork)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__fork(bool bReturnStream) {
  int filedes[2];
  if (bReturnStream) {
    if (pipe(filedes) == -1) {
      perror("pipe");
      abort();
    }
  }
  pid_t child_PID = my_thread->safe_fork();
  if (child_PID >= 0) {
    if (child_PID == 0) {
      // Child
      if (bReturnStream) {
        while ((dup2(filedes[1], STDOUT_FILENO) == -1) && (errno == EINTR)) {
        }
        close(filedes[1]);
        close(filedes[0]);
        int flags = fcntl(STDOUT_FILENO, F_GETFL, 0);
        fcntl(STDOUT_FILENO, F_SETFL, flags | FD_CLOEXEC);
      }
      return Values(nil<T_O>(), make_fixnum(child_PID), nil<T_O>());
    } else {
      // Parent
      if (bReturnStream) {
        close(filedes[1]);
        int flags = fcntl(filedes[0], F_GETFL, 0);
        fcntl(filedes[0], F_SETFL, flags | O_NONBLOCK);
        T_sp stream = PosixFileStream_O::make(SimpleBaseString_O::make("execvp"), filedes[0], StreamDirection::input, 8,
                                              CLASP_STREAM_DEFAULT_FORMAT, nil<T_O>());
        return Values(nil<T_O>(), clasp_make_fixnum(child_PID), stream);
      }
      return Values(nil<T_O>(), clasp_make_fixnum(child_PID), nil<T_O>());
    }
  }
  return Values(clasp_make_fixnum(-1), SimpleBaseString_O::make(std::strerror(errno)), nil<T_O>());
};

CL_LAMBDA(stdout-fd stderr-fd);
CL_DECLARE();
CL_DOCSTRING(R"dx(fork-redirect)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__fork_redirect(int stdout_fd, int stderr_fd) {
  pid_t child_PID = my_thread->safe_fork();
  if (child_PID >= 0) {
    if (child_PID == 0) {
      // Child
      if (stdout_fd != STDOUT_FILENO) {
        while ((dup2(stdout_fd, STDOUT_FILENO) == -1) && (errno == EINTR)) {
        }
        close(stdout_fd);
      }
      if (stderr_fd != STDERR_FILENO) {
        while ((dup2(stderr_fd, STDERR_FILENO) == -1) && (errno == EINTR)) {
        }
        close(stderr_fd);
      }
      int flags = fcntl(STDOUT_FILENO, F_GETFL, 0);
      fcntl(STDOUT_FILENO, F_SETFL, flags | FD_CLOEXEC);
      flags = fcntl(STDERR_FILENO, F_GETFL, 0);
      fcntl(STDERR_FILENO, F_SETFL, flags | FD_CLOEXEC);
      return Values(nil<T_O>(), make_fixnum(child_PID));
    } else {
      // Parent
      return Values(nil<T_O>(), clasp_make_fixnum(child_PID));
    }
  }
  return Values(clasp_make_fixnum(-1), SimpleBaseString_O::make(std::strerror(errno)));
};

CL_DOCSTRING(R"(wait - see unix wait - returns (values pid status).
The status can be passed to //core:wifexited// and //core:wifsignaled//. )")
DOCGROUP(clasp);
CL_DEFUN T_mv core__wait() {
  int status;
  pid_t p = wait(&status);
  return Values(make_fixnum(p), make_fixnum(status));
};

CL_DOCSTRING(R"dx(Wraps the WIFEXITED(status) macro of the posix wait function.)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__wifexited(Fixnum_sp fstatus) {
  int status = fstatus.unsafe_fixnum();
  return WIFEXITED(status);
};

CL_DOCSTRING(R"dx(Wraps the WEXITSTATUS(status) macro of the posix wait function.)dx");
DOCGROUP(clasp);
CL_DEFUN int core__wexitstatus(Fixnum_sp fstatus) {
  int status = fstatus.unsafe_fixnum();
  return (int)WEXITSTATUS(status);
};

CL_DOCSTRING(R"dx(Wraps the WTERMSIG(status) macro of the posix wait function.)dx");
DOCGROUP(clasp);
CL_DEFUN int core__wtermsig(Fixnum_sp fstatus) {
  int status = fstatus.unsafe_fixnum();
  return WTERMSIG(status);
};

CL_DOCSTRING(R"dx(Wraps the WIFSIGNALED(status) macro of the posix wait function.)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__wifsignaled(Fixnum_sp fstatus) {
  int status = fstatus.unsafe_fixnum();
  return WIFSIGNALED(status);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(getpid)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__getpid() {
  Fixnum_sp pid = make_fixnum(getpid());
  return pid;
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(getppid)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__getppid() {
  Fixnum_sp pid = make_fixnum(getppid());
  return pid;
};

CL_LAMBDA(pathname &optional change_default_pathname_defaults);
CL_DECLARE();
CL_DOCSTRING(R"dx(Change the posix current working directory to pathname.)dx");
CL_DOCSTRING_LONG(R"dx(If change-default-pathname-defaults is T then also change *default-pathname-defaults*.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp ext__chdir(T_sp dir, T_sp change_default_pathname_defaults) {
  if (dir.nilp())
    SIMPLE_ERROR("{} is about to pass NIL to clasp_namestring", __FUNCTION__);
  //  printf("%s:%d ext__chdir dir -> %s\n", __FILE__, __LINE__, _rep_(dir).c_str());
  T_sp tdir = clasp_namestring(dir, true);
  LIKELY_if(cl__stringp(tdir)) {
    String_sp sdir = gc::As_unsafe<String_sp>(tdir);
    Integer_sp result = Integer_O::create((gc::Fixnum)safe_chdir(sdir->get_path_string().c_str(), nil<T_O>()));
    //    printf("%s:%d:%s  After safe_chdir\n", __FILE__, __LINE__, __FUNCTION__ );
    if (change_default_pathname_defaults.notnilp()) {
      clasp_write_string(fmt::format("Changing *default-pathname-defaults* because change-default-pathname-defaults -> {}\n",
                                     _rep_(change_default_pathname_defaults)));
      //      printf("%s:%d:%s  Before getcwd\n", __FILE__, __LINE__, __FUNCTION__ );
      core::getcwd(true); // get the current working directory and change *default-pathname-defaults* to it
                          //      printf("%s:%d:%s  After getcwd\n", __FILE__, __LINE__, __FUNCTION__ );
    }
    return result;
  }
  SIMPLE_ERROR("Could not convert {} to a namestring", _rep_(dir));
};

static int safe_stat(const char* path, struct stat* sb) {
  int output;
  output = stat(path, sb);
  return output;
}

#ifdef HAVE_LSTAT
static int safe_lstat(const char* path, struct stat* sb) {
  int output;
  output = lstat(path, sb);
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

}; // namespace core

namespace ext {
/*
 * Finds current directory by using getcwd() with an adjustable
 * string which grows until it can host the whole path.
 */

CL_DOCSTRING(R"dx(Return the unix current working directory)dx");
DOCGROUP(clasp);
CL_DEFUN core::Str8Ns_sp ext__getcwd() {
  // TESTME :   Test this function with the new code
  const char* ok = ::getcwd(NULL, 0);
  if (!ok) {
    SIMPLE_ERROR("There was an error in ext__getcwd - error: {}", strerror(errno));
  }
#if 0
  printf("%s:%d:%s entered ok -> %s\n", __FILE__, __LINE__, __FUNCTION__, ok );
  // Take into account what the shell, if any, might think about
  // the current working directory.  This is important in symlinked
  // trees.  However, we need to make sure that the information is
  // not outdated.
  const char *spwd = getenv("PWD");
  if (spwd) {
    printf("%s:%d:%s entered spwd -> %s\n", __FILE__, __LINE__, __FUNCTION__, spwd );
    if (::chdir(spwd) != -1) {
      // We make sure $PWD is not outdated by chdir'ing into it,
      // which resolves all symlinks, and make sure that the
      // directory ends up being the same as getcwd
      //
      // Performance optimization: we want to avoid this system
      // call on repeated calls to this function in the future.
      // However, I don't want to make it a non-const function
      // at this time.
      const char *nowpwd = ::getcwd(NULL,0);
      printf("%s:%d:%s  nowpwd -> %s\n", __FILE__, __LINE__, __FUNCTION__, nowpwd );
      if (nowpwd) {
        if (strcmp(ok, nowpwd) == 0) {
        // OK, we should use the shell's/user's idea of "cd"
          ::free((void*)ok);
          ok = strdup(spwd);
        }
        ::free((void*)nowpwd);
      }
    }
  } else {
    // was found to be invalid, save us re-testing on next call
    unsetenv("PWD");
  }
#endif
  size_t cwdsize = strlen(ok);
  // Pad with 4 characters for / and terminator \0
  core::Str8Ns_sp output = core::Str8Ns_O::make(cwdsize + 2, '\0', true, core::clasp_make_fixnum(0));
  StringPushStringCharStar(output, ok);
  ::free((void*)ok);
#if defined(_TARGET_OS_DARWIN) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
  // Add a terminal '/' if there is none.
  if (output[output->fillPointer() - 1] != DIR_SEPARATOR_CHAR) {
    output->vectorPushExtend(DIR_SEPARATOR_CHAR);
  }
#endif
#ifdef _MSC_VER
  for (c = output->base_string.self; *c; c++)
    if (*c == '\\')
      *c = '/';
#endif
  //  printf("%s:%d:%s %d  output-> %s\n", __FILE__, __LINE__, __FUNCTION__, getpid(), _rep_(output).c_str());
  return output;
}
}; // namespace ext

namespace core {
/*
 * Using a certain path, guess the type of the object it points to.
 */

static Symbol_sp file_kind(const char* filename, bool follow_links) {
  Symbol_sp output;
  struct stat buf;
#ifdef DEBUG_FILE_KIND
  printf("%s:%d   (file_kind %s %d)\n", __FILE__, __LINE__, filename, follow_links);
#ifdef HAVE_LSTAT
  printf("%s:%d    Using lstat\n", __FILE__, __LINE__);
#endif
#endif

#ifdef HAVE_LSTAT
  if ((follow_links ? safe_stat : safe_lstat)(filename, &buf) < 0)
#else
  if (safe_stat(filename, &buf) < 0)
#endif
  {
    output = nil<Symbol_O>();
  }
#ifdef HAVE_LSTAT
  else if (S_ISLNK(buf.st_mode)) {
    output = kw::_sym_link;
#ifdef DEBUG_FILE_KIND
    printf("%s:%d   output = :LINK\n", __FILE__, __LINE__);
#endif
  }
#endif
  else if (S_ISDIR(buf.st_mode)) {
    output = kw::_sym_directory;
#ifdef DEBUG_FILE_KIND
    printf("%s:%d   output = :DIRECTORY\n", __FILE__, __LINE__);
#endif
  } else if (S_ISREG(buf.st_mode)) {
    output = kw::_sym_file;
#ifdef DEBUG_FILE_KIND
    printf("%s:%d   output = :FILE\n", __FILE__, __LINE__);
#endif
  } else {
    output = kw::_sym_special;
#ifdef DEBUG_FILE_KIND
    printf("%s:%d   output = :SPECIAL\n", __FILE__, __LINE__);
#endif
  }
#ifdef DEBUG_FILE_KIND
  printf("%s:%d   Final file_kind --> %s\n", __FILE__, __LINE__, _rep_(output).c_str());
#endif
  return output;
}

static Symbol_sp smart_file_kind(String_sp sfilename, bool follow_links) {
  ASSERT(cl__stringp(sfilename));
  if (follow_links) {
    Symbol_sp kind_follow_links = file_kind((char*)(sfilename->get_path_string().c_str()), true);
    if (kind_follow_links.notnilp()) {
      return kind_follow_links;
    } else {
      // If its a broken link return _sym_file
      Symbol_sp kind_no_follow_links = file_kind((char*)(sfilename->get_path_string().c_str()), false);
      if (kind_no_follow_links.nilp())
        return nil<T_O>();
      return kw::_sym_broken_link;
    }
  } else {
    Symbol_sp kind = file_kind((char*)(sfilename->get_path_string().c_str()), false);
    return kind;
  }
}

CL_LAMBDA(filename follow-links);
CL_DECLARE();
CL_DOCSTRING(R"dx(file_kind (values kind found) - if found but kind==nil then its a broken symlink)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp core__file_kind(T_sp filename, bool follow_links) {
  ASSERT(filename);
  String_sp sfilename = coerce_to_posix_filename(filename);
  return smart_file_kind(sfilename, follow_links);
}

#if defined(HAVE_LSTAT) && !defined(CLASP_MS_WINDOWS_HOST)

CL_LAMBDA(filename);
CL_DECLARE();
CL_DOCSTRING(R"dx(file_kind (values kind found) - if found but kind==nil then its a br)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__readlink(String_sp filename) {
  ASSERT(cl__stringp(filename));
  /* Given a filename which is a symlink, this routine returns
   * the value of this link in the form of a pathname. */
  size_t size = 128, written;
  Str8Ns_sp output;
  Symbol_sp kind;
  do {
    output = Str8Ns_O::make(size + 2, '*', true, clasp_make_fixnum(0));
    written = readlink((char*)filename->get_path_string().c_str(), (char*)output->rowMajorAddressOfElement_(0), size);
    size += 256;
  } while (written == size - 256);
  output[written] = '\0';
  kind = file_kind((const char*)output->rowMajorAddressOfElement_(0), false);
  if (kind == kw::_sym_directory) {
    output[written++] = DIR_SEPARATOR_CHAR;
    output[written] = '\0';
  }
  output->fillPointerSet(written);
  return output;
}
#endif /* HAVE_LSTAT */

static Pathname_sp enter_directory(Pathname_sp base_dir, T_sp subdir, bool ignore_if_failure) {
  /* Assuming we start in "base_dir", enter a subdirectory named by
   * "subdir", which may be a string, :UP, :ABSOLUTE or :RELATIVE.
   * If the operation succeeds, return the truename of the resulting
   * path -- resolving any links in the process. */
  String_sp aux;
  Pathname_sp output;
  Symbol_sp kind;
  if (subdir == kw::_sym_absolute) {
    return gc::As<Pathname_sp>(
        eval::funcall(cl::_sym_makePathname, kw::_sym_directory, Cons_O::createList(subdir), kw::_sym_defaults, base_dir));
  } else if (subdir == kw::_sym_relative) {
    /* Nothing to do */
    return base_dir;
  } else if (subdir == kw::_sym_up) {
    aux = SimpleBaseString_O::make("..");
  } else if (!cl__stringp(subdir)) {
    SIMPLE_ERROR("Directory component {} found in pathname {}"
                 "is not allowed in TRUENAME or DIRECTORY",
                 _rep_(subdir), _rep_(base_dir));
  } else {
    aux = gc::As<String_sp>(subdir);
  }
  /* We now compose a new path based on the base directory and
   * the new component. We have to verify that the new pathname is
   * a directory and if it is a link recover the true name. */
  T_sp ldir = Cons_O::append(base_dir->_Directory, Cons_O::createList(aux));
  output = gc::As<Pathname_sp>(eval::funcall(cl::_sym_makePathname, kw::_sym_directory, ldir, kw::_sym_defaults, base_dir));
  if (output.nilp())
    SIMPLE_ERROR("{} is about to pass NIL to clasp_namestring", __FUNCTION__);
  aux = gc::As<String_sp>(clasp_namestring(output, CLASP_NAMESTRING_FORCE_BASE_STRING));
  aux = gc::As<String_sp>(aux->subseq(0, clasp_make_fixnum(aux->length() - 1)));
  //    aux->_contents()[aux->base_string.fillp-1] = 0;
  kind = file_kind((const char*)aux->rowMajorAddressOfElement_(0), false);
  if (kind.nilp()) {
    if (ignore_if_failure)
      return nil<Pathname_O>();
    CANNOT_OPEN_FILE_ERROR(aux);
//	FEcannot_open(aux);
#ifdef HAVE_LSTAT
  } else if (kind == kw::_sym_link) {
    output = cl__truename(cl__merge_pathnames(core__readlink(aux), base_dir, kw::_sym_default));
    if (output->_Name.notnilp() || output->_Type.notnilp())
      goto WRONG_DIR;
    return output;
#endif
  } else if (kind != kw::_sym_directory) {
  WRONG_DIR:
    if (ignore_if_failure)
      return nil<Pathname_O>();
    SIMPLE_ERROR("The directory {} in pathname {} actually points to a file or special device.", _rep_(subdir), _rep_(base_dir));
  }
  if (subdir == kw::_sym_up) {
    T_sp newdir = output->_Directory;
    newdir = cl__nbutlast(newdir, make_fixnum(2));
    if (newdir.nilp()) {
      if (ignore_if_failure)
        return nil<Pathname_O>();
      SIMPLE_ERROR("Pathname contained an :UP component  "
                   "that goes above the base directory: {}",
                   _rep_(output));
    }
    output->_Directory = newdir;
  }
  return output;
}

static Pathname_sp make_absolute_pathname(T_sp orig_pathname) {
  Pathname_sp base_dir = core::getcwd(false); // FIXME
  if (orig_pathname.nilp()) {
    SIMPLE_ERROR("In make_absolute_pathname NIL is about to be passed to core__coerce_to_file_pathname");
  }
  Pathname_sp pathname = core__coerce_to_file_pathname(orig_pathname);
  Pathname_sp result = clasp_mergePathnames(pathname, base_dir, kw::_sym_default);
  return result;
}

static Pathname_sp make_base_pathname(Pathname_sp pathname) {
  return Pathname_O::makePathname(pathname->_Host,                       // host
                                  pathname->_Device,                     // device
                                  Cons_O::createList(kw::_sym_absolute), // dir
                                  nil<T_O>(),                            // name
                                  nil<T_O>(),                            // type
                                  nil<T_O>(),                            // version
                                  kw::_sym_local);
}

#define FOLLOW_SYMLINKS 1

static Pathname_mv file_truename(T_sp pathname, T_sp filename, int flags) {
  Symbol_mv kind;
#ifdef DEBUG_FILE_KIND
  printf("%s:%d file_truename pathname: %s\n", __FILE__, __LINE__, _rep_(pathname).c_str());
#endif
  if (pathname.nilp()) {
    if (filename.nilp()) {
      INTERNAL_ERROR("file_truename: both FILENAME and PATHNAME are null!");
    }
    if (filename.nilp())
      SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
    pathname = cl__pathname(filename);
  } else if (filename.nilp()) {
    if (pathname.nilp())
      SIMPLE_ERROR("{} is about to pass NIL to clasp_namestring", __FUNCTION__);
    filename = clasp_namestring(pathname, CLASP_NAMESTRING_FORCE_BASE_STRING);
    if (filename.nilp()) {
      SIMPLE_ERROR("Unprintable pathname {} found in TRUENAME", _rep_(pathname));
    }
  }
  kind = file_kind((char*)gc::As<String_sp>(filename)->get_path_string().c_str(), false);
  //  kind = smart_file_kind( filename, false);
  if (kind.nilp()) {
    CANNOT_OPEN_FILE_ERROR(filename);
#ifdef HAVE_LSTAT
  } else if (kind == kw::_sym_link && (flags & FOLLOW_SYMLINKS)) {
    /* The link might be a relative pathname. In that case
     * we have to merge with the original pathname.  On
     * the other hand, if the link is broken - return file
     * truename "as is". */
    struct stat filestatus;
    if (safe_stat(gc::As<String_sp>(filename)->get_path_string().c_str(), &filestatus) < 0)
      return Values(pathname, kind);
    /* The link might be a relative pathname. In that case we have
     * to merge with the original pathname */
    filename = core__readlink(gc::As<String_sp>(filename));
    Pathname_sp pn = gc::As<Pathname_sp>(pathname);
    pathname = Pathname_O::makePathname(pn->_Host, pn->_Device, pn->_Directory, nil<T_O>(), nil<T_O>(), nil<T_O>(), kw::_sym_local);
    pathname = clasp_mergePathnames(filename, pathname, kw::_sym_default);
    if (pathname.nilp())
      SIMPLE_ERROR("{} is about to pass NIL to clasp_namestring", __FUNCTION__);
    filename = clasp_namestring(pathname, CLASP_NAMESTRING_FORCE_BASE_STRING);
    Pathname_sp truename = cl__truename(pathname);
    return Values(truename, kind);
#endif
  } else if (kind == kw::_sym_directory) {
    /* If the pathname is a directory but we have supplied
           a file name, correct the type by appending a directory
           separator and re-parsing again the namestring */
    if (gc::As<Pathname_sp>(pathname)->_Name.notnilp() || gc::As<Pathname_sp>(pathname)->_Type.notnilp()) {
      String_sp spathname = gc::As<String_sp>(filename);
      SafeBufferStr8Ns buffer;
      StringPushString(buffer.string(), spathname);
      buffer.string()->vectorPushExtend(DIR_SEPARATOR_CHAR);
      pathname = cl__truename(buffer.string());
    }
  }
  /* ECL does not contemplate version numbers
       in directory pathnames */
  if (gc::As<Pathname_sp>(pathname)->_Name.nilp() && gc::As<Pathname_sp>(pathname)->_Type.nilp()) {
    /* We have to destructively change the
     * pathname version here. Otherwise
     * merge_pathnames will not do it. It is
     * safe because coerce_to_file_pathname
     * created a copy. */
    gc::As<Pathname_sp>(pathname)->_Version = nil<T_O>();
  } else {
    gc::As<Pathname_sp>(pathname)->_Version = kw::_sym_newest;
  }
  return Values(gc::As<Pathname_sp>(pathname), kind);
}

CL_LAMBDA(pathname filename follow-links);
CL_DECLARE();
CL_DOCSTRING(R"dx(truename)dx");
DOCGROUP(clasp);
CL_DEFUN Pathname_mv core__file_truename(T_sp pathname, T_sp filename, bool follow_links) {
  return file_truename(pathname, filename, follow_links);
}

// This function is exposed to CL because it is needed to implement
// the generic version of CL:TRUENAME. It will be unexported by
// streams.lisp.
CL_DEFUN Pathname_sp gray__PERCENTtruename(T_sp filespec) {
  if (stream_p(filespec)) {
    T_sp pathname = stream_truename(filespec);
    if (cl__stringp(pathname))
      pathname = cl__parse_namestring(pathname);
    if (gc::IsA<Pathname_sp>(pathname))
      return gc::As<Pathname_sp>(pathname);
  }

  Pathname_sp pathname = make_absolute_pathname(filespec);
  Pathname_sp base_dir = make_base_pathname(pathname);
  Cons_sp dir;
  /* We process the directory part of the filename, removing all
   * possible symlinks. To do so, we inspect recursively the
   * directory which contains our file, and come back. We also have to
   * ensure that the filename itself does not point to a symlink: if so,
   * then we resolve the value of the symlink and continue traversing
   * the filesystem.
   */
  List_sp directory_parts = coerce_to_list(pathname->_Directory);
  for (auto dir : directory_parts) {
    base_dir = enter_directory(base_dir, oCar(dir), false);
  }
  pathname = clasp_mergePathnames(base_dir, pathname, kw::_sym_default);
#ifdef DEBUG_FILE_KIND
  printf("%s:%d cl__truename pathname: %s\n", __FILE__, __LINE__, _rep_(pathname).c_str());
#endif
  Pathname_mv truename = file_truename(pathname, nil<T_O>(), FOLLOW_SYMLINKS);
  return truename;
}

CL_LAMBDA(filespec);
CL_DECLARE();
CL_DOCSTRING(R"dx(truename tries to find the file indicated by filespec and returns its truename.
If the gray-streams module has been loaded then this function will be made generic.)dx");
DOCGROUP(clasp);
CL_DEFUN Pathname_sp cl__truename(T_sp filespec) { return gray__PERCENTtruename(filespec); }

int clasp_backup_open(const char* filename, int option, int mode) {
  stringstream sbackup;
  sbackup << filename << ".BAK";
  string backupfilename = sbackup.str();
#if defined(CLASP_MS_WINDOWS_HOST)
  /* Windows' rename doesn't replace an existing file */
  // FIXME: Classic TOCTOU bug here.
  if (access(backupfilename, F_OK) == 0 && unlink(backupfilename)) {
    FElibc_error("Cannot remove the file ~S", 1, ecl_make_constant_base_string(backupfilename, -1));
  }
#endif
  if (rename(filename, backupfilename.c_str())) {
    SIMPLE_ERROR("Cannot rename the file {} to {}.", _rep_(SimpleBaseString_O::make(std::string(filename))), backupfilename);
  }
  return open(filename, option, mode);
}

Integer_sp clasp_file_len(int f) {
#if 1
  size_t pos = lseek(f, 0, SEEK_CUR);
  lseek(f, 0, SEEK_END);
  size_t size = lseek(f, 0, SEEK_CUR);
  lseek(f, pos, SEEK_SET);
  return Integer_O::create((gc::Fixnum)(size));
#else
  struct stat filestatus;
  fstat(f, &filestatus);
  return Integer_O::create((gc::Fixnum)(filestatus.st_size));
#endif
}

CL_LAMBDA(oldn newn &key (if-exists :error));
CL_DECLARE();
CL_DOCSTRING(R"dx(renameFile)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__rename_file(T_sp oldn, T_sp newn, T_sp if_exists) {
  Pathname_sp old_truename, new_truename;
  /* 1) Get the old filename, and complain if it has wild components,
   *    or if it does not exist. Notice that the filename to be renamed
   *    is not the truename, because we might be renaming a symbolic link.
   */
  old_truename = cl__truename(oldn);
  if (old_truename.nilp())
    SIMPLE_ERROR("In {} the original name {} wasn't found", __FUNCTION__, _rep_(oldn));
  String_sp old_filename = coerce_to_posix_filename(old_truename);

  /* 2) Create the new file name. */
  Pathname_sp pnewn = clasp_mergePathnames(newn, oldn, kw::_sym_newest);
  if (pnewn.nilp())
    SIMPLE_ERROR("In {} the new name is NIL", __FUNCTION__);
  String_sp new_filename = core__coerce_to_filename(pnewn);
  while (if_exists == kw::_sym_error || if_exists.nilp()) {
    if (cl__probe_file(new_filename).nilp()) {
      if_exists = _lisp->_true();
      break;
    }
    /* if the file already exists */
    if (if_exists == kw::_sym_error) {
      std::string msg = "When trying to rename ~S, ~S already exists";
      if_exists = eval::funcall(_sym_signalSimpleError, core::_sym_simpleFileError, /* condition */
                                kw::_sym_supersede,                                 /* continuable */
                                /* format */
                                SimpleBaseString_O::make(msg), Cons_O::createList(oldn, new_filename), /* format args */
                                kw::_sym_pathname,                                                     /* file-error options */
                                new_filename);
      if (if_exists == _lisp->_true())
        if_exists = kw::_sym_error;
    }
    if (if_exists.nilp()) {
      return Values(nil<T_O>(), nil<T_O>(), nil<T_O>());
    }
  }
  if (UNLIKELY(if_exists != kw::_sym_supersede && if_exists != _lisp->_true())) {
    /* invalid key */
    SIMPLE_ERROR("{} is an illegal IF-EXISTS option for RENAME-FILE.", _rep_(if_exists));
  }
  if (cl__equal(new_filename, old_filename))
    goto SUCCESS;
  if (if_exists == _lisp->_true()) {
    T_sp dkind = core__file_kind(new_filename, false);
    if (dkind == kw::_sym_directory) {
      rmtree(new_filename->get_path_string().c_str());
    }
  }
  if (rename((char*)old_filename->get_path_string().c_str(), (char*)new_filename->get_path_string().c_str()) == 0) {
    goto SUCCESS;
  }
  {
    T_sp c_error = clasp_strerror(errno);
    std::string msg = "Unable to rename file ~S to ~S.~%C library error: ~S";
    eval::funcall(_sym_signalSimpleError, core::_sym_simpleFileError, /* condition */
                  nil<T_O>(),                                         /* continuable */
                  SimpleBaseString_O::make(msg),                      /* format */
                  Cons_O::createList(oldn, newn, c_error),            /* format args */
                  kw::_sym_pathname,                                  /* file-error options */
                  oldn);
  }

SUCCESS:
  new_truename = cl__truename(pnewn);
  // The first result value is newn with any missing components filled by a merge-pathnames
  return Values(pnewn, old_truename, new_truename);
}

static int directory_pathname_p(Pathname_sp path) { return (path->_Name.nilp()) && (path->_Type.nilp()); }

CL_LAMBDA(file);
CL_DECLARE();
CL_DOCSTRING(R"dx(deleteFile)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__delete_file(T_sp file) {
  if (file.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  Pathname_sp path = cl__pathname(file);
  int isdir = directory_pathname_p(path);
  String_sp filename = coerce_to_posix_filename(path);
  int ok;

  ok = (isdir ? rmdir : unlink)((char*)filename->get_path_string().c_str());

  if (ok < 0) {
    std::string msg =
        isdir ? "Cannot delete the file ~S.~%C library error: ~S" : "Cannot delete the directory ~S.~%C library error: ~S";
    T_sp c_error = clasp_strerror(errno);
    eval::funcall(_sym_signalSimpleError, core::_sym_simpleFileError,
                  _lisp->_true(),                    // continuable
                  SimpleBaseString_O::make(msg),     // format
                  Cons_O::createList(file, c_error), // format args
                  kw::_sym_pathname,                 /* file-error options */
                  file);
  }
  return _lisp->_true();
}

CL_LAMBDA(filespec);
CL_DECLARE();
CL_DOCSTRING(R"dx(probe_file)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__probe_file(T_sp filespec) {
  if (filespec.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  Pathname_sp pfile = cl__pathname(filespec);
  /* INV: Both SI:FILE-KIND and TRUENAME complain if "file" has wildcards */
  return (core__file_kind(pfile, true).notnilp() ? cl__truename(pfile) : nil<Pathname_O>());
}

CL_LAMBDA(pathspec);
CL_DECLARE();
CL_DOCSTRING(R"dx(file_write_date)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__file_write_date(T_sp pathspec) {
  T_sp time;
  if (pathspec.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  Pathname_sp pathname = cl__pathname(pathspec);
  String_sp filename = coerce_to_posix_filename(pathname);
  struct stat filestatus;
  time = nil<T_O>();
  if (safe_stat((char*)filename->get_path_string().c_str(), &filestatus) >= 0) {
    Integer_sp accJan1st1970UT = Integer_O::create((gc::Fixnum)(24 * 60 * 60));
    accJan1st1970UT = accJan1st1970UT * Integer_O::create((gc::Fixnum)(17 + 365 * 70));
    time = Integer_O::create((gc::Fixnum)filestatus.st_mtime) + accJan1st1970UT;
  }
  return time;
}

CL_LAMBDA(file);
CL_DECLARE();
CL_DOCSTRING(R"dx(file_author)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__file_author(T_sp file) {
  T_sp output;
  if (file.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  Pathname_sp pn = cl__pathname(file);
  String_sp filename = coerce_to_posix_filename(pn);
  struct stat filestatus;
  if (safe_stat((char*)filename->get_path_string().c_str(), &filestatus) < 0) {
    std::string msg = "Unable to read file author for ~S."
                      "~%C library error: ~S";
    T_sp c_error = clasp_strerror(errno);
    eval::funcall(_sym_signalSimpleError, core::_sym_simpleFileError, /* condition */
                  _lisp->_true(),                                     /* continuable */
                  SimpleBaseString_O::make(msg),                      /* format */
                  Cons_O::createList(file, c_error),                  /* format args */
                  kw::_sym_pathname,                                  /* file-error options */
                  file);
  }
#ifdef HAVE_PWD_H
  {
    struct passwd* pwent;
    pwent = ::getpwuid(filestatus.st_uid);
    output = SimpleBaseString_O::make(pwent->pw_name);
  }
#else
  output = make_constant_base_string("UNKNOWN");
#endif
  return output;
}

Pathname_sp clasp_homedir_pathname(T_sp tuser) {
  size_t i;
  String_sp namestring;
  const char* h;
#if defined(CLASP_MS_WINDOWS_HOST)
  const char* d;
#endif
  if (cl__stringp(tuser)) {
    String_sp user = gc::As_unsafe<String_sp>(tuser);
#ifdef HAVE_PWD_H
    struct passwd* pwent = NULL;
#endif
    char* p;
    /* This ensures that our string has the right length
           and it is terminated with a '\0' */
    user = SimpleBaseString_O::make(user->get_std_string());
    std::string suser = user->get_std_string();
    p = (char*)suser.c_str();
    i = user->length();
    if (i > 0 && *p == '~') {
      p++;
      i--;
    }
    if (i == 0)
      return clasp_homedir_pathname(nil<T_O>());
#ifdef HAVE_PWD_H
    pwent = getpwnam(p);
    if (pwent == NULL) {
      SIMPLE_ERROR("Unknown user {}.", p);
    }
    namestring = SimpleBaseString_O::make(std::string(pwent->pw_dir));
#endif
    SIMPLE_ERROR("Unknown user {}.", p);
  } else if ((h = getenv("HOME"))) {
    namestring = SimpleBaseString_O::make(std::string(h));
#if defined(CLASP_MS_WINDOWS_HOST)
  } else if ((h = getenv("HOMEPATH")) && (d = getenv("HOMEDRIVE"))) {
    namestring = si_base_string_concatenate(2, make_constant_base_string(d), make_constant_base_string(h));
#endif
  } else {
    namestring = SimpleBaseString_O::make("/");
  }
  if (namestring->get_path_string().c_str()[0] == '~') {
    SIMPLE_ERROR("Not a valid home pathname {}", namestring->get_path_string());
  }
  i = namestring->length();
  if (!IS_DIR_SEPARATOR(namestring->get_path_string().c_str()[i - 1]))
    namestring = SimpleBaseString_O::make(namestring->get_path_string() + DIR_SEPARATOR);
  return gc::As<Pathname_sp>(cl__parse_namestring(namestring));
}

CL_LAMBDA(&optional host);
CL_DECLARE();
CL_DOCSTRING(R"dx(userHomedirPathname)dx");
DOCGROUP(clasp);
CL_DEFUN Pathname_sp cl__user_homedir_pathname(T_sp host) {
  /* Ignore optional host argument. */
  return clasp_homedir_pathname(nil<T_O>());
}

static bool string_match(const char* s, T_sp pattern) {
  if (pattern.nilp() || pattern == kw::_sym_wild) {
    return 1;
  } else {
    int ls = strlen(s);
    String_sp strng = Str8Ns_O::create(s, strlen(s));
    return clasp_stringMatch(strng, 0, ls, pattern, 0, cl__length(pattern));
  }
}

/*
 * list_current_directory() lists the files and directories which are contained
 * in the current working directory (as given by current_dir()). If ONLY_DIR is
 * true, the list is made of only the directories -- a propert which is checked
 * by following the symlinks.
 */
static T_sp list_directory(T_sp base_dir, T_sp text_mask, T_sp pathname_mask, int flags) {
  T_sp out = nil<T_O>();
  if (base_dir.nilp())
    SIMPLE_ERROR("{} is about to pass NIL to clasp_namestring", __FUNCTION__);
  T_sp prefix = clasp_namestring(base_dir, CLASP_NAMESTRING_FORCE_BASE_STRING);
  T_sp component, component_path, kind;
  char* text;
  DIR* dir;
  struct dirent* entry;
  MultipleValues& mvn = core::lisp_multipleValues();

  dir = opendir((char*)gc::As<String_sp>(prefix)->get_path_string().c_str());
  if (dir == NULL) {
    out = nil<T_O>();
    goto OUTPUT;
  }

  while ((entry = readdir(dir))) {
    text = entry->d_name;
    if (text[0] == '.' && (text[1] == '\0' || (text[1] == '.' && text[2] == '\0')))
      continue;
    if (!string_match(text, text_mask))
      continue;
    component = SimpleBaseString_O::make(std::string(text));
    stringstream concat;
    String_sp str_prefix = coerce::stringDesignator(prefix);
    concat << str_prefix->get_std_string();
    String_sp str_component = coerce::stringDesignator(component);
    concat << str_component->get_std_string();
    // TODO Support proper strings
    component = SimpleBaseString_O::make(concat.str());
    if (component.nilp())
      SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
    component_path = cl__pathname(component);
    if (!pathname_mask.nilp()) {
      if (!cl__pathname_match_p(component, pathname_mask)) // should this not be inverted?
        continue;
    }
    T_mv component_path_mv = file_truename(component_path, component, flags);
    component_path = component_path_mv;
    kind = mvn.valueGet(1, component_path_mv.number_of_values());
    out = Cons_O::create(Cons_O::create(component_path, kind), out);
  }
  closedir(dir);
OUTPUT:
  return cl__nreverse(out);
}

// TODO This will need to be adjusted if we port to Windows.
Pathname_sp clasp_temporary_directory() {
  const char* tmp_env = getenv("TMPDIR");
#ifdef P_tmpdir
  std::string tempdir = tmp_env ? tmp_env : P_tmpdir;
#else
  std::string tempdir = tmp_env ? tmp_env : (DIR_SEPARATOR + "tmp" + DIR_SEPARATOR);
#endif

  if (!IS_DIR_SEPARATOR(tempdir[tempdir.size() - 1])) {
    tempdir += DIR_SEPARATOR;
  }

  return gc::As<Pathname_sp>(cl__parse_namestring(SimpleBaseString_O::make(tempdir)));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the temporary directory for this system.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__temporary_directory() { return clasp_temporary_directory(); }

CL_LAMBDA(template);
CL_DECLARE();
CL_DOCSTRING(R"dx(mkstemp)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mkstemp(String_sp thetemplate) {
  //  cl_index l;
  int fd;
  ASSERT(cl__stringp(thetemplate));
  if (thetemplate.nilp())
    SIMPLE_ERROR("In {} the template is NIL", __FUNCTION__);
  thetemplate = core__coerce_to_filename(thetemplate);
  stringstream outss;
  outss << thetemplate->get_path_string();
  outss << "XXXXXX";
  string outname = outss.str();
  std::vector<char> dst_path(outname.begin(), outname.end());
  dst_path.push_back('\0');
  fd = mkstemp(&dst_path[0]);
  outname.assign(dst_path.begin(), dst_path.end() - 1);
  T_sp output;
  if (fd < 0) {
    output = nil<T_O>();
  } else {
    close(fd);
    output = cl__truename(SimpleBaseString_O::make(outname));
  }
  return output;
}

CL_LAMBDA(template);
CL_DECLARE();
CL_DOCSTRING(R"dx(mkstemp-fd - return a file descriptor)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mkstemp_fd(String_sp thetemplate) {
  //  cl_index l;
  int fd;
  ASSERT(cl__stringp(thetemplate));
  if (thetemplate.nilp())
    SIMPLE_ERROR("In {} the template is NIL", __FUNCTION__);
  thetemplate = core__coerce_to_filename(thetemplate);
  stringstream outss;
  outss << thetemplate->get_path_string();
  outss << "XXXXXX";
  string outname = outss.str();
  std::vector<char> dst_path(outname.begin(), outname.end());
  dst_path.push_back('\0');
  fd = mkstemp(&dst_path[0]);
  outname.assign(dst_path.begin(), dst_path.end() - 1);
  T_sp output;
  if (fd < 0) {
    output = nil<T_O>();
  }
  unlink(outname.c_str());
  return make_fixnum(fd);
}

DOCGROUP(clasp);
CL_DEFUN void ext__rmtree(const string& spath) {
  const char* path = spath.c_str();
  rmtree(path);
};

CL_LAMBDA(template);
CL_DECLARE();
CL_DOCSTRING(R"dx(mkdtemp)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mkdtemp(String_sp thetemplate) {
  //  cl_index l;
  ASSERT(cl__stringp(thetemplate));
  if (thetemplate.nilp())
    SIMPLE_ERROR("In {} the template is NIL", __FUNCTION__);
  thetemplate = core__coerce_to_filename(thetemplate);
  stringstream outss;
  outss << thetemplate->get_path_string();
  outss << "XXXXXX";
  string outname = outss.str();
  std::vector<char> dst_path(outname.begin(), outname.end());
  dst_path.push_back('\0');
  const char* dirname = mkdtemp(&dst_path[0]);
  if (dirname == NULL) {
    SIMPLE_ERROR("There was an error in mkdtemp - errno {}", errno);
  }
  outname.assign(dst_path.begin(), dst_path.end() - 1);
  T_sp output;
  outname = outname + "/";
  output = cl__truename(SimpleBaseString_O::make(outname));
  return output;
}

#if 0 // working
@(defun ext::getcwd (&optional (change_d_p_d ECL_NIL))
	T_sp output;
@
	output = cl_parse_namestring(3, current_dir(), nil<T_O>(), nil<T_O>());
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
#if defined(CLASP_MS_WINDOWS_HOST)
	{
        char *buffer;
	HMODULE hnd;
	cl_index len, ep;
        s = ecl_alloc_adjustable_base_string(cl_core.path_max);
        buffer = (char*)s->c_str();
	hnd = GetModuleHandle("ecl.dll");
	len = GetModuleFileName(hnd, buffer, cl_core.path_max-1);
	if (len == 0) {
		FEerror("GetModuleFileName failed (last error = ~S)",
			1, ecl_make_fixnum(GetLastError()));
	}
	s->base_string.fillp = len;
        /* GetModuleFileName returns a file name. We have to strip
         * the directory component. */
        s = cl__make_pathname(8, kw::_sym_name, nil<T_O>(), kw::_sym_type, nil<T_O>(),
			     kw::_sym_version, nil<T_O>(),
                             kw::_sym_defaults, s);
        s = ecl__namestring(s, CLASP_NAMESTRING_FORCE_BASE_STRING);
	}
#else
        s = make_constant_base_string(ECLDIR "/");
#endif
 OUTPUT:
        {
                T_sp true_pathname = cl__probe_file(s);
                if (Null(true_pathname)) {
                        s = current_dir();
                } else {
                        /* Produce a string */
                        s = ecl__namestring(s, CLASP_NAMESTRING_FORCE_BASE_STRING);
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
	directory = cl__truename(directory);
	if (directory->_Name.notnilp() ||
	    directory->_Type.notnilp())
		FEerror("~A is not a directory pathname.", 1, directory);
	namestring = ecl__namestring(directory,
                                    CLASP_NAMESTRING_TRUNCATE_IF_ERROR |
                                    CLASP_NAMESTRING_FORCE_BASE_STRING);
	if (safe_chdir((char*)namestring->c_str(), nil<T_O>()) < 0) {
		T_sp c_error = clasp_strerror(errno);
                std::string msg = "Can't change the current directory to ~A."
                  "~%C library error: ~S";
		eval::funcall(_sym_signalSimpleError,
			      core::_sym_simpleFileError, /* condition */
			      _lisp->_true(), /* continuable */
			      /* format */
			      SimpleBaseString_O::make(msg),
			      Cons_O::createList( directory, c_error), /* format args */
			      kw::_sym_pathname, /* file-error options */
			      directory);
	} else if (change_d_p_d.notnilp()) {
		ECL_SETQ(the_env, @'*default-pathname-defaults*', directory);
	}
	@(return previous)
@)
T_sp core__mkstemp(T_sp template)
{
    T_sp output;
    cl_index l;
    int fd;
#if defined(CLASP_MS_WINDOWS_HOST)
    T_sp phys, dir, file;
    char strTempDir[MAX_PATH];
    char strTempFileName[MAX_PATH];
    char *s;
    int ok;
    phys = cl_translate_logical_pathname(1, template);
    dir = cl__make_pathname(8,
			   kw::_sym_type, nil<T_O>(),
			   kw::_sym_name, nil<T_O>(),
			   kw::_sym_version, nil<T_O>(),
			   kw::_sym_defaults, phys);
    if (dir.nilp()) SIMPLE_ERROR("In {} the dir is NIL", __FUNCTION__);
    dir = core__coerce_to_filename(dir);
    file = cl_file_namestring(phys);
    l = dir->base_string.fillp;
    memcpy(strTempDir, dir->c_str(), l);
    strTempDir[l] = 0;
    for (s = strTempDir; *s; s++)
	if (*s == '/')
	    *s = '\\';
    ok = GetTempFileName(strTempDir, (char*)file->c_str(), 0,
			 strTempFileName);
    if (!ok) {
	output = nil<T_O>();
    } else {
	l = strlen(strTempFileName);
	output = ecl_alloc_simple_base_string(l);
	memcpy(output->c_str(), strTempFileName, l);
    }
#else
    if (template.nilp()) SIMPLE_ERROR("In {} the template is NIL", __FUNCTION__);
    template = core__coerce_to_filename(template);
    l = template->base_string.fillp;
    output = ecl_alloc_simple_base_string(l + 6);
    memcpy(output->c_str(), template->c_str(), l);
    memcpy(output->c_str() + l, "XXXXXX", 6);
#ifdef HAVE_MKSTEMP
    fd = mkstemp((char*)output->c_str());
#else
    if (mktemp((char*)output->c_str())) {
	fd = open((char*)output->c_str(), O_CREAT|O_TRUNC, 0666);
    } else {
	fd = -1;
    }
#endif
    if (fd < 0) {
	output = nil<T_O>();
    } else {
	close(fd);
    }
#endif
    @(return (Null(output)? output : cl__truename(output)))
	}
#endif // working

CL_LAMBDA(directory);
CL_DECLARE();
CL_DOCSTRING(R"dx(Like unix rmdir)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__rmdir(T_sp directory) {
  return cl__delete_file(
      eval::funcall(cl::_sym_makePathname, kw::_sym_name, nil<T_O>(), kw::_sym_type, nil<T_O>(), kw::_sym_defaults, directory));
}

CL_LAMBDA(file mode);
CL_DECLARE();
CL_DOCSTRING(R"dx(chmod - use octal values for mode for convenience (eg #o777))dx");
DOCGROUP(clasp);
CL_DEFUN void core__chmod(T_sp file, T_sp mode) {
  mode_t code = clasp_to_uint32_t(mode);
  T_sp filename = coerce_to_posix_filename(file);
  unlikely_if(chmod((char*)gc::As<String_sp>(filename)->get_path_string().c_str(), code)) {
    T_sp c_error = clasp_strerror(errno);
    std::string msg = "Unable to change mode of file ~S to value ~O"
                      "~%C library error: ~S";
    eval::funcall(_sym_signalSimpleError, core::_sym_simpleFileError,                     /* condition */
                  _lisp->_true(),                                                         /* continuable */
                                                                                          /* format */
                  SimpleBaseString_O::make(msg), Cons_O::createList(file, mode, c_error), /* format args */
                  kw::_sym_pathname,                                                      /* file-error options */
                  file);
  }
}

CL_LAMBDA(orig dest);
CL_DECLARE();
CL_DOCSTRING(R"dx(copy_file)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__copy_file(T_sp orig, T_sp dest) {
  FILE *in, *out;
  int ok = 0;
  if (orig.nilp())
    SIMPLE_ERROR("In {} the source pathname is NIL", __FUNCTION__);
  String_sp sorig = core__coerce_to_filename(orig);
  if (dest.nilp())
    SIMPLE_ERROR("In {} the destination pathname is NIL", __FUNCTION__);
  String_sp sdest = core__coerce_to_filename(dest);
  in = fopen(sorig->get_path_string().c_str(), "r");
  if (in) {
    out = fopen(sdest->get_path_string().c_str(), "w");
    if (out) {
      unsigned char* buffer = (unsigned char*)malloc(1024);
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
  if (ok)
    return _lisp->_true();
  return nil<T_O>();
}

/*
 * dir_files() lists all files which are contained in the current directory and
 * which match the masks in PATHNAME. This routine is essentially a wrapper for
 * list_current_directory(), which transforms the list of strings into a list
 * of pathnames. BASEDIR is the truename of the current directory and it is
 * used to build these pathnames.
 */
static T_sp dir_files(T_sp base_dir, T_sp tpathname, int flags) {
  T_sp all_files, output = nil<T_O>();
  T_sp mask;
  Pathname_sp pathname = gc::As<Pathname_sp>(tpathname);
  T_sp name = pathname->_Name;
  T_sp type = pathname->_Type;
  if (name.nilp() && type.nilp()) {
    return Cons_O::create(base_dir, nil<T_O>());
  }
  mask = cl__make_pathname(nil<T_O>(), false, nil<T_O>(), false, nil<T_O>(), false, name, true, type, true, pathname->_Version,
                           true, kw::_sym_local);
  for (all_files = list_directory(base_dir, nil<T_O>(), mask, flags); !all_files.nilp(); all_files = oCdr(all_files)) {
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
static T_sp dir_recursive(T_sp base_dir, T_sp directory, T_sp filemask, int flags) {
  T_sp item, output = nil<T_O>();
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

  if (item == kw::_sym_wild || clasp_wild_string_p(item)) {
    /*
     * 2.1) If CAR(DIRECTORY) is a string or :WILD, we have to
     * enter & scan all subdirectories in our curent directory.
     */
    T_sp next_dir = list_directory(base_dir, item, nil<T_O>(), flags);
    for (; !next_dir.nilp(); next_dir = oCdr(next_dir)) {
      T_sp record = oCar(next_dir);
      T_sp component = oCar(record);
      T_sp kind = oCdr(record);
      if (kind != kw::_sym_directory)
        continue;
      if (component.nilp())
        SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
      item = dir_recursive(cl__pathname(component), oCdr(directory), filemask, flags);
      output = clasp_nconc(item, output);
    }
  } else if (item == kw::_sym_wild_inferiors) {
    /*
     * 2.2) If CAR(DIRECTORY) is :WILD-INFERIORS, we have to do
     * scan all subdirectories from _all_ levels, looking for a
     * tree that matches the remaining part of DIRECTORY.
     */
    T_sp next_dir = list_directory(base_dir, nil<T_O>(), nil<T_O>(), flags);
    for (; !next_dir.nilp(); next_dir = oCdr(next_dir)) {
      T_sp record = oCar(next_dir);
      T_sp component = oCar(record);
      T_sp kind = oCdr(record);
      if (kind != kw::_sym_directory)
        continue;
      if (component.nilp())
        SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
      item = dir_recursive(cl__pathname(component), directory, filemask, flags);
      output = clasp_nconc(item, output);
    }
    directory = oCdr(directory);
    goto AGAIN;
  } else { /* :ABSOLUTE, :RELATIVE, :UP, component without wildcards */
    /*
     * 2.2) If CAR(DIRECTORY) is :ABSOLUTE, :RELATIVE or :UP we update
     * the directory to reflect the root, the current or the parent one.
     */
    base_dir = enter_directory(gc::As<Pathname_sp>(base_dir), item, 1);
    /*
     * If enter_directory() fails, we simply ignore this path. This is
     * what other implementations do and is consistent with the behavior
     * for the file part.
     */
    if (base_dir.nilp())
      return nil<T_O>();
    directory = oCdr(directory);
    goto AGAIN;
  }
  return output;
}

CL_LAMBDA(mask &key (resolve-symlinks t) &allow-other-keys);
CL_DECLARE();
CL_DOCSTRING(R"dx(directory)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__directory(T_sp mask, T_sp resolveSymlinks) {
  T_sp base_dir;
  T_sp output;
  if (mask.nilp())
    SIMPLE_ERROR("In cl__directory NIL is about to be passed to core__coerce_to_file_pathname");
  mask = core__coerce_to_file_pathname(mask);
  mask = make_absolute_pathname(mask); // in this file
  base_dir = make_base_pathname(gc::As<Pathname_sp>(mask));
  output = dir_recursive(base_dir, cl__pathname_directory(mask), mask, resolveSymlinks.nilp() ? 0 : FOLLOW_SYMLINKS);
  return output;
};

CL_LAMBDA(unix-time);
CL_DECLARE();
CL_DOCSTRING(R"dx(unixDaylightSavingTime return true if in daylight saving time)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__unix_daylight_saving_time(Integer_sp unix_time) {
  time_t when = clasp_to_uint64_t(unix_time);
  struct tm* ltm = localtime(&when);
  return ltm->tm_isdst;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(unixGetLocalTimeZone)dx");
DOCGROUP(clasp);
CL_DEFUN Rational_sp core__unix_get_local_time_zone() {
  gctools::Fixnum mw;
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
  // Fix from ecl
  if (ltm.tm_isdst)
    mw += 60;
#endif
  return Rational_O::create(make_fixnum(mw), make_fixnum(60));
}

SYMBOL_EXPORT_SC_(CorePkg, mkdir);

CL_LAMBDA(dir mode);
CL_DECLARE();
CL_DOCSTRING(R"dx(mkdir)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mkdir(T_sp directory, T_sp mode) {
  int modeint = 0;
  int ok;
  String_sp filename = coerce::stringDesignator(directory);
  if (mode.fixnump()) {
    Fixnum_sp fnMode(gc::As<Fixnum_sp>(mode));
    modeint = unbox_fixnum(fnMode);
    if (modeint < 0 || modeint > 0777) {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_mkdir, 2, mode, Integer_O::makeIntegerType(0, 0777));
    }
  }
  {
    /* Ensure a clean string, without trailing slashes,
     * and null terminated. */
    int last = cl__length(filename);
    if (last > 1) {
      claspCharacter c = clasp_as_claspCharacter(cl__char(filename, last - 1));
      if (IS_DIR_SEPARATOR(c))
        last--;
    }
    filename = gc::As_unsafe<String_sp>(filename->subseq(0, make_fixnum(last)));
  }
#if defined(CLASP_MS_WINDOWS_HOST)
  ok = mkdir((char*)filename->c_str());
#else
  ok = mkdir((char*)filename->get_path_string().c_str(), modeint);
#endif

  if (UNLIKELY(ok < 0)) {
    T_sp c_error = clasp_strerror(errno);
    std::string msg = "Could not create directory ~S"
                      "~%C library error: ~S";
    eval::funcall(_sym_signalSimpleError, core::_sym_simpleFileError, /* condition */
                  _lisp->_true(),                                     /* continuable */
                  /* format */
                  SimpleBaseString_O::make(msg), Cons_O::createList(filename, c_error), /* format args */
                  kw::_sym_pathname,                                                    /* file-error options */
                  filename);
  }
  return filename;
}

CL_LAMBDA(dir mode);
CL_DECLARE();
CL_DOCSTRING(R"dx(ensure-directory is like si:mkdir but it doesn't signal an error if the directory exists.
Return NIL if the directory could not be created.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__ensure_directory(T_sp directory, T_sp mode) {
  int modeint = 0;
  int ok;
  String_sp filename = coerce::stringDesignator(directory);
  if (mode.fixnump()) {
    Fixnum_sp fnMode(gc::As<Fixnum_sp>(mode));
    modeint = unbox_fixnum(fnMode);
    if (modeint < 0 || modeint > 0777) {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_mkdir, 2, mode, Integer_O::makeIntegerType(0, 0777));
    }
  }
  {
    /* Ensure a clean string, without trailing slashes,
     * and null terminated. */
    int last = cl__length(filename);
    if (last > 1) {
      claspCharacter c = clasp_as_claspCharacter(cl__char(filename, last - 1));
      if (IS_DIR_SEPARATOR(c))
        last--;
    }
    filename = gc::As_unsafe<String_sp>(filename->subseq(0, make_fixnum(last)));
  }
#if defined(CLASP_MS_WINDOWS_HOST)
  ok = mkdir((char*)filename->c_str());
#else
  ok = mkdir((char*)filename->get_path_string().c_str(), modeint);
#endif

  if (UNLIKELY(ok < 0)) {
    return nil<T_O>();
  }
  return filename;
}

CL_LAMBDA(name value &optional (overwrite t));
CL_DECLARE();
CL_DOCSTRING(R"dx(Set environment variable NAME to VALUE)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__setenv(String_sp name, String_sp value, bool overwrite) {
  ASSERT(cl__stringp(name));
  ASSERT(cl__stringp(value));
  setenv(name->get_std_string().c_str(), value->get_std_string().c_str(), overwrite);
}

CL_LAMBDA(cmd);
CL_DECLARE();
CL_DOCSTRING(R"dx(system)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv ext__system(String_sp cmd) {
  ASSERT(cl__stringp(cmd));
  string command = cmd->get_std_string();
  int ret = system(command.c_str());
  if (ret == 0) {
    return Values(core::make_fixnum(0));
  } else {
    return Values(core::make_fixnum(ret), SimpleBaseString_O::make(std::strerror(errno)));
  }
}

CL_DOCSTRING(R"dx(Invoke unix setpgid(p1, p2))dx");
DOCGROUP(clasp);
CL_DEFUN int core__setpgid(pid_t p1, pid_t p2) {
  int pid = setpgid(p1, p2);
  return pid;
}

CL_DOCSTRING(R"dx(Invoke unix setpgrp())dx");
DOCGROUP(clasp);
CL_DEFUN int core__setpgrp() {
  // not a typo in the function name.  This is the portable version
  int pid = setpgid(0, 0);
  return pid;
}

CL_DOCSTRING(R"dx(Return (values pipe0 pipe1). Signal an error if pipe failed.)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__pipe() {
  int pipes[2];
  int ret = pipe(pipes);
  if (ret == 0) {
    return Values(make_fixnum(pipes[0]), make_fixnum(pipes[1]));
  }
  SIMPLE_ERROR("Could not create pipe - error: {}", strerror(errno));
}

CL_LAMBDA(call-and-arguments &optional return-stream);
CL_DECLARE();
CL_DOCSTRING(R"(vfork_execvp - pass optional return-stream value of T if you want the output stream of the child.
Returns (values 0 child-pid stream) if return-stream is T, (values errno error-message nil) if there was an error. )")
DOCGROUP(clasp);
CL_DEFUN T_mv ext__vfork_execvp(List_sp call_and_arguments, T_sp return_stream) {

  if (call_and_arguments.nilp())
    return Values0<T_O>();

  bool bReturnStream = return_stream.isTrue();

  std::vector<char const*> execvp_args(cl__length(call_and_arguments) + 1);

  size_t idx = 0;

  for (auto cur : call_and_arguments) {
    String_sp sarg = gc::As<String_sp>(oCar(cur));
    size_t sarg_size = sarg->length();
    //    printf("%s:%d sarg = %s sarg->size() = %ld  strlen(sarg->c_str()) = %ld\n", __FILE__, __LINE__, sarg->c_str(),
    //    sarg->size(), strlen(sarg->c_str()));
    char* arg = (char*)malloc(sarg_size + 1);
    std::strncpy(arg, sarg->get_std_string().c_str(), sarg_size);
    arg[sarg_size] = '\0';
    execvp_args[idx++] = arg;
  }

  execvp_args[idx] = NULL;
  int filedes[2] = {-1, -1};

  if (bReturnStream) {
    if (pipe(filedes) == -1) {
      perror("pipe");
      abort();
    }
  }

  pid_t child_PID = vfork();

  if (child_PID >= 0) {
    if (child_PID == 0) {
      // Child

      if (bReturnStream) {
        while ((dup2(filedes[1], STDOUT_FILENO) == -1) && (errno == EINTR)) {
        }
        close(filedes[1]);
        close(filedes[0]);
        int flags = fcntl(STDOUT_FILENO, F_GETFL, 0);
        fcntl(STDOUT_FILENO, F_SETFL, flags | FD_CLOEXEC);
      }

      bool b_done = false;
      sigset_t new_sigset;
      sigset_t old_sigset;

      while (b_done == false) {
        int rc = 0;

        sigemptyset(&new_sigset);
        sigemptyset(&old_sigset);
        sigaddset(&new_sigset, SIGINT);
        sigaddset(&new_sigset, SIGCHLD);

        rc = pthread_sigmask(SIG_SETMASK, &new_sigset, &old_sigset); // TODO: Check return value
        rc = execvp(execvp_args[0], (char* const*)execvp_args.data());
        pthread_sigmask(SIG_SETMASK, &old_sigset, NULL); // Restore signal mask

        if (rc == -1) // An  error has occurred during - we do a retry
        {
          if (errno != EINTR)
            b_done = true;
        }
      }

      _exit(EXIT_FAILURE); // Should never reach
    } else {
      // Parent

      int status = 0;
      bool b_done = false;
      pid_t wait_ret = -1;
      int child_exit_status = -1;

      while (b_done == false) {
        errno = 0;

        wait_ret = wait(&status);

        if (WIFEXITED(status)) {
          child_exit_status = WEXITSTATUS(status);
          DEBUG_PRINT(BF("%s (%s:%d) | Child process exited with status %d\n.") % __FUNCTION__ % __FILE__ % __LINE__ %
                      child_exit_status);

          b_done = true;
        }

        if (WIFSIGNALED(status)) {
          int signal = 0;

          signal = WTERMSIG(status);
          DEBUG_PRINT(BF("%s (%s:%d) | Child process got signal %d\n.") % __FUNCTION__ % __FILE__ % __LINE__ % signal);

          // Continue waiting !
        }
      }

      DEBUG_PRINT(BF("%s (%s:%d) | Child process cmd = %s\n.") % __FUNCTION__ % __FILE__ % __LINE__ % execvp_args[0]);
      DEBUG_PRINT(BF("%s (%s:%d) | Child process wait(): return code = %d, errno = %d, error = %s\n.") % __FUNCTION__ % __FILE__ %
                  __LINE__ % wait_ret % errno % strerror(errno));

      // Clean up args
      for (int i(0); i < execvp_args.size() - 1; ++i) {
        free((void*)execvp_args[i]);
        execvp_args[i] = nullptr;
      }

      if ((wait_ret >= 0) || ((wait_ret == -1) && (errno == EINTR) && (child_exit_status == 0))) {
        errno = 0;

        if (bReturnStream) {
          int flags = fcntl(filedes[0], F_GETFL, 0);
          fcntl(filedes[0], F_SETFL, flags | O_NONBLOCK);
          T_sp stream = PosixFileStream_O::make(SimpleBaseString_O::make("execvp"), filedes[0], StreamDirection::input, 8,
                                                CLASP_STREAM_DEFAULT_FORMAT, nil<T_O>());

          DEBUG_PRINT(BF("%s (%s:%d) | Values( %d %d %p )\n.") % __FUNCTION__ % __FILE__ % __LINE__ % 0 % child_PID % stream);

          return Values(clasp_make_fixnum(0), clasp_make_fixnum(child_PID), stream);
        }

        DEBUG_PRINT(BF("%s (%s:%d) | Values( %d %d %d )\n.") % __FUNCTION__ % __FILE__ % __LINE__ % 0 % child_PID % 0);

        return Values(clasp_make_fixnum(0), clasp_make_fixnum(child_PID), nil<T_O>());
      }

      // error
      DEBUG_PRINT(BF("%s (%s:%d) | Values( %d %d %d )\n.") % __FUNCTION__ % __FILE__ % __LINE__ % errno % strerror(errno) % 0);

      char* serr = std::strerror(errno);
      return Values(clasp_make_fixnum(errno), SimpleBaseString_O::make(serr), nil<T_O>());
    }
  } else {
    // Creating the child failed

    // Clean up args
    for (int i(0); i < execvp_args.size() - 1; ++i) {
      free((void*)execvp_args[i]);
      execvp_args[i] = nullptr;
    }

    return Values(clasp_make_fixnum(-1), SimpleBaseString_O::make(std::strerror(errno)), nil<T_O>());
  }
}

CL_LAMBDA(call-and-arguments &optional return-stream);
CL_DECLARE();
CL_DOCSTRING(R"(fork_execvp - set optional return-stream to T if you want the output stream of the child.
Returns (values error pid return-stream).)")
DOCGROUP(clasp);
CL_DEFUN T_mv ext__fork_execvp(List_sp call_and_arguments, T_sp return_stream) {
  bool bReturnStream = return_stream.isTrue();
  if (call_and_arguments.nilp())
    return Values0<T_O>();
  std::vector<char const*> execvp_args(cl__length(call_and_arguments) + 1);
  size_t idx = 0;
  for (auto cur : call_and_arguments) {
    String_sp sarg = gc::As<String_sp>(oCar(cur));
    size_t sarg_size = sarg->length();
    //    printf("%s:%d sarg = %s sarg->size() = %ld\n", __FILE__, __LINE__, sarg->c_str(), sarg->size());
    char* arg = (char*)malloc(sarg_size + 1);
    std::strncpy(arg, sarg->get_std_string().c_str(), sarg_size);
    arg[sarg_size] = '\0';
    execvp_args[idx++] = arg;
  }
  execvp_args[idx] = NULL;
  int filedes[2];
  if (bReturnStream) {
    if (pipe(filedes) == -1) {
      perror("pipe");
      abort();
    }
  }
  pid_t child_PID = my_thread->safe_fork();
  if (child_PID >= 0) {
    if (child_PID == 0) {
      // Child
      if (bReturnStream) {
        while ((dup2(filedes[1], STDOUT_FILENO) == -1) && (errno == EINTR)) {
        }
        close(filedes[1]);
        close(filedes[0]);
        int flags = fcntl(STDOUT_FILENO, F_GETFL, 0);
        fcntl(STDOUT_FILENO, F_SETFL, flags | FD_CLOEXEC);
      }
      execvp(execvp_args[0], (char* const*)execvp_args.data());
      printf("%s:%d execvp returned with errno=%d   strerror(errno) = %s\n", __FILE__, __LINE__, errno, strerror(errno));
      for (int i = 0; execvp_args[i] != NULL; ++i) {
        printf("    arg#%d  %s\n", i, execvp_args[i]);
      }
      printf("  cannot continue... exiting... sorry...\n");
      _exit(0); // Should never reach
    } else {
      // Parent
      int status;
      pid_t wait_ret = wait(&status);
      // Clean up args
      for (int i(0); i < execvp_args.size() - 1; ++i)
        free((void*)execvp_args[i]);
      if (wait_ret >= 0) {
        if (wait_ret != child_PID) {
          printf("%s:%d wait return PID(%d) that did not match child(%d)\n", __FILE__, __LINE__, wait_ret, child_PID);
        }
        if (bReturnStream) {
          int flags = fcntl(filedes[0], F_GETFL, 0);
          fcntl(filedes[0], F_SETFL, flags | O_NONBLOCK);
          T_sp stream = PosixFileStream_O::make(SimpleBaseString_O::make("execvp"), filedes[0], StreamDirection::input, 8,
                                                CLASP_STREAM_DEFAULT_FORMAT, nil<T_O>());
          return Values(nil<T_O>(), clasp_make_fixnum(child_PID), stream);
        }
        return Values(nil<T_O>(), clasp_make_fixnum(child_PID), nil<T_O>());
      }
      // error
      return Values(clasp_make_fixnum(errno), SimpleBaseString_O::make(std::strerror(errno)), nil<T_O>());
    }
  } else {
    // Clean up args
    for (int i(0); i < execvp_args.size() - 1; ++i)
      free((void*)execvp_args[i]);
    return Values(clasp_make_fixnum(-1), SimpleBaseString_O::make(std::strerror(errno)), nil<T_O>());
  }
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Get environment variable NAME)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp ext__getenv(String_sp arg) {
  ASSERT(cl__stringp(arg));
  char* sres = getenv(arg->get_std_string().c_str());
  if (sres == NULL) {
    return nil<T_O>();
  }
  return SimpleBaseString_O::make(sres);
};

SYMBOL_EXPORT_SC_(CorePkg, file_kind);
SYMBOL_EXPORT_SC_(ClPkg, truename);
SYMBOL_EXPORT_SC_(ClPkg, probe_file);
SYMBOL_EXPORT_SC_(ClPkg, deleteFile);
SYMBOL_EXPORT_SC_(ClPkg, file_write_date);
SYMBOL_EXPORT_SC_(ClPkg, userHomedirPathname);

#if defined(HAVE_SELECT)
void error_bad_fd(int fd) { SIMPLE_ERROR("Invalid file-descriptor {}", fd); }

CL_DOCSTRING(R"dx(See unix select)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__select(int nfds, FdSet_sp readfds, FdSet_sp writefds, FdSet_sp errorfds, size_t seconds, size_t microseconds) {
  struct timeval timeout;
  timeout.tv_sec = seconds;
  timeout.tv_usec = microseconds;
  int num = select(nfds, &readfds->_fd_set, &writefds->_fd_set, &errorfds->_fd_set, &timeout);
  if (num < 0) {
    return Values(make_fixnum(num), make_fixnum(errno));
  }
  return Values(make_fixnum(num), nil<T_O>());
}

DOCGROUP(clasp);
CL_DEFUN FdSet_sp core__make_fd_set() {
  auto fdset = gctools::GC<FdSet_O>::allocate_with_default_constructor();
  return fdset;
}
#endif // defined(HAVE_SELECT)

CL_LAMBDA(filedescriptor);
CL_DECLARE();
CL_DOCSTRING("Returns data of the posix fstat() function for file-descriptor"
             "if pathname is not found, returns nil"
             "else mutiple values of"
             "stat.st_size (file size in bytes, follows symbolic links)"
             "stat.st_mtime (Time of last data modification.)"
             "stat.st_mode (Mode of file))doc")
DOCGROUP(clasp);
CL_DEFUN T_mv ext__fstat(int filedescriptor) {
  struct stat sb;
  if (fstat(filedescriptor, &sb) == -1)
    return Values(nil<T_O>());
  else
    return Values(Integer_O::create(sb.st_size), Integer_O::create((gctools::Fixnum)sb.st_mtime),
                  Integer_O::create((gctools::Fixnum)sb.st_mode));
};

bool clasp_has_file_position(int filedescriptor) {
  struct stat sb;
  if (fstat(filedescriptor, &sb) == -1)
    return false;
  else if (S_ISSOCK(sb.st_mode) || S_ISFIFO(sb.st_mode) || S_ISDIR(sb.st_mode))
    return false;
  else
    return true;
}

CL_LAMBDA(pathname);
CL_DECLARE();
CL_DOCSTRING(R"dx(Returns data of the posix stat() function for pathname"
"if pathname is not found, returns nil"
"else mutiple values of"
"stat.st_size (file size in bytes, follows symbolic links)"
"stat.st_mtime (Time of last data modification.)"
"stat.st_mode (Mode of file))dx")
DOCGROUP(clasp);
CL_DEFUN T_mv ext__stat(T_sp pathname) {
  struct stat sb;
  String_sp filename = gc::As<String_sp>(cl__namestring(cl__translate_logical_pathname(pathname)));
  if (stat(filename->get_path_string().c_str(), &sb) == -1)
    return Values(nil<T_O>());
  else
    return Values(Integer_O::create(sb.st_size), Integer_O::create((gctools::Fixnum)sb.st_mtime),
                  Integer_O::create((gctools::Fixnum)sb.st_mode));
};

SYMBOL_EXPORT_SC_(ExtPkg, fstat);
SYMBOL_EXPORT_SC_(ExtPkg, stat);

}; // namespace core
