/*
    File: bundle.cc
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef _TARGET_OS_DARWIN
#include <libproc.h>
#endif

#include <stdlib.h>
#include <limits.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/bundle.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/str.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lisp.h>
#include <boost/filesystem.hpp>

namespace bf = boost_filesystem;

namespace core {

Bundle::Bundle() {
  this->_Initialized = false;
}

void Bundle::initializeStartupWorkingDirectory() {
  string cwd = "";
  boost_filesystem::path curPath;
  try {
    curPath = boost_filesystem::current_path();
  } catch (std::runtime_error &e) {
    printf("%s:%d - There was a problem getting the current_path - error[%s]\n",
           __FILE__, __LINE__, e.what());
    printf("     This appears to be a problem with boost_filesystem\n");
    printf("     - see https://svn.boost.org/trac/boost/ticket/4688\n");
    SIMPLE_ERROR(BF("There is a problem with boost_filesystem"));
  }
  cwd = curPath.string();
  this->_StartupWorkingDir = boost_filesystem::path(cwd);
}

void Bundle::initialize(const string &raw_argv0, const string &envVar) {
  pid_t pid = getpid();
#ifdef _TARGET_OS_DARWIN
  char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
  /*int ret = */ proc_pidpath(pid, pathbuf, sizeof(pathbuf));
  //        printf("%s:%d pid path = %s\n", __FILE__, __LINE__, pathbuf );
  string argv0 = string(pathbuf);
#endif
#ifdef _TARGET_OS_LINUX
  stringstream path;
  path << "/proc/" << pid << "/exe";
  char buffer[PATH_MAX + 1];
  char *rp = realpath(path.str().c_str(), buffer);
  if (!rp) {
    printf("%s:%d Could not resolve pid realpath for %s\n", __FILE__, __LINE__, path.str().c_str());
  }
  string argv0 = string(rp);
#endif
  string cwd = this->_StartupWorkingDir.string();
  bf::path appDir = this->findAppDir(argv0, cwd, envVar);
  // First crawl up the directory tree and look for the cando root
  bf::path curPath = appDir;
  while (curPath.has_relative_path()) {
    // The following line used to contain curPath.leaf().find("cando") but that
    // is weird because .leaf()[-now depreciated-] returned a path and not a string!!!!!
    // I changed it to what should work now but the behavior might have changed!!!!!!! Aug15-2011
    if (curPath.filename().string().find("clasp") != string::npos) {
      break;
    }
    curPath = curPath.branch_path();
  }
  if (!curPath.has_relative_path()) {
    THROW_HARD_ERROR(BF("Could not find the root directory of the cando application bundle.\n"
                        " It must contain the word \"clasp\".\n"));
  }
  this->_RootDir = curPath;
  this->findSubDirectories(curPath);
  this->_Initialized = true;
}

// Find the absolute path where this application has been run from.
// argv0 is wxTheApp->argv[0]
// cwd is the current working directory (at startup)
// appVariableName is the name of a variable containing the directory for this app, e.g.
// MYAPPDIR. This is checked first.

boost_filesystem::path Bundle::findAppDir(const string &argv0, const string &cwd, const string &appVariableName) {
  // Try appVariableName
  if (appVariableName != "") {
    char *cenv = getenv(appVariableName.c_str());
    if (cenv != NULL) {
      string str = cenv;
      boost_filesystem::path strPath(str);
      if (bf::exists(strPath)) {
        return strPath;
      }
    }
  }

#if 0 // defined(darwin)
	boost_filesystem::path cwdPath(cwd);
	LOG(BF("Using current working directory: path=%s") % cwdPath.string() );
	return cwdPath;
#endif

  boost_filesystem::path argv0Path(argv0);

  if (argv0Path.has_root_path()) {
    return argv0Path.branch_path();
  } else {
    boost_filesystem::path cwdPath(cwd);
    boost_filesystem::path absPath;

    absPath = cwdPath / argv0Path;
    if (bf::exists(absPath)) {
      return absPath.branch_path();
    }
  }

  // OK, it's neither an absolute path nor a relative path.
  // Search PATH.

  char *pc = getenv("PATH");
  if (pc == NULL) {
    THROW_HARD_ERROR(BF("PATH environment variable must be defined"));
  }
  string pathList = pc;
  VectorStrings pathParts;
  string argv0Extension;
#ifdef WIN32
  tokenize(pathList, pathParts, ";"); // Windows path separator char
  if (argv0.find(".exe") == string::npos) {
    argv0Extension = argv0 + ".exe";
  } else {
    argv0Extension = argv0;
  }
#else
  tokenize(pathList, pathParts, ":"); // Unix path separator char
  argv0Extension = argv0;
#endif
  for (VectorStrings::iterator it = pathParts.begin(); it != pathParts.end(); it++) {
    bf::path onePath(*it);
    onePath = onePath / argv0Extension;
    if (bf::exists(onePath)) {
      return onePath.branch_path();
    }
  }
  THROW_HARD_ERROR(BF("Could not determine absolute path to executable: " + argv0 + "\n" + " set environment variable(" + appVariableName + ") before running\n" + " or add application directory to PATH"));
}

void Bundle::findSubDirectories(boost_filesystem::path rootDir) {
  string appDirName;
#ifdef _TARGET_OS_DARWIN
  appDirName = "macos";
#else
  appDirName = "bin";
#endif
  ASSERTF(appDirName != "", BF("Could not figure out what OS this is running on in Bundle::findSubDirectories"));
  bf::recursive_directory_iterator dirs(rootDir);
  while (dirs != bf::recursive_directory_iterator()) {
    if (is_directory(dirs->path())) {
      string leaf = dirs->path().filename().string();
      int dirsSize = dirs->path().string().size();
      std::transform(leaf.begin(), leaf.end(), leaf.begin(), ::tolower);
      if (leaf == appDirName) {
        this->_AppDir = dirs->path();
      } else if (leaf == "resources" && (this->_ResourcesDir.empty() || (dirsSize < this->_ResourcesDir.string().size()))) {
        this->_ResourcesDir = dirs->path();
      } else if (leaf == "databases" && (this->_DatabasesDir.empty() || (dirsSize < this->_DatabasesDir.string().size()))) {
        this->_DatabasesDir = dirs->path();
      } else if (leaf == "lib" && (this->_LibDir.empty() || (dirsSize < this->_LibDir.string().size()))) {
        this->_LibDir = dirs->path();
      } else if (leaf == "clasp-src" && (this->_SourceDir.empty() || (dirsSize < this->_SourceDir.string().size()))) {
        this->_SourceDir = dirs->path();
      } else if (leaf == "include" && (this->_IncludeDir.empty() || (dirsSize < this->_IncludeDir.string().size()))) {
        this->_IncludeDir = dirs->path();
      } else if (leaf == "lisp" && (this->_LispDir.empty() || (dirsSize < this->_LispDir.string().size()))) {
        this->_LispDir = dirs->path();
      }
    }
    dirs++;
  }
  char *lispdir = getenv("CLASP_LISP_SOURCE_DIR");
  if (lispdir != NULL) {
    //	    printf("Using CLASP_LISP_SOURCE_DIR --> %s\n", lispdir );
    this->_LispDir = boost_filesystem::path(lispdir);
  }
}

string Bundle::describe() {
  _G();
  stringstream ss;
  ss << "Root dir:        " << this->_RootDir.string() << std::endl;
  ss << "Application dir: " << this->_AppDir.string() << std::endl;
  ss << "Resources dir:   " << this->_ResourcesDir.string() << std::endl;
  ss << "Databases dir:   " << this->_DatabasesDir.string() << std::endl;
  ss << "Scripts dir:     " << this->_ScriptsDir.string() << std::endl;
  ss << "Lisp dir:        " << this->_LispDir.string() << std::endl;
  ss << "Source dir:      " << this->_SourceDir.string() << std::endl;
  ss << "Include dir:      " << this->_IncludeDir.string() << std::endl;
  ss << "Lib dir:         " << this->_LibDir.string() << std::endl;
  return ss.str();
}

bf::path Bundle::getRootDir() {
  _OF();
  ASSERT(!this->_RootDir.empty());
  return this->_RootDir;
}

bf::path Bundle::getAppDir() {
  _OF();
  ASSERT(!this->_AppDir.empty());
  return this->_AppDir;
}

bf::path Bundle::getResourcesDir() {
  _OF();
  ASSERT(!this->_ResourcesDir.empty());
  return this->_ResourcesDir;
}

bf::path Bundle::getDatabasesDir() {
  _OF();
  ASSERT(!this->_DatabasesDir.empty());
  return this->_DatabasesDir;
}

bf::path Bundle::getScriptsDir() {
  return this->_ScriptsDir;
}

bf::path Bundle::getLispDir() {
  return this->_LispDir;
}

Pathname_sp Bundle::getSysPathname() {
  stringstream ss;
  ss << this->_LispDir.string();
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl_pathname(Str_O::create(ss.str()));
}

Pathname_sp Bundle::getSourcePathname() {
  stringstream ss;
  ss << this->_SourceDir.string();
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl_pathname(Str_O::create(ss.str()));
}

Pathname_sp Bundle::getIncludePathname() {
  stringstream ss;
  ss << this->_IncludeDir.string();
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl_pathname(Str_O::create(ss.str()));
}

Pathname_sp Bundle::getAppContentsResourcesPathname() {
  stringstream ss;
  ASSERT(!this->_RootDir.empty());
  ss << this->_RootDir.string();
  ss << DIR_SEPARATOR;
  ss << "Contents";
  ss << DIR_SEPARATOR;
  ss << "Resources";
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl_pathname(Str_O::create(ss.str()));
}

Pathname_sp Bundle::getAppContentsPathname() {
  stringstream ss;
  ASSERT(!this->_RootDir.empty());
  ss << this->_RootDir.string();
  ss << DIR_SEPARATOR;
  ss << "Contents";
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl_pathname(Str_O::create(ss.str()));
}

bf::path Bundle::getLibDir() {
  return this->_LibDir;
}

bf::path Bundle::getStartupWorkingDir() {
  return this->_StartupWorkingDir;
}
};
