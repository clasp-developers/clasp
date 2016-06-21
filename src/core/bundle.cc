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
#include <clasp/core/foundation.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef _TARGET_OS_DARWIN
#include <libproc.h>
#endif

#include <stdlib.h>
#include <limits.h>
#include <clasp/core/object.h>
#include <clasp/core/bundle.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/str.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lisp.h>
#include <boost/filesystem.hpp>

namespace bf = boost_filesystem;

namespace core {

struct BundleDirectories {
  boost_filesystem::path _StartupWorkingDir;
  boost_filesystem::path _ExecutableDir;
  boost_filesystem::path _CxxBitcodeDir;
  boost_filesystem::path _ContentsDir;
  boost_filesystem::path _ResourcesDir;
  boost_filesystem::path _LispSourceDir;
  boost_filesystem::path _LispGeneratedDir;
  boost_filesystem::path _LispBuildDir;
  boost_filesystem::path _IncludeDir;
  boost_filesystem::path _LibDir;
  boost_filesystem::path _DatabasesDir;
};

Bundle::Bundle() {
  this->_Initialized = false;
  this->_Directories = NULL;
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
  this->_Directories->_StartupWorkingDir = boost_filesystem::path(cwd);
}


void Bundle::initialize(const string &raw_argv0, const string &envVar) {
  this->_Directories = new BundleDirectories();
  this->initializeStartupWorkingDirectory();
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
  string cwd = this->_Directories->_StartupWorkingDir.string();
  bf::path appDir = this->findAppDir(argv0, cwd, envVar);
  // First crawl up the directory tree and look for the cando root
  bf::path curPath = appDir;
  // Climb up one level
  this->_Directories->_ExecutableDir = curPath;
#ifdef DEBUG_BUNDLE
  printf("%s:%d Bundle::initialize raw_argv0: %s\n", __FILE__, __LINE__, raw_argv0.c_str());
  printf("%s:%d Bundle::initialize argv0: %s\n", __FILE__, __LINE__, argv0.c_str());
  printf("%s:%d    _ExecutableDir = %s\n", __FILE__, __LINE__, this->_Directories->_ExecutableDir.string().c_str());
  printf("%s:%d    _StartupWorkingDir = %s\n", __FILE__, __LINE__, this->_Directories->_StartupWorkingDir.string().c_str());
#endif
  curPath = curPath.branch_path();
  if (!curPath.has_relative_path()) {
    THROW_HARD_ERROR(BF("Could not find the root directory of the cando application bundle.\n"
                        " It must contain the word \"clasp\".\n"));
  }
  // Check if there is a 'src' directory in _ExecutableDir - if so we are building
  bf::path srcPath = this->_Directories->_ExecutableDir / "src";
  if (bf::exists(srcPath)) {
    this->_Directories->_CxxBitcodeDir = this->_Directories->_ExecutableDir;
#ifdef DEBUG_BUNDLE
    printf("%s:%d Looking for Content subdirectories for building\n", __FILE__, __LINE__ );
#endif
    this->findContentSubDirectories(srcPath);
    bf::path contents = this->_Directories->_ExecutableDir / "Contents";
    this->_Directories->_ContentsDir = contents;
    this->findContentSubDirectories(this->_Directories->_ContentsDir);
    this->fillInMissingPaths();
  } else {
#ifdef DEBUG_BUNDLE
    printf("%s:%d Find Contents elsewhere\n", __FILE__, __LINE__ );
#endif
//  this->_Directories->_RootDir = curPath;
    bf::path one_up_contents = this->_Directories->_ExecutableDir.parent_path();
    one_up_contents /= std::string("Contents");
    this->_Directories->_ContentsDir = one_up_contents;
    this->findContentSubDirectories(one_up_contents);
    this->fillInMissingPaths();
  }
#ifdef DEBUG_BUNDLE
  printf("%s\n", this->describe().c_str());
  printf("%s:%d Aborting for now\n", __FILE__, __LINE__ );
#endif
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

void Bundle::findContentSubDirectories(boost_filesystem::path contentDir) {
  if ( !bf::exists(contentDir) ) {
    bf::create_directories(contentDir);
  }
  string appDirName;
#ifdef _TARGET_OS_DARWIN
  appDirName = "macos";
#else
  appDirName = "bin";
#endif
  ASSERTF(appDirName != "", BF("Could not figure out what OS this is running on in Bundle::findSubDirectories"));
  {
    bf::recursive_directory_iterator dirs(contentDir);
    while (dirs != bf::recursive_directory_iterator()) {
      if (is_directory(dirs->path())) {
        string leaf = dirs->path().filename().string();
        int dirsSize = dirs->path().string().size();
        std::transform(leaf.begin(), leaf.end(), leaf.begin(), ::tolower);
        if (leaf == "resources" && (this->_Directories->_ResourcesDir.empty())) {
          this->_Directories->_ResourcesDir = dirs->path();
        } else if (leaf == "lib" && (this->_Directories->_LibDir.empty())) {
          this->_Directories->_LibDir = dirs->path();
        } else if (leaf == "cxx-bitcode" && (this->_Directories->_CxxBitcodeDir.empty())) {
          this->_Directories->_CxxBitcodeDir = dirs->path();
        } else if (leaf == "include" && (this->_Directories->_IncludeDir.empty())) {
          this->_Directories->_IncludeDir = dirs->path();
        } else if (leaf == "lisp" && (this->_Directories->_LispSourceDir.empty())) {
          this->_Directories->_LispSourceDir = dirs->path();
        } else if (leaf == "generated" && (this->_Directories->_LispGeneratedDir.empty() )) {
          this->_Directories->_LispGeneratedDir = dirs->path();
        } else if (leaf == "lisp-build" && (this->_Directories->_LispBuildDir.empty() )) {
          this->_Directories->_LispBuildDir = dirs->path();
        }
      }
      dirs++;
    }
  }
  char *lispdir = getenv("CLASP_LISP_SOURCE_DIR");
  if (lispdir != NULL) {
    //	    printf("Using CLASP_LISP_SOURCE_DIR --> %s\n", lispdir );
    this->_Directories->_LispSourceDir = boost_filesystem::path(lispdir);
  }
}


void Bundle::fillInMissingPaths() {
  if ( !bf::exists(this->_Directories->_ContentsDir) ) {
    bool created = bf::create_directory(this->_Directories->_ContentsDir);
  }
  if ( this->_Directories->_ResourcesDir.empty() ) {
    this->_Directories->_ResourcesDir = this->_Directories->_ContentsDir / "Resources";
    bool created = bf::create_directory(this->_Directories->_ResourcesDir);
  }
  if ( this->_Directories->_LispBuildDir.empty() ) {
    this->_Directories->_LispBuildDir = this->_Directories->_ResourcesDir / "lisp-build";
    bf::create_directory(this->_Directories->_LispBuildDir);
  }
}


string Bundle::describe() {
  stringstream ss;
  ss << "ExecutableDir:     " << this->_Directories->_ExecutableDir.string() << std::endl;
  ss << "CxxBitcodeDir:     " << this->_Directories->_CxxBitcodeDir.string() << std::endl;
  ss << "Contents dir:      " << this->_Directories->_ContentsDir.string() << std::endl;
  ss << "Resources dir:     " << this->_Directories->_ResourcesDir.string() << std::endl;
  ss << "Databases dir:     " << this->_Directories->_DatabasesDir.string() << std::endl;
  ss << "Lisp source dir:   " << this->_Directories->_LispSourceDir.string() << std::endl;
  ss << "Lisp generated dir:" << this->_Directories->_LispGeneratedDir.string() << std::endl;
  ss << "LispBuild dir:     " << this->_Directories->_LispBuildDir.string() << std::endl;
  ss << "Include dir:       " << this->_Directories->_IncludeDir.string() << std::endl;
  ss << "Lib dir:           " << this->_Directories->_LibDir.string() << std::endl;
  return ss.str();
}


Pathname_sp generate_pathname(const boost::filesystem::path& path)
{
  stringstream ss;
  ss << path.string();
  ss << DIR_SEPARATOR;
  ss << "**";
  ss << DIR_SEPARATOR;
  ss << "*.*";
  return cl__pathname(Str_O::create(ss.str()));
}


Pathname_sp Bundle::getIncludePathname() {
  return generate_pathname(this->_Directories->_IncludeDir);
}

Pathname_sp Bundle::getAppContentsResourcesPathname() {
  stringstream ss;
  ASSERT(!this->_Directories->_ContentsDir.empty());
  ss << this->_Directories->_ContentsDir.string();
  ss << DIR_SEPARATOR;
  ss << "Resources";
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl__pathname(Str_O::create(ss.str()));
}

void Bundle::setup_pathname_translations()
{
      // setup the SYS logical-pathname-translations
  {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(Str_O::create("sys:**;*.*"),
                                            generate_pathname(this->_Directories->_LispSourceDir))
        /* ,  more here */
                         );
    core__pathname_translations(Str_O::create("sys"), _lisp->_true(), pts);
  }
      // setup the LISP-SOURCE logical-pathname-translations
  {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(Str_O::create("LISP-SOURCE:**;*.*"),
                                            generate_pathname(this->_Directories->_LispSourceDir)));
    core__pathname_translations(Str_O::create("LISP-SOURCE"), _lisp->_true(), pts);
  }
  {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(Str_O::create("LISP-GENERATED:**;*.*"),
                                            generate_pathname(this->_Directories->_LispGeneratedDir)));
    core__pathname_translations(Str_O::create("LISP-GENERATED"), _lisp->_true(), pts);
  }
  {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(Str_O::create("LISP-BUILD:**;*.*"),
                                            generate_pathname(this->_Directories->_LispBuildDir)));
    core__pathname_translations(Str_O::create("LISP-BUILD"), _lisp->_true(), pts);
  }
  {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(Str_O::create("CXX-BITCODE:**;*.*"),
                                            generate_pathname(this->_Directories->_CxxBitcodeDir)));
    core__pathname_translations(Str_O::create("CXX-BITCODE"), _lisp->_true(), pts);
  }
    // setup the TMP logical-pathname-translations
  Cons_sp entryTmp = Cons_O::createList(Str_O::create("tmp:**;*.*"),
                                        cl__pathname(Str_O::create("/tmp/**/*.*")));
  Cons_sp ptsTmp = Cons_O::createList(entryTmp
                                        /* ,  more here */
                                      );
  core__pathname_translations(Str_O::create("tmp"), _lisp->_true(), ptsTmp);

            // setup the APP-EXECUTABLE logical-pathname-translations
  {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(Str_O::create("app-executable:**;*.*"),
                                            generate_pathname(this->_Directories->_ExecutableDir)));
    core__pathname_translations(Str_O::create("app-executable"), _lisp->_true(), appc);
  }

    // setup the APP-CONTENTS logical-pathname-translations
  {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(Str_O::create("app-contents:**;*.*"),
                                            generate_pathname(this->_Directories->_ContentsDir)));
    core__pathname_translations(Str_O::create("app-contents"), _lisp->_true(), appc);
  }
  {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(Str_O::create("app-resources:**;*.*"),
                                            generate_pathname(this->_Directories->_ResourcesDir)));
    core__pathname_translations(Str_O::create("app-resources"), _lisp->_true(), appc);
  }
}

};
