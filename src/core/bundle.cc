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
//#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>


//#define DEBUG_DESC_BUNDLE


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
#include <clasp/core/array.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lisp.h>
#include <boost/filesystem.hpp>

namespace bf = boost_filesystem;

namespace core {


struct BundleDirectories {
  boost_filesystem::path _StartupWorkingDir;
  boost_filesystem::path _ExecutableDir;
  boost_filesystem::path _ContentsDir;
  boost_filesystem::path _ResourcesDir;
  boost_filesystem::path _LispSourceDir;
  boost_filesystem::path _GeneratedDir;
  boost_filesystem::path _SourceDir;
  boost_filesystem::path _IncludeDir;
  boost_filesystem::path _LibDir;
  boost_filesystem::path _DatabasesDir;
  boost_filesystem::path _FaslDir;
  boost_filesystem::path _BitcodeDir;
};

Bundle::Bundle(const string &raw_argv0, const string &appDirName) {
//  printf("%s:%d ---------- Initializing Bundle\n", __FILE__, __LINE__);
  this->_Initialized = false;
  this->_Directories = NULL;
  bool verbose = false;
  if (getenv("CLASP_VERBOSE_BUNDLE_SETUP") != NULL) {
    verbose = true;
  }
  this->_Directories = new BundleDirectories();
  this->initializeStartupWorkingDirectory(verbose);
  bf::path appDir;
  if (verbose) {
    printf("%s:%d Bundle appDirName = %s\n", __FILE__, __LINE__, appDirName.c_str() );
  }
  if (appDirName.size() == 0) {
    pid_t pid = getpid();
    // fixme, cracauer.  This should be passed in from main()
    // /proc lookup can fail.  /proc is not always mounted or available,
    // e.g. in chroot situations.
    string argv0 = "";
#ifdef _TARGET_OS_DARWIN
    char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
    /*int ret = */ proc_pidpath(pid, pathbuf, sizeof(pathbuf));
  //        printf("%s:%d pid path = %s\n", __FILE__, __LINE__, pathbuf );
    argv0 = string(pathbuf);
#endif
#ifdef _TARGET_OS_FREEBSD
    stringstream path;
    path << "/proc/" << pid << "/file";
    char buffer[PATH_MAX + 1];
    char *rp = realpath(path.str().c_str(), buffer);
    if (!rp) {
      printf("%s:%d Could not resolve pid realpath for %s\n", __FILE__, __LINE__, path.str().c_str());
    } else {
      argv0 = string(rp);
    }
#endif
#ifdef _TARGET_OS_LINUX
    stringstream path;
    path << "/proc/" << pid << "/exe";
    char buffer[PATH_MAX + 1];
    char *rp = realpath(path.str().c_str(), buffer);
    if (!rp) {
      printf("%s:%d Could not resolve pid realpath for %s\n", __FILE__, __LINE__, path.str().c_str());
    } else {
      argv0 = string(rp);
    }
#endif
    string cwd = this->_Directories->_StartupWorkingDir.string();
    appDir = this->findAppDir(argv0, cwd);
  } else {
    appDir = bf::path(appDirName);
  }
  // First crawl up the directory tree and look for the cando root
  // Climb up one level
  this->_Directories->_ExecutableDir = appDir;
  if (verbose) {
    printf("%s:%d _ExecutableDir = %s\n", __FILE__, __LINE__, this->_Directories->_ExecutableDir.string().c_str());
  }
#ifdef DEBUG_DESC_BUNDLE
  printf("%s:%d Bundle::initialize raw_argv0: %s\n", __FILE__, __LINE__, raw_argv0.c_str());
  printf("%s:%d Bundle::initialize argv0: %s\n", __FILE__, __LINE__, argv0.c_str());
  printf("%s:%d    _ExecutableDir = %s\n", __FILE__, __LINE__, this->_Directories->_ExecutableDir.string().c_str());
  printf("%s:%d    _StartupWorkingDir = %s\n", __FILE__, __LINE__, this->_Directories->_StartupWorkingDir.string().c_str());
#endif
  appDir = appDir.branch_path();
  if (verbose) {
    printf("%s:%d   Climb one level up from _ExecutablePath = %s\n", __FILE__, __LINE__, appDir.string().c_str());
  }

  // Check if there is a 'src' directory in _ExecutableDir - if so we are building
  bf::path srcPath = this->_Directories->_ExecutableDir / "src";
  bool foundContents = false;
  if (bf::exists(srcPath)) {
    if (verbose) {
      printf("%s:%d   In development environment - found src path = %s\n", __FILE__, __LINE__, srcPath.string().c_str());
    }
    this->_Directories->_ContentsDir = this->_Directories->_ExecutableDir.parent_path().parent_path();
    foundContents = true;
  } else {
    if (verbose) {
      printf("%s:%d   Did not find src dir\n", __FILE__, __LINE__ );
    }
#ifdef DEBUG_DESC_BUNDLE
    printf("%s:%d Find Contents elsewhere\n", __FILE__, __LINE__ );
#endif
//  this->_Directories->_RootDir = appDir;
    bf::path one_up_contents = this->_Directories->_ExecutableDir.parent_path();
    one_up_contents = one_up_contents / "lib" / "clasp";
    if (bf::exists(one_up_contents)) {
      this->_Directories->_ContentsDir = one_up_contents;
      if (verbose) {
        printf("%s:%d   Set _ContentsDir = %s\n", __FILE__, __LINE__, this->_Directories->_ContentsDir.string().c_str());
      }
      foundContents = true;
    } else {
      if (verbose) {
        printf("%s:%d  Could not find the Contents/clasp library directory - searching...\n", __FILE__, __LINE__ );
      }
      char *homedir = getenv("CLASP_HOME");
      if (homedir) {
        if (verbose) printf("%s:%d  Using the CLASP_HOME directory %s\n", __FILE__, __LINE__, homedir);
        this->_Directories->_ContentsDir = bf::path(homedir) / "lib" / "clasp";
        foundContents = true;
      } else {
        std::string install_path = std::string(PREFIX)+"/lib/clasp";
        bf::path test_path(install_path);
        if (bf::exists(test_path)) {
          if (verbose) printf("%s:%d  Looking for %s\n", __FILE__, __LINE__, install_path.c_str());
          this->_Directories->_ContentsDir = test_path;
          foundContents = true;
        }
        const char* paths[] = { "/usr/local/lib/clasp/Contents",
                                "/usr/lib/clasp/Contents" };
        if (!foundContents) {
          for ( size_t i=0; i<sizeof(paths)/sizeof(paths[0]); ++i ) {
            bf::path test_path(paths[i]);
            if (bf::exists(test_path)) {
              if (verbose) printf("%s:%d  Looking for %s\n", __FILE__, __LINE__, paths[i]);
              this->_Directories->_ContentsDir = test_path;
              foundContents = true;
              break;
            }
          }
        }
      }
    }
  }
  if (foundContents) {
    if (verbose) {
      printf("%s:%d   Set _ContentsDir = %s\n", __FILE__, __LINE__, this->_Directories->_ContentsDir.string().c_str());
    }
  } else {
    printf("%s:%d The ContentsDir could not be found\n", __FILE__, __LINE__ );
    abort();
  }
#if defined(USE_BOEHM)
  #if defined(_DEBUG_BUILD)
     std::string target = "boehm_d";
  #else
     std::string target = "boehm";
  #endif
#elif defined(USE_MPS)
  #if defined(_DEBUG_BUILD)
    std::string target = "mps_d";
  #else
    std::string target = "mps";
  #endif
#else
#error "There must be a target - only boehm and mps are supported"
#endif
  this->_Directories->_LibDir = this->_Directories->_ContentsDir / "build" / target / "fasl";
  if (verbose) {
    printf("%s:%d   Set _LibDir = %s\n", __FILE__, __LINE__, this->_Directories->_LibDir.string().c_str());
  }

  this->_Directories->_SourceDir = this->_Directories->_ContentsDir;
  if (verbose) printf("%s:%d Setting up _SourceDir = %s\n", __FILE__, __LINE__, this->_Directories->_SourceDir.string().c_str());
  this->_Directories->_IncludeDir = this->_Directories->_ContentsDir / "include";
  if (verbose) printf("%s:%d Setting up _IncludeDir = %s\n", __FILE__, __LINE__, this->_Directories->_IncludeDir.string().c_str());
  this->_Directories->_LispSourceDir = this->_Directories->_ContentsDir / "src" / "lisp";
  if (verbose) printf("%s:%d Setting up _LispSourceDir = %s\n", __FILE__, __LINE__, this->_Directories->_LispSourceDir.string().c_str());
  this->_Directories->_GeneratedDir = this->_Directories->_ContentsDir / "build" / target / "generated";
  if (verbose) {printf("%s:%d Setting up _GeneratedDir = %s\n", __FILE__, __LINE__, this->_Directories->_GeneratedDir.string().c_str());}
  this->_Directories->_FaslDir = this->_Directories->_LibDir;
  if (verbose) {printf("%s:%d Setting up _FaslDir = %s\n", __FILE__, __LINE__, this->_Directories->_FaslDir.string().c_str());}
  this->_Directories->_BitcodeDir = this->_Directories->_LibDir;
  if (verbose) {printf("%s:%d Setting up _BitcodeDir = %s\n", __FILE__, __LINE__, this->_Directories->_BitcodeDir.string().c_str());}
  if (verbose) {
    printf("%s:%d  Final bundle setup:\n", __FILE__, __LINE__ );
    printf("%s\n", this->describe().c_str());
  }
#ifdef DEBUG_DESC_BUNDLE
  printf("%s\n", this->describe().c_str());
  printf("%s:%d Aborting for now\n", __FILE__, __LINE__ );
#endif
}

void Bundle::initializeStartupWorkingDirectory(bool verbose) {
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
  if ( verbose ) {
    printf("%s:%d  _StartupWorkingDir = %s\n", __FILE__, __LINE__, this->_Directories->_StartupWorkingDir.string().c_str() );
  }
}



// Find the absolute path where this application has been run from.
// argv0 is wxTheApp->argv[0]
// cwd is the current working directory (at startup)
// appVariableName is the name of a variable containing the directory for this app, e.g.
// MYAPPDIR. This is checked first.

boost_filesystem::path Bundle::findAppDir( const string &argv0, const string &cwd) {
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
  THROW_HARD_ERROR(BF("Could not determine absolute path to executable: %s") % argv0 );
}



string Bundle::describe() {
  stringstream ss;
  ss << "ExecutableDir:   " << this->_Directories->_ExecutableDir.string() << std::endl;
  ss << "Lib dir:         " << this->_Directories->_LibDir.string() << std::endl;
  ss << "Contents dir:    " << this->_Directories->_ContentsDir.string() << std::endl;
  ss << "Resources dir:   " << this->_Directories->_ResourcesDir.string() << std::endl;
  ss << "Lisp source dir: " << this->_Directories->_LispSourceDir.string() << std::endl;
  ss << "Source dir:      " << this->_Directories->_SourceDir.string() << std::endl;
  ss << "Generated dir:   " << this->_Directories->_GeneratedDir.string() << std::endl;
  ss << "Include dir:     " << this->_Directories->_IncludeDir.string() << std::endl;
  ss << "Fasl dir:        " << this->_Directories->_FaslDir.string() << std::endl;
  ss << "Bitcode dir:     " << this->_Directories->_BitcodeDir.string() << std::endl;
  ss << "Databases dir:   " << this->_Directories->_DatabasesDir.string() << std::endl;
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
  return cl__pathname(SimpleBaseString_O::make(ss.str()));
}


Pathname_sp Bundle::getIncludePathname() {
  return generate_pathname(this->_Directories->_IncludeDir);
}

Pathname_sp Bundle::getAppContentsResourcesPathname() {
  stringstream ss;
  ASSERT(!this->_Directories->_ContentsDir.empty());
  ss << this->_Directories->_ContentsDir.string();
  ss << DIR_SEPARATOR;
  ss << "lib";
  ss << DIR_SEPARATOR;
  ss << "**/*.*";
  return cl__pathname(SimpleBaseString_O::make(ss.str()));
}

void Bundle::setup_pathname_translations()
{
      // setup the SYS logical-pathname-translations
  if ( !this->_Directories->_LispSourceDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("sys:**;*.*"),
                                            generate_pathname(this->_Directories->_LispSourceDir))
        /* ,  more here */
                         );
    core__pathname_translations(SimpleBaseString_O::make("sys"), _lisp->_true(), pts);
  }
      // setup the LISP-SOURCE logical-pathname-translations
  if ( !this->_Directories->_LispSourceDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("LISP-SOURCE:**;*.*"),
                                            generate_pathname(this->_Directories->_LispSourceDir)));
    core__pathname_translations(SimpleBaseString_O::make("LISP-SOURCE"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_SourceDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("SOURCE-DIR:**;*.*"),
                                            generate_pathname(this->_Directories->_SourceDir)));
    core__pathname_translations(SimpleBaseString_O::make("SOURCE-DIR"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_GeneratedDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("GENERATED:**;*.*"),
                                            generate_pathname(this->_Directories->_GeneratedDir)));
    core__pathname_translations(SimpleBaseString_O::make("GENERATED"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_LibDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("LIB:**;*.*"),
                                            generate_pathname(this->_Directories->_LibDir)));
    core__pathname_translations(SimpleBaseString_O::make("LIB"), _lisp->_true(), pts);
  }
    // setup the TMP logical-pathname-translations
  Cons_sp entryTmp = Cons_O::createList(SimpleBaseString_O::make("tmp:**;*.*"),
                                        cl__pathname(SimpleBaseString_O::make("/tmp/**/*.*")));
  Cons_sp ptsTmp = Cons_O::createList(entryTmp
                                        /* ,  more here */
                                      );
  core__pathname_translations(SimpleBaseString_O::make("tmp"), _lisp->_true(), ptsTmp);

            // setup the APP-EXECUTABLE logical-pathname-translations
  {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("app-executable:**;*.*"),
                                            generate_pathname(this->_Directories->_ExecutableDir)));
    core__pathname_translations(SimpleBaseString_O::make("app-executable"), _lisp->_true(), appc);
  }

            // setup the APP-FASL logical-pathname-translations
  {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("app-fasl:**;*.*"),
                                            generate_pathname(this->_Directories->_FaslDir)));
    core__pathname_translations(SimpleBaseString_O::make("app-fasl"), _lisp->_true(), appc);
  }
            // setup the APP-BITCODE logical-pathname-translations
  {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("app-bitcode:**;*.*"),
                                            generate_pathname(this->_Directories->_BitcodeDir)));
    core__pathname_translations(SimpleBaseString_O::make("app-bitcode"), _lisp->_true(), appc);
  }
  
    // setup the APP-CONTENTS logical-pathname-translations
  if ( !this->_Directories->_ContentsDir.empty() ) {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("app-contents:**;*.*"),
                                            generate_pathname(this->_Directories->_ContentsDir)));
    core__pathname_translations(SimpleBaseString_O::make("app-contents"), _lisp->_true(), appc);
  }
  if ( !this->_Directories->_ResourcesDir.empty() ) {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("app-resources:**;*.*"),
                                            generate_pathname(this->_Directories->_ResourcesDir)));
    core__pathname_translations(SimpleBaseString_O::make("app-resources"), _lisp->_true(), appc);
  }
}

};
