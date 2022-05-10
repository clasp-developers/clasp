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

#include <unistd.h>
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
#include <filesystem>

namespace core {


bool safe_is_directory(const std::filesystem::path& path) {
  try {
    return std::filesystem::is_directory(path);
  } catch (...) {
    SIMPLE_ERROR(("The std::filesystem::is_directory(%s) call threw a C++ exception") , path.string().c_str() );
  }
}

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
  std::filesystem::path appDir;
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
    appDir = this->findAppDir(argv0, cwd,verbose);
    if ( verbose ) printf("%s:%d Using this->findAppDir(...) appDir = %s\n", __FILE__, __LINE__, appDir.string().c_str() );
  } else {
    appDir = std::filesystem::path(appDirName);
    if ( verbose ) printf("%s:%d Using appDirName appDir = %s\n", __FILE__, __LINE__, appDir.string().c_str() );
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
  // Check if there is a dev test directory in _ExecutableDir - if so we are building
  std::filesystem::path devPath = this->_Directories->_ExecutableDir / CLASP_DEV_TEST_PATH;
  char *homedir = getenv("CLASP_HOME");
  if (std::filesystem::exists(devPath)) {
    if (verbose) {
      printf("%s:%d In development environment, found = %s\n", __FILE__, __LINE__, devPath.string().c_str());
    }
    this->_Directories->_SourceDir = (this->_Directories->_ExecutableDir / CLASP_DEV_SOURCE_PATH).lexically_normal();
  } else if (homedir) {
    if (verbose) {
      printf("%s:%d Using the CLASP_HOME environment variable %s\n", __FILE__, __LINE__, homedir);
    }
    this->_Directories->_SourceDir = std::filesystem::path(homedir);
  }
  if (this->_Directories->_SourceDir.empty()) {
    if (verbose) {
      printf("%s:%d Using the configured installed directories\n", __FILE__, __LINE__);
    }
    this->_Directories->_SourceDir = CLASP_INSTALL_SOURCE_PATH;
    this->_Directories->_BitcodeDir = CLASP_INSTALL_BITCODE_PATH;
    this->_Directories->_FaslDir = CLASP_INSTALL_FASL_PATH;
    this->_Directories->_GeneratedDir = CLASP_INSTALL_GENERATED_PATH;
    this->_Directories->_StartupDir = CLASP_INSTALL_STARTUP_PATH;
    this->_Directories->_IncludeDir = CLASP_INSTALL_INCLUDE_PATH;
    this->_Directories->_LibDir = CLASP_INSTALL_LIB_PATH;
    this->_Directories->_SysDir = CLASP_INSTALL_SYS_PATH;
  } else {
    this->_Directories->_BitcodeDir = (this->_Directories->_SourceDir / CLASP_DEV_BITCODE_PATH).lexically_normal();
    this->_Directories->_FaslDir = (this->_Directories->_SourceDir / CLASP_DEV_FASL_PATH).lexically_normal();
    this->_Directories->_GeneratedDir = (this->_Directories->_SourceDir / CLASP_DEV_GENERATED_PATH).lexically_normal();
    this->_Directories->_StartupDir = (this->_Directories->_SourceDir / CLASP_DEV_STARTUP_PATH).lexically_normal();
    this->_Directories->_IncludeDir = (this->_Directories->_SourceDir / CLASP_DEV_INCLUDE_PATH).lexically_normal();
    this->_Directories->_LibDir = (this->_Directories->_SourceDir / CLASP_DEV_LIB_PATH).lexically_normal();
    this->_Directories->_SysDir = (this->_Directories->_SourceDir / CLASP_DEV_SYS_PATH).lexically_normal();
  }

#ifdef DEBUG_DESC_BUNDLE
  printf("%s\n", this->describe().c_str());
  printf("%s:%d Aborting for now\n", __FILE__, __LINE__ );
#endif
  // Setup the quicklisp dir - if none is available then leave _QuicklispDir empty and don't create a hostname
  if (verbose) printf("%s:%d   Starting to look for quicklisp\n", __FILE__, __LINE__);
  const char* quicklisp_env = getenv("CLASP_QUICKLISP_DIRECTORY");
  if (quicklisp_env) {
    std::filesystem::path env_path(quicklisp_env);
    if (std::filesystem::exists(env_path)) {
      if (safe_is_directory(std::filesystem::path(quicklisp_env))) {
        if (verbose) printf("%s:%d   Found path %s\n", __FILE__, __LINE__, quicklisp_env);
        this->_Directories->_QuicklispDir = std::filesystem::path(quicklisp_env);
      }
    }
  } else {
    bool gotQuicklispPath = false;
    // Try "sys:modules;quicklisp;"
    std::filesystem::path modules_quicklisp = this->_Directories->_SysDir / "modules" / "quicklisp";
    if (verbose) printf("%s:%d   Looking in modulesat %s\n", __FILE__, __LINE__, modules_quicklisp.string().c_str());
    if (std::filesystem::exists(modules_quicklisp)) {
      if (safe_is_directory(modules_quicklisp)) {
        if (!getenv("ASDF_OUTPUT_TRANSLATIONS")) {
          if (!global_options->_SilentStartup) {
            printf("%s:%d Found %s so setting ASDF_OUTPUT_TRANSLATIONS to /:\n", __FILE__, __LINE__, modules_quicklisp.string().c_str() );
          }
//          setenv("ASDF_OUTPUT_TRANSLATIONS","/:",1);
        }
        gotQuicklispPath = true;
        if (verbose) printf("%s:%d   Found %s\n", __FILE__, __LINE__, modules_quicklisp.string().c_str());
        this->_Directories->_QuicklispDir = modules_quicklisp;
      }
    }
    if (!gotQuicklispPath) {
    // Try $HOME/quicklisp
      const char* home_dir = getenv("HOME");
      std::stringstream sdir;
      if (home_dir) {
        std::filesystem::path quicklispPath(home_dir);
        quicklispPath = quicklispPath / "quicklisp";
        if (verbose) printf("%s:%d  Looking for %s\n", __FILE__, __LINE__, quicklispPath.string().c_str() );
        if (std::filesystem::exists(quicklispPath)) {
          if (safe_is_directory(quicklispPath)) {
        // printf("%s:%d  ~/quicklisp/ exists\n", __FILE__, __LINE__);
            if (verbose) printf("%s:%d  Found %s\n", __FILE__, __LINE__, quicklispPath.string().c_str() );
            this->_Directories->_QuicklispDir = quicklispPath;
            gotQuicklispPath = true;
          }
        }
      }
    }
  }
  if (verbose) {
    printf("%s:%d  Final bundle setup:\n", __FILE__, __LINE__ );
    printf("%s\n", this->describe().c_str());
  }
}
void Bundle::initializeStartupWorkingDirectory(bool verbose) {
  string cwd = "";
  std::filesystem::path curPath;
  try {
    curPath = std::filesystem::current_path();
  } catch (std::runtime_error &e) {
    printf("%s:%d - There was a problem getting the current_path - error[%s]\n",
           __FILE__, __LINE__, e.what());
    printf("     This appears to be a problem with std::filesystem\n");
    printf("     - see https://svn.boost.org/trac/boost/ticket/4688\n");
    SIMPLE_ERROR(("There is a problem with std::filesystem"));
  }
  cwd = curPath.string();
  this->_Directories->_StartupWorkingDir = std::filesystem::path(cwd);
  if ( verbose ) {
    printf("%s:%d  _StartupWorkingDir = %s\n", __FILE__, __LINE__, this->_Directories->_StartupWorkingDir.string().c_str() );
  }
}



// Find the absolute path where this application has been run from.
// argv0 is wxTheApp->argv[0]
// cwd is the current working directory (at startup)
// appVariableName is the name of a variable containing the directory for this app, e.g.
// MYAPPDIR. This is checked first.

std::filesystem::path Bundle::findAppDir( const string &argv0, const string &cwd, bool verbose) {
  if (verbose) {
    printf("%s:%d In findAppDir argv0: %s\n", __FILE__, __LINE__, argv0.c_str() );
    printf("%s:%d In findAppDir cwd: %s\n", __FILE__, __LINE__, cwd.c_str() );
  }
  std::filesystem::path argv0Path(argv0);
  if (argv0Path.has_root_path()) {
    if (verbose) printf("%s:%d has_root_path: %s\n", __FILE__, __LINE__, argv0Path.parent_path().string().c_str() );
    return argv0Path.parent_path();
  } else {
    std::filesystem::path cwdPath(cwd);
    std::filesystem::path absPath;
    
    absPath = cwdPath / argv0Path;
    if (std::filesystem::exists(absPath)) {
      if (verbose) printf("%s:%d absPath.parent_path(): %s\n", __FILE__, __LINE__, absPath.parent_path().string().c_str() );
      return absPath.parent_path();
    }
  }
  // OK, it's neither an absolute path nor a relative path.
  // Search PATH.
  char *pc = getenv("PATH");
  if (pc == NULL) {
    THROW_HARD_ERROR("PATH environment variable must be defined");
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
    std::filesystem::path onePath(*it);
    onePath = onePath / argv0Extension;
    if (std::filesystem::exists(onePath)) {
      if (verbose) printf("%s:%d onePath.parent_path(): %s\n", __FILE__, __LINE__, onePath.parent_path().string().c_str() );
      return onePath.parent_path();
    }
  }
  THROW_HARD_ERROR("Could not determine absolute path to executable: %s", argv0 );
}


string Bundle::describe() {
  stringstream ss;
  ss << "ExecutableDir: " << this->_Directories->_ExecutableDir.string() << std::endl
     << "Lib dir:       " << this->_Directories->_LibDir.string() << std::endl
     << "Resources dir: " << this->_Directories->_ResourcesDir.string() << std::endl
     << "Sys dir:       " << this->_Directories->_SysDir.string() << std::endl
     << "Source dir:    " << this->_Directories->_SourceDir.string() << std::endl
     << "Generated dir: " << this->_Directories->_GeneratedDir.string() << std::endl
     << "Startup dir:   " << this->_Directories->_StartupDir.string() << std::endl
     << "Include dir:   " << this->_Directories->_IncludeDir.string() << std::endl
     << "Fasl dir:      " << this->_Directories->_FaslDir.string() << std::endl
     << "Bitcode dir:   " << this->_Directories->_BitcodeDir.string() << std::endl
     << "Databases dir: " << this->_Directories->_DatabasesDir.string() << std::endl
     << "Quicklisp dir: " << this->_Directories->_QuicklispDir.string() << std::endl;
  return ss.str();
}


Pathname_sp generate_pathname(const std::filesystem::path& path)
{
  stringstream ss;
  ss << path.string();
  ss << DIR_SEPARATOR;
  ss << "**";
  ss << DIR_SEPARATOR;
  ss << "*.*";
  return cl__pathname(SimpleBaseString_O::make(ss.str()));
}

void Bundle::setup_pathname_translations()
{
  // setup the SYS logical-pathname-translations
  if ( !this->_Directories->_SysDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("sys:**;*.*"),
                                            generate_pathname(this->_Directories->_SysDir))
        /* ,  more here */
                         );
    core__pathname_translations(SimpleBaseString_O::make("sys"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_SourceDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("source-dir:**;*.*"),
                                            generate_pathname(this->_Directories->_SourceDir)));
    core__pathname_translations(SimpleBaseString_O::make("source-dir"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_GeneratedDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("generated:**;*.*"),
                                            generate_pathname(this->_Directories->_GeneratedDir)));
    core__pathname_translations(SimpleBaseString_O::make("generated"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_StartupDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("startup:**;*.*"),
                                            generate_pathname(this->_Directories->_StartupDir)));
    core__pathname_translations(SimpleBaseString_O::make("startup"), _lisp->_true(), pts);
  }
  if ( !this->_Directories->_LibDir.empty() ) {
    Cons_sp pts =
      Cons_O::createList(
                         Cons_O::createList(SimpleBaseString_O::make("lib:**;*.*"),
                                            generate_pathname(this->_Directories->_LibDir)));
    core__pathname_translations(SimpleBaseString_O::make("lib"), _lisp->_true(), pts);
  }
  // setup the TMP logical-pathname-translations
  Cons_sp entryTmp = Cons_O::createList(SimpleBaseString_O::make("tmp:**;*.*"),
                                        cl__pathname(SimpleBaseString_O::make("/tmp/**/*.*")));
  Cons_sp ptsTmp = Cons_O::createList(entryTmp
                                        /* ,  more here */
                                      );
  core__pathname_translations(SimpleBaseString_O::make("tmp"), _lisp->_true(), ptsTmp);

  // Setup hostname pathname translations
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
  
  if ( !this->_Directories->_ResourcesDir.empty() ) {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("app-resources:**;*.*"),
                                            generate_pathname(this->_Directories->_ResourcesDir)));
    core__pathname_translations(SimpleBaseString_O::make("app-resources"), _lisp->_true(), appc);
  }
  if ( !this->_Directories->_QuicklispDir.empty() ) {
    Cons_sp appc =
      Cons_O::createList(Cons_O::createList(SimpleBaseString_O::make("quicklisp:**;*.*"),
                                            generate_pathname(this->_Directories->_QuicklispDir)));
    core__pathname_translations(SimpleBaseString_O::make("quicklisp"), _lisp->_true(), appc);
  }
}

};
