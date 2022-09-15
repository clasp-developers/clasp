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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef _TARGET_OS_DARWIN
#include <libproc.h>
#endif

#include <clasp/core/array.h>
#include <clasp/core/bundle.h>
#include <clasp/core/lisp.h>
#include <clasp/core/object.h>
#include <clasp/core/pathname.h>
#include <clasp/core/sourceFileInfo.h>
#include <filesystem>
#include <limits.h>
#include <stdlib.h>

namespace core {

bool safe_is_directory(const std::filesystem::path &path) {
  try {
    return std::filesystem::is_directory(path);
  } catch (...) {
    SIMPLE_ERROR(("The std::filesystem::is_directory(%s) call threw a C++ exception"), path.string().c_str());
  }
}

Bundle::Bundle(const string &raw_argv0) {
  this->_Initialized = false;
  this->_Directories = NULL;
  bool verbose = false;
  if (getenv("CLASP_VERBOSE_BUNDLE_SETUP") != NULL) {
    verbose = true;
  }
  this->_Directories = new BundleDirectories();
  this->_Directories->_StartupWorkingDir = std::filesystem::current_path();
  this->findExecutableDir(this->deduceArgv0(raw_argv0), verbose);
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
    this->_Directories->_SysDir = (this->_Directories->_ExecutableDir / CLASP_DEV_SYS_PATH).lexically_normal();
  } else if (homedir) {
    if (verbose) {
      printf("%s:%d Using the CLASP_HOME environment variable %s\n", __FILE__, __LINE__, homedir);
    }
    this->_Directories->_SysDir = std::filesystem::path(homedir);
  }
  if (this->_Directories->_SysDir.empty()) {
    if (verbose) {
      printf("%s:%d Using the configured installed directories\n", __FILE__, __LINE__);
    }
    this->_Directories->_SysDir = CLASP_INSTALL_SYS_PATH;
    this->_Directories->_GeneratedDir = CLASP_INSTALL_GENERATED_PATH;
    this->_Directories->_IncludeDir = CLASP_INSTALL_INCLUDE_PATH;
    this->_Directories->_LibDir = CLASP_INSTALL_LIB_PATH;
  } else {
    this->_Directories->_GeneratedDir = (this->_Directories->_SysDir / CLASP_DEV_GENERATED_PATH).lexically_normal();
    this->_Directories->_IncludeDir = (this->_Directories->_SysDir / CLASP_DEV_INCLUDE_PATH).lexically_normal();
    this->_Directories->_LibDir = (this->_Directories->_SysDir / CLASP_DEV_LIB_PATH).lexically_normal();
  }

#ifdef DEBUG_DESC_BUNDLE
  printf("%s\n", this->describe().c_str());
  printf("%s:%d Aborting for now\n", __FILE__, __LINE__);
#endif
  // Setup the quicklisp dir - if none is available then leave _QuicklispDir empty and don't create a hostname
  if (verbose)
    printf("%s:%d   Starting to look for quicklisp\n", __FILE__, __LINE__);
  const char *quicklisp_env = getenv("CLASP_QUICKLISP_DIRECTORY");
  if (quicklisp_env) {
    std::filesystem::path env_path(quicklisp_env);
    if (std::filesystem::exists(env_path)) {
      if (safe_is_directory(std::filesystem::path(quicklisp_env))) {
        if (verbose)
          printf("%s:%d   Found path %s\n", __FILE__, __LINE__, quicklisp_env);
        this->_Directories->_QuicklispDir = std::filesystem::path(quicklisp_env);
      }
    }
  } else {
    bool gotQuicklispPath = false;
    // Try "sys:src;lisp;modules;quicklisp;"
    std::filesystem::path modules_quicklisp = this->_Directories->_SysDir / "modules" / "quicklisp";
    if (verbose)
      printf("%s:%d   Looking in modulesat %s\n", __FILE__, __LINE__, modules_quicklisp.string().c_str());
    if (std::filesystem::exists(modules_quicklisp)) {
      if (safe_is_directory(modules_quicklisp)) {
        if (!getenv("ASDF_OUTPUT_TRANSLATIONS")) {
          if (!global_options->_SilentStartup) {
            printf("%s:%d Found %s so setting ASDF_OUTPUT_TRANSLATIONS to /:\n", __FILE__, __LINE__,
                   modules_quicklisp.string().c_str());
          }
          //          setenv("ASDF_OUTPUT_TRANSLATIONS","/:",1);
        }
        gotQuicklispPath = true;
        if (verbose)
          printf("%s:%d   Found %s\n", __FILE__, __LINE__, modules_quicklisp.string().c_str());
        this->_Directories->_QuicklispDir = modules_quicklisp;
      }
    }
    if (!gotQuicklispPath) {
      // Try $HOME/quicklisp
      const char *home_dir = getenv("HOME");
      std::stringstream sdir;
      if (home_dir) {
        std::filesystem::path quicklispPath(home_dir);
        quicklispPath = quicklispPath / "quicklisp";
        if (verbose)
          printf("%s:%d  Looking for %s\n", __FILE__, __LINE__, quicklispPath.string().c_str());
        if (std::filesystem::exists(quicklispPath)) {
          if (safe_is_directory(quicklispPath)) {
            if (verbose)
              printf("%s:%d  Found %s\n", __FILE__, __LINE__, quicklispPath.string().c_str());
            this->_Directories->_QuicklispDir = quicklispPath;
            gotQuicklispPath = true;
          }
        }
      }
    }
  }
  if (verbose) {
    printf("%s:%d  Final bundle setup:\n", __FILE__, __LINE__);
    printf("%s\n", this->describe().c_str());
  }
}

std::string Bundle::deduceArgv0(const std::string &raw_argv0) {
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
  return raw_argv0;
}

// Find the absolute path where this application has been run from.
// argv0 is wxTheApp->argv[0]
// cwd is the current working directory (at startup)
// appVariableName is the name of a variable containing the directory for this app, e.g.
// MYAPPDIR. This is checked first.

void Bundle::findExecutableDir(const string &argv0, bool verbose) {
  if (verbose) {
    printf("%s:%d In findExecutableDir argv0: %s\n", __FILE__, __LINE__, argv0.c_str());
  }

  std::filesystem::path argv0Path(argv0);
  if (argv0Path.has_root_path()) {
    if (verbose)
      printf("%s:%d has_root_path: %s\n", __FILE__, __LINE__, argv0Path.parent_path().string().c_str());
    this->_Directories->_ExecutableDir = argv0Path.parent_path();
    return;
  }

  std::filesystem::path absPath = this->_Directories->_StartupWorkingDir / argv0Path;
  if (std::filesystem::exists(absPath)) {
    if (verbose)
      printf("%s:%d absPath.parent_path(): %s\n", __FILE__, __LINE__, absPath.parent_path().string().c_str());
    this->_Directories->_ExecutableDir = absPath.parent_path();
    return;
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
      if (verbose)
        printf("%s:%d onePath.parent_path(): %s\n", __FILE__, __LINE__, onePath.parent_path().string().c_str());
      this->_Directories->_ExecutableDir = onePath.parent_path();
      return;
    }
  }
  THROW_HARD_ERROR("Could not determine absolute path to executable: %s", argv0);
}

string Bundle::describe() {
  stringstream ss;
  ss << "Executable dir: " << this->_Directories->_ExecutableDir << std::endl
     << "Sys dir:        " << this->_Directories->_SysDir.string() << std::endl
     << "Generated dir:  " << this->_Directories->_GeneratedDir.string() << std::endl
     << "Lib dir:        " << this->_Directories->_LibDir.string() << std::endl
     << "Include dir:    " << this->_Directories->_IncludeDir.string() << std::endl
     << "Quicklisp dir:  " << this->_Directories->_QuicklispDir.string() << std::endl;
  return ss.str();
}

void create_translation(const std::filesystem::path &path, const std::string &translation) {
  if (!path.empty()) {
    T_sp name = SimpleBaseString_O::make("SYS");
    cl__setf_logical_pathname_translations(
        Cons_O::create(Cons_O::createList(SimpleBaseString_O::make("SYS:" + translation),
                                              cl__pathname(SimpleBaseString_O::make(path / "**" / "*.*"))),
                        cl__logical_pathname_translations(name)),
        name);
  }
}

void Bundle::setup_pathname_translations() {
  create_translation(this->_Directories->_SysDir, "**;*.*.*");
  create_translation(this->_Directories->_GeneratedDir, "GENERATED;**;*.*.*");
  create_translation(this->_Directories->_LibDir, "LIB;**;*.*.*");
  create_translation(this->_Directories->_ExecutableDir, "EXECUTABLE;**;*.*.*");
  create_translation(this->_Directories->_QuicklispDir, "QUICKLISP;**;*.*.*");
}

}; // namespace core
