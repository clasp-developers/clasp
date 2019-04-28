/*
    File: bundle.h
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
#ifndef Bundle_H //[
#define Bundle_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/pathname.fwd.h>
#include <clasp/core/sourceFileInfo.fwd.h>
#include <boost/filesystem.hpp>

namespace core {

  struct BundleDirectories;
  
/*! Maintains the file paths to the different directories of the Cando bundle
 */
class Bundle {
#if defined(USE_MPS)
  friend mps_res_t globals_scan(mps_ss_t ss, void *p, size_t s);
#endif

private:
  bool _Initialized;
  BundleDirectories* _Directories;

public:
  /*! Initialize the bundle and set up all the paths
     */
private:
  boost_filesystem::path findAppDir(const string &argv0, const string &cwd, bool verbose=false);
  void findContentSubDirectories(boost_filesystem::path p, bool verbose=false);
  void fillInMissingPaths(bool verbose=false);

public:
  void initializeStartupWorkingDirectory(bool verbose=false);

  Pathname_sp getRootPathname();
  Pathname_sp getExecutablePathname();
  Pathname_sp getSysPathname();
  Pathname_sp getIncludePathname();
  Pathname_sp getSourcePathname();
  Pathname_sp getAppContentsPathname();
  Pathname_sp getAppContentsResourcesPathname();

  string describe();
  Bundle(const string &argv0, const string &appPath);

  void setup_pathname_translations();
  
  virtual ~Bundle(){};
};

/*! Get the system bundle */
//extern	Bundle&	bundle();
};
#endif //]
