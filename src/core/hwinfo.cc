/*
    File: hwinfo.cc
*/

/*
Copyright (c) 2018, Christian E. Schafmeister

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
#include <clasp/core/foundation.h>
#include <clasp/core/hwinfo.h>
#include <clasp/core/wrappers.h>

#if defined( _WIN32 ) || defined( _TARGET_OS_WIN )
#include <windows.h>
#endif

#if defined( __APPLE__ ) || defined( _TARGET_OS_DARWIN )
#include <sys/param.h>
#include <sys/sysctl.h>
#endif

#if !defined( _WIN32 ) && !defined( _TARGET_OS_WIN  )
#include <unistd.h>
#endif


namespace core {

CL_DOCSTRING("num-logical-processors: returns the nr of logical processors in the system.");
CL_DEFUN T_sp num_logical_processors() {

#if defined( _WIN32 ) || defined( _TARGET_OS_WIN )

  SYSTEM_INFO systeminfo;
  GetSystemInfo( &systeminfo );

  return core::make_fixnum( systeminfo.dwNumberOfProcessors );

#else

  return core::make_fixnum( sysconf( _SC_NPROCESSORS_ONLN ) );

#endif
};

} // namespace
