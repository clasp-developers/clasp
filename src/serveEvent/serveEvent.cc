/*
    File: serveEvent.cc
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

#include <errno.h>
#include <sys/select.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/symbolTable.h>
#include <clasp/serveEvent/serveEventPackage.h>
#include <clasp/core/wrappers.h>

namespace serveEvent {

using namespace core;

CL_DEFUN void serve_event_internal__ll_fd_zero(core::ForeignData_sp fdset) {
  FD_ZERO(fdset->data<fd_set *>());
}

CL_DEFUN void serve_event_internal__ll_fd_set(int fd, core::ForeignData_sp fdset) {
  FD_SET(fd, fdset->data<fd_set *>());
}

CL_DEFUN int serve_event_internal__ll_fd_isset(int fd, core::ForeignData_sp fdset) {
  return FD_ISSET(fd, fdset->data<fd_set *>());
}

CL_DEFUN int serve_event_internal__ll_fdset_size() {
  return sizeof(fd_set);
}

CL_DEFUN core::Integer_mv serve_event_internal__ll_serveEventNoTimeout(core::ForeignData_sp rfd, core::ForeignData_sp wfd, int maxfdp1) {
  gc::Fixnum selectRet = select(maxfdp1, rfd->data<fd_set *>(), wfd->data<fd_set *>(), NULL, NULL);
  return Values(Integer_O::create(selectRet), Integer_O::create((gc::Fixnum)errno));
}

CL_DEFUN core::Integer_mv serve_event_internal__ll_serveEventWithTimeout(core::ForeignData_sp rfd, core::ForeignData_sp wfd, int maxfdp1, double seconds) {
  if (seconds < 0.0) {
    SIMPLE_ERROR(BF("Illegal timeout %lf seconds") % seconds);
  }
  struct timeval tv;
  tv.tv_sec = seconds;
  tv.tv_usec = ((seconds - floor(seconds)) * 1e6);
  gc::Fixnum selectRet = select(maxfdp1, rfd->data<fd_set *>(), wfd->data<fd_set *>(), NULL, &tv);
  return Values(Integer_O::create(selectRet), Integer_O::create((gc::Fixnum)errno));
}

void initialize_serveEvent_globals() {
  SYMBOL_EXPORT_SC_(ServeEventPkg, _PLUS_EINTR_PLUS_);
  _sym__PLUS_EINTR_PLUS_->defconstant(Integer_O::create((gc::Fixnum)EINTR));
};


  SYMBOL_EXPORT_SC_(ServeEventPkg, ll_fd_zero);
  SYMBOL_EXPORT_SC_(ServeEventPkg, ll_fd_set);
  SYMBOL_EXPORT_SC_(ServeEventPkg, ll_fd_isset);
  SYMBOL_EXPORT_SC_(ServeEventPkg, ll_fdset_size);
  SYMBOL_EXPORT_SC_(ServeEventPkg, ll_serveEventNoTimeout);
  SYMBOL_EXPORT_SC_(ServeEventPkg, ll_serveEventWithTimeout);

};
