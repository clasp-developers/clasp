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
#include <clasp/serveEvent/symbolTable.h>
#include <clasp/serveEvent/serveEventPackage.h>
#include <clasp/core/wrappers.h>


namespace serveEvent {


    using namespace core;




#define ARGS_af_ll_fd_zero "(fdset)"
#define DECL_af_ll_fd_zero ""
#define DOCS_af_ll_fd_zero "ll_fd_zero"
    void af_ll_fd_zero(core::ForeignData_sp fdset)
    {_G();
	FD_ZERO(fdset->data<fd_set*>());
    }



#define ARGS_af_ll_fd_set "(fd fdset)"
#define DECL_af_ll_fd_set ""
#define DOCS_af_ll_fd_set "ll_fd_set"
    void af_ll_fd_set(int fd, core::ForeignData_sp fdset)
    {_G();
	FD_SET(fd,fdset->data<fd_set*>());
    }

	     
	     
#define ARGS_af_ll_fd_isset "(fd fdset)"
#define DECL_af_ll_fd_isset ""
#define DOCS_af_ll_fd_isset "ll_fd_isset"
    int af_ll_fd_isset(int fd, core::ForeignData_sp fdset)
    {_G();
	return FD_ISSET(fd,fdset->data<fd_set*>());
    }





	     
	     
#define ARGS_af_ll_fdset_size "()"
#define DECL_af_ll_fdset_size ""
#define DOCS_af_ll_fdset_size "ll_fdset_size"
    int af_ll_fdset_size()
    {_G();
	return sizeof(fd_set);
    }



  
  
#define ARGS_af_ll_serveEventNoTimeout "(rfd wfd maxfdp1)"
#define DECL_af_ll_serveEventNoTimeout ""
#define DOCS_af_ll_serveEventNoTimeout "ll_serveEventNoTimeout"
    core::Integer_mv af_ll_serveEventNoTimeout(core::ForeignData_sp rfd, core::ForeignData_sp wfd, int maxfdp1)
    {_G();
	int selectRet = select(maxfdp1, rfd->data<fd_set*>(), wfd->data<fd_set*>(), NULL, NULL );
	return Values(Integer_O::create(selectRet),Integer_O::create(errno));
    }

		
		
#define ARGS_af_ll_serveEventWithTimeout "(rfd wfd maxfdp1 seconds)"
#define DECL_af_ll_serveEventWithTimeout ""
#define DOCS_af_ll_serveEventWithTimeout "ll_serveEventWithTimeout"
    Integer_mv af_ll_serveEventWithTimeout(core::ForeignData_sp rfd, core::ForeignData_sp wfd, int maxfdp1, double seconds)
    {_G();
	if ( seconds < 0.0 ) {
	    SIMPLE_ERROR(BF("Illegal timeout %lf seconds") % seconds);
	}
	struct timeval tv;
	tv.tv_sec = seconds;
	tv.tv_usec = ((seconds-floor(seconds)) * 1e6);
	int selectRet = select(maxfdp1, rfd->data<fd_set*>(), wfd->data<fd_set*>(), NULL,&tv);
	return Values(Integer_O::create(selectRet),Integer_O::create(errno));
    }






    void initialize_serveEvent_globals()
    {
	SYMBOL_EXPORT_SC_(ServeEventPkg,_PLUS_EINTR_PLUS_);
	_sym__PLUS_EINTR_PLUS_->defconstant(Integer_O::create(EINTR));
    };



    void initialize_serveEvent_functions()
    {

	SYMBOL_EXPORT_SC_(ServeEventPkg,ll_fd_zero);
	Defun(ll_fd_zero);
	SYMBOL_EXPORT_SC_(ServeEventPkg,ll_fd_set);
	Defun(ll_fd_set);
	SYMBOL_EXPORT_SC_(ServeEventPkg,ll_fd_isset);
	Defun(ll_fd_isset);
	SYMBOL_EXPORT_SC_(ServeEventPkg,ll_fdset_size);
	Defun(ll_fdset_size);
	SYMBOL_EXPORT_SC_(ServeEventPkg,ll_serveEventNoTimeout);
	Defun(ll_serveEventNoTimeout);
	SYMBOL_EXPORT_SC_(ServeEventPkg,ll_serveEventWithTimeout);
	Defun(ll_serveEventWithTimeout);
    };



};
