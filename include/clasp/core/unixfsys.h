/*
    File: unixfsys.h
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
#ifndef core_unixfsys_H
#define core_unixfsys_H
#include <csignal>
#include <sys/select.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/pathname.fwd.h>

#if !defined(_TARGET_OS_DARWIN) && !defined(_TARGET_OS_FREEBSD)
#if !defined( FD_COPY )
#define FD_COPY(dest,src) memcpy((dest),(src),sizeof *(dest))
#endif
#endif

namespace core {

  enum SignalEnum {
      signal_SIGABRT = SIGABRT,
      signal_SIGALRM = SIGALRM,
      signal_SIGBUS = SIGBUS,
      signal_SIGCHLD = SIGCHLD,
      signal_SIGCONT = SIGCONT,
      signal_SIGFPE = SIGFPE,
      signal_SIGHUP = SIGHUP,
      signal_SIGILL = SIGILL,
      signal_SIGINT = SIGINT,
      signal_SIGKILL = SIGKILL,
      signal_SIGPIPE = SIGPIPE,
      signal_SIGQUIT = SIGQUIT,
      signal_SIGSEGV = SIGSEGV,
      signal_SIGSTOP = SIGSTOP,
      signal_SIGTERM = SIGTERM,
      signal_SIGTSTP = SIGTSTP,
      signal_SIGTTIN = SIGTTIN,
      signal_SIGTTOU = SIGTTOU,
      signal_SIGUSR1 = SIGUSR1,
      signal_SIGUSR2 = SIGUSR2,
//      signal_SIGPOLL = SIGPOLL,
      signal_SIGPROF = SIGPROF,
      signal_SIGSYS = SIGSYS,
      signal_SIGTRAP = SIGTRAP,
      signal_SIGURG = SIGURG,
      signal_SIGVTALRM = SIGVTALRM,
      signal_SIGXCPU = SIGXCPU,
      signal_SIGXFSZ = SIGXFSZ,
  };

  FORWARD(Sigset);
  class Sigset_O : public General_O {
    CL_DOCSTRING(R"(Wraps the unix sigset_t data type used to represent a signal set.)");
    LISP_CLASS(core, CorePkg, Sigset_O, "Sigset", General_O);
  public: // Simple default ctor/dtor
    Sigset_O();
  public: // instance variables here
    sigset_t _sigset;
  public: // Functions here
    int sigset_sigaddset(SignalEnum sym);
  }; // Sigset class

  FORWARD(FdSet);
  class FdSet_O : public General_O {
    CL_DOCSTRING(R"(Wraps the unix fdset data type used by select.)");
    LISP_CLASS(core, CorePkg, FdSet_O, "FdSet", General_O);
  public: // Simple default ctor/dtor
    FdSet_O();
  public: // instance variables here
    fd_set _fd_set;
  public: // Functions here
    
    void fd_clr_(int fd);
    void fd_set_(int fd);
    void fd_copy_(FdSet_sp copy);
    bool fd_isset_(int fd);
    void fd_zero_();
    
  }; // fdset class

  
}; // core namespace


namespace core {

Integer_sp clasp_file_len(int f);
int clasp_backup_open(const char *filename, int option, int mode);

String_sp core__current_dir();
Pathname_sp cl__truename(T_sp filespec);
T_sp cl__probe_file(T_sp filespec);
Symbol_sp core__file_kind(T_sp filename, bool follow_links = true);
T_mv af_renameFile(T_sp oldn, T_sp newn, T_sp if_exists = kw::_sym_supersede);
T_sp cl__delete_file(T_sp filespec);
String_sp clasp_strerror(int e);
bool clasp_has_file_position (int filedescriptor);
};

namespace ext {
core::Str8Ns_sp ext__getcwd();
};

DECLARE_ENUM_SYMBOL_TRANSLATOR(core::SignalEnum,core::_sym__PLUS_SignalEnumConverter_PLUS_);
#endif
