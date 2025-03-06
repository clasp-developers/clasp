
#include <clasp/core/foundation.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <pthread.h> // TODO: PORTING - frgo, 2017-08-04
#include <signal.h>  // TODO: PORTING - frgo, 2017-08-04

#ifndef _MSC_VER
#include <unistd.h>
#else
#include <io.h>
#include <direct.h>
#define access _access
#define F_OK 0
typedef int mode_t;
#endif

#include <sys/types.h>

#include <pwd.h>

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#else
#include <uuid/uuid.h>
#endif

#include <sys/stat.h>
#include <stdlib.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#else
#if !defined(_MSC_VER)
#include <sys/dir.h>
#endif
#endif
#if defined(CLASP_MS_WINDOWS_HOST)
#include <windows.h>
#undef ERROR
#endif
#include <fcntl.h>
#include <errno.h>

#include <clasp/core/pathname.h>
#include <clasp/core/array.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/character.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bformat.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispList.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/primitives.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/wrappers.h>

SYMBOL_EXPORT_SC_(KeywordPkg, default);
SYMBOL_EXPORT_SC_(KeywordPkg, stream);

namespace core {

/* Mingw defines 'environ' to be a macro instead of a global variable. */
#ifdef environ
#undef environ
#endif

T_sp clasp_system(T_sp cmd_string) {
#if !defined(HAVE_SYSTEM)
  FElibc_error("clasp_system not implemented", 1);
  return nil<T_O>();
#else
  std::string cmd = gc::As<String_sp>(cmd_string)->get_std_string();
  int code = system((const char*)(cmd.c_str()));
  return clasp_make_fixnum(code);
#endif
}

T_sp clasp_getuid(void) {
#if defined(ECL_MS_WINDOWS_HOST)
  @(return ecl_make_fixnum(0));
#else
  return clasp_make_integer(getuid());
#endif
}

T_sp clasp_make_pipe() {
#if defined(NACL)
  FElibc_error("clasp_make_pipe not implemented", 1);
  @(return ECL_NIL);
#else
  T_sp output;
  int fds[2], ret;
#if defined(ECL_MS_WINDOWS_HOST)
  ret = _pipe(fds, 4096, _O_BINARY);
#else
  ret = pipe(fds);
#endif
  if (ret < 0) {
    FElibc_error("Unable to create pipe", 0);
    output = nil<T_O>();
  } else {
    T_sp fake_in_name = SimpleBaseString_O::make("PIPE-READ-ENDPOINT");
    T_sp fake_out_name = SimpleBaseString_O::make("PIPE-WRITE-ENDPOINT");
    T_sp in = CFileStream_O::make(fake_in_name, fds[0], StreamDirection::input, 8, CLASP_STREAM_DEFAULT_FORMAT, nil<T_O>());
    T_sp out = CFileStream_O::make(fake_out_name, fds[1], StreamDirection::output, 8, CLASP_STREAM_DEFAULT_FORMAT, nil<T_O>());
    output = TwoWayStream_O::make(in, out);
  }
  return output;
#endif
}

SYMBOL_EXPORT_SC_(KeywordPkg, abort);
SYMBOL_EXPORT_SC_(KeywordPkg, aborted);
SYMBOL_EXPORT_SC_(KeywordPkg, exited);
SYMBOL_EXPORT_SC_(KeywordPkg, signaled);
SYMBOL_EXPORT_SC_(KeywordPkg, stopped);
SYMBOL_EXPORT_SC_(KeywordPkg, resumed);
SYMBOL_EXPORT_SC_(KeywordPkg, running);

static void from_list_to_execve_argument(T_sp l, char*** environp) {
  T_sp p;
  cl_index i, j, total_size = 0, nstrings = 0;
  SimpleBaseString_sp buffer;
  char** environ;
  for (p = l; p.notnilp(); p = CONS_CDR(p)) {
    T_sp s = CONS_CAR(p);
    total_size += cl__length(gc::As<String_sp>(s)) + 1;
    nstrings++;
  }
  /* Extra place for ending null */
  total_size++;

  environ = (char**)malloc((nstrings + 1) * sizeof(char*));
  for (j = i = 0, p = l; p.notnilp(); p = CONS_CDR(p)) {
    T_sp s = CONS_CAR(p);
    std::string ss = gc::As<String_sp>(s)->get_std_string();
    cl_index l = ss.size();
    environ[j] = (char*)malloc(l + 1);
    memcpy(environ[j], ss.c_str(), l);
    j++;
    environ[j][l] = 0;
  }
  environ[j] = 0;
  if (environp)
    *environp = environ;
}

T_mv clasp_waitpid(T_sp pid, T_sp wait) {
  T_sp status, code;
#if defined(NACL)
  FElibc_error("clasp_waitpid not implemented", 1);
  @(return ECL_NIL);
#elif defined(ECL_MS_WINDOWS_HOST)
  cl_env_ptr the_env = ecl_process_env();
  HANDLE* hProcess = ecl_foreign_data_pointer_safe(pid);
  DWORD exitcode;
  int ok;
  WaitForSingleObject(*hProcess, Null(wait) ? 0 : INFINITE);
  ecl_disable_interrupts_env(the_env);
  ok = GetExitCodeProcess(*hProcess, &exitcode);
  if (!ok) {
    status = @':error';
    code = ECL_NIL;
  } else if (exitcode == STILL_ACTIVE) {
    status = @':running';
    code = ECL_NIL;
  } else {
    status = @':exited';
    code = ecl_make_fixnum((int)exitcode);
    pid->foreign.data = NULL;
    CloseHandle(*hProcess);
  }
  ecl_enable_interrupts_env(the_env);
#else
  int code_int, error;

  if (wait.nilp())
    error = waitpid(pid.unsafe_fixnum(), &code_int, WNOHANG | WUNTRACED | WCONTINUED);
  else
    error = waitpid(pid.unsafe_fixnum(), &code_int, 0);

  if (error < 0) {
    if (errno == EINTR) {
      status = kw::_sym_abort;
    } else {
      status = kw::_sym_error;
    }
    code = nil<T_O>();
    pid = nil<T_O>();
  } else if (error == 0) {
    status = nil<T_O>();
    code = nil<T_O>();
    pid = nil<T_O>();
  } else {
    pid = clasp_make_fixnum(error);
    if (WIFEXITED(code_int)) {
      status = kw::_sym_exited;
      code = clasp_make_fixnum(WEXITSTATUS(code_int));
    } else if (WIFSIGNALED(code_int)) {
      status = kw::_sym_signaled;
      code = clasp_make_fixnum(WTERMSIG(code_int));
    } else if (WIFSTOPPED(code_int)) {
      status = kw::_sym_stopped;
      code = clasp_make_fixnum(WSTOPSIG(code_int));
    } else if (WIFCONTINUED(code_int)) {
      status = kw::_sym_resumed;
      code = clasp_make_fixnum(SIGCONT);
    } else {
      status = kw::_sym_running;
      code = nil<T_O>();
    }
  }
#endif
  return Values(status, code, pid);
}

CL_LAMBDA(pid wait);
CL_DECLARE();
CL_DOCSTRING(R"dx(waitpid - see unix waitpid - returns status)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__waitpid(T_sp pid, T_sp wait) { return clasp_waitpid(pid, wait); };

#if !defined(ECL_MS_WINDOWS_HOST)
CL_DEFUN T_sp sys__killpid(T_sp pid, T_sp signal) {
  int ret = kill(pid.unsafe_fixnum(), signal.unsafe_fixnum());
  return clasp_make_fixnum(ret);
}
#endif

void describe_fildes(int fildes, const char* name) {
  struct stat info;
  [[maybe_unused]] int fstat_error = fstat(fildes, &info);
  int fdflags;
  if ((fdflags = fcntl(fildes, F_GETFL, 0)) < 0)
    perror("fcntl failed");
  printf("%s:%d name: %s filedes: %d  fdflags = %d\n", __FUNCTION__, __LINE__, name, fildes, fdflags);
}

#if defined(ECL_MS_WINDOWS_HOST)
static void create_descriptor(T_sp stream, T_sp direction, HANDLE* child, int* parent) {
  SECURITY_ATTRIBUTES attr;
  HANDLE current = GetCurrentProcess();
  attr.nLength = sizeof(SECURITY_ATTRIBUTES);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  if (stream == @':stream') {
    /* Creates a pipe that we can write to and the child reads
       from. We duplicate one extreme of the pipe so that the child
       does not inherit it. */
    HANDLE tmp;
    if (direction == @':input') {
      if (CreatePipe(child, &tmp, &attr, 0) == 0)
        return;
      if (DuplicateHandle(current, tmp, current, &tmp, 0, FALSE, DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS) == 0)
        return;

      *parent = _open_osfhandle((intptr_t)tmp, _O_WRONLY);
    } else /* if (direction == @':output') */ {
      if (CreatePipe(&tmp, child, &attr, 0) == 0)
        return;
      if (DuplicateHandle(current, tmp, current, &tmp, 0, FALSE, DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS) == 0)
        return;

      *parent = _open_osfhandle((intptr_t)tmp, _O_RDONLY);
    }

    if (*parent < 0)
      printf("open_osfhandle failed\n");
  } else if (!Null(cl_streamp(stream))) {
    HANDLE stream_handle = ecl_stream_to_HANDLE(stream, direction != @':input');
    if (stream_handle == INVALID_HANDLE_VALUE) {
      CEerror(ecl_make_constant_base_string("Create a new stream.", -1),
              "~S argument to RUN-PROGRAM does not have a file handle:~%~S", 2, direction, stream);
      create_descriptor(@':stream', direction, child, parent);
      return;
    }
    DuplicateHandle(current, stream_handle, current, child, 0, TRUE, DUPLICATE_SAME_ACCESS);
  } else {
    FEerror("Invalid ~S argument to EXT:RUN-PROGRAM.", 1, stream);
  }
}
#else
static void create_descriptor(T_sp stream, T_sp direction, int* child, int* parent) {
  if (stream == kw::_sym_stream) {
    int fd[2], ret;
    ret = pipe(fd);
    if (ret != 0) {
      FElibc_error("Unable to create pipe", 0);
    }
    if (direction == kw::_sym_input) {
      *parent = fd[1];
      *child = fd[0];
    } else {
      *parent = fd[0];
      *child = fd[1];
    }
  } else if (cl__streamp(stream)) {
    *child = stream_file_descriptor(stream, (direction == kw::_sym_input) ? StreamDirection::input : StreamDirection::output);
    if (*child >= 0) {
      *child = dup(*child);
    } else {
      CEerror(SimpleBaseString_O::make("Create a new stream."), "~S argument to RUN-PROGRAM does not have a file handle:~%~S",
              2, direction, stream);
      create_descriptor(kw::_sym_stream, direction, child, parent);
      return;
    }
  } else {
    FEerror("Invalid ~S argument to EXT:RUN-PROGRAM.", 1, stream);
  }
}
#endif

SYMBOL_EXPORT_SC_(ClPkg, coerce);

CL_DEFUN
T_mv sys__spawn_subprocess(T_sp command, T_sp argv, T_sp environ, T_sp input, T_sp output, T_sp error) {
  int parent_write = 0, parent_read = 0, parent_error = 0;
  int child_pid;
  T_sp pid;

  /* environ is either a list or `:default'. */
  if (cl__listp(environ)) {
    environ = cl__mapcar(_sym_copy_to_simple_base_string, environ);
  } else if (environ != kw::_sym_default) {
    FEerror("Malformed :ENVIRON argument to EXT:RUN-PROGRAM.", 0);
  }

#if defined(ECL_MS_WINDOWS_HOST)
  {
    BOOL ok;
    STARTUPINFO st_info;
    PROCESS_INFORMATION pr_info;
    HANDLE child_stdout, child_stdin, child_stderr;
    HANDLE current = GetCurrentProcess();
    T_sp env_buffer;
    char* env = NULL;

    if (ECL_LISTP(environ)) {
      env_buffer = from_list_to_execve_argument(environ, NULL);
      env = env_buffer->base_string.self;
    }
    create_descriptor(input, @':input', &child_stdin, &parent_write);
    create_descriptor(output, @':output', &child_stdout, &parent_read);
    if (error == @':output') {
      /* The child inherits a duplicate of its own output handle. */
      DuplicateHandle(current, child_stdout, current, &child_stderr, 0, TRUE, DUPLICATE_SAME_ACCESS);
      /* Same for the parent_read and parent_error. */
      parent_error = dup(parent_read);
    } else
      create_descriptor(error, @':output', &child_stderr, &parent_error);

    ZeroMemory(&st_info, sizeof(STARTUPINFO));
    st_info.cb = sizeof(STARTUPINFO);
    st_info.lpTitle = NULL;                                        /* No window title, just exec name */
    st_info.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW; /* Specify std{in,out,err} */
    st_info.wShowWindow = SW_HIDE;
    st_info.hStdInput = child_stdin;
    st_info.hStdOutput = child_stdout;
    st_info.hStdError = child_stderr;
    ZeroMemory(&pr_info, sizeof(PROCESS_INFORMATION));

    /* Command is passed as is from argv. It is responsibility of
       higher level interface to decide, whenever arguments should be
       quoted or left as-is. */
    /* ecl_null_terminated_base_string(argv); */
    ok = CreateProcess(NULL, argv->base_string.self, NULL, NULL, /* lpProcess/ThreadAttributes */
                       TRUE,                                     /* Inherit handles (for files) */
                       /*CREATE_NEW_CONSOLE |*/
                       0 /*(input == ECL_T || output == ECL_T || error == ECL_T ? 0 : CREATE_NO_WINDOW)*/,
                       env,       /* Inherit environment */
                       NULL,      /* Current directory */
                       &st_info,  /* Startup info */
                       &pr_info); /* Process info */

    /* Child handles must be closed in the parent process */
    /* otherwise the created pipes are never closed       */
    if (ok) {
      CloseHandle(pr_info.hThread);
      pid = make_windows_handle(pr_info.hProcess);
    } else {
      char* message;
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER, 0, GetLastError(), 0, (void*)&message, 0, NULL);
      printf("%s\n", message);
      LocalFree(message);
      pid = nil<T_O>();
    }
    if (child_stdin)
      CloseHandle(child_stdin);
    if (child_stdout)
      CloseHandle(child_stdout);
    if (child_stderr)
      CloseHandle(child_stderr);
  }
#elif !defined(NACL) /* All POSIX but NaCL/pNaCL */
  {
    ql::list ll;
    ll << nil<T_O>();
    int child_stdin, child_stdout, child_stderr;
    argv = clasp_nconc(argv, ll.cons());
    argv = eval::funcall(cl::_sym_coerce, argv, cl::_sym_vector);

    create_descriptor(input, kw::_sym_input, &child_stdin, &parent_write);
    create_descriptor(output, kw::_sym_output, &child_stdout, &parent_read);
    if (error == kw::_sym_output) {
      child_stderr = child_stdout;
      parent_error = dup(parent_read);
    } else
      create_descriptor(error, kw::_sym_output, &child_stderr, &parent_error);

    child_pid = fork();
    if (child_pid == 0) {
      /* Child */
      int j;
      Array_sp argvv = gc::As<Array_sp>(argv);
      void** argv_ptr = (void**)malloc(sizeof(void*) * cl__length(argvv));
      for (j = 0; j < cl__length(argvv); j++) {
        T_sp arg = argvv->rowMajorAref(j);
        if (arg.nilp()) {
          argv_ptr[j] = NULL;
        } else {
          std::string str = gc::As<String_sp>(arg)->get_std_string();
          argv_ptr[j] = (void*)malloc(str.size() + 1);
          char* end = strncpy((char*)argv_ptr[j], (const char*)str.c_str(), str.size());
          end = end + str.size();
          *end = '\0';
        }
      }
      if (parent_write)
        close(parent_write);
      if (parent_read)
        close(parent_read);
      if (parent_error)
        close(parent_error);

      dup2(child_stdin, STDIN_FILENO);
      dup2(child_stdout, STDOUT_FILENO);
      dup2(child_stderr, STDERR_FILENO);

      if (environ.consp() || environ.nilp()) {
        char** pstrings;
        from_list_to_execve_argument(environ, &pstrings);
        execve((char*)gc::As<String_sp>(command)->get_std_string().c_str(), (char**)argv_ptr, pstrings);
      } else {
        execvp((char*)gc::As<String_sp>(command)->get_std_string().c_str(), (char**)argv_ptr);
      }
      /* at this point exec has failed */
      perror("exec");
      _exit(EXIT_FAILURE);
    }
    close(child_stdin);
    close(child_stdout);
    if (!(error == kw::_sym_output))
      close(child_stderr);

    if (child_pid < 0) {
      pid = nil<T_O>();
    } else {
      pid = clasp_make_fixnum(child_pid);
    }
  }
#else                /* NACL */
  {
    FElibc_error("ext::run-program-inner not implemented", 1);
    @(return nil<T_O>());
  }
#endif

  if (pid.nilp()) {
    if (parent_write)
      close(parent_write);
    if (parent_read)
      close(parent_read);
    if (parent_error > 0)
      close(parent_error);
    parent_write = 0;
    parent_read = 0;
    parent_error = 0;
    FEerror("Could not spawn subprocess to run ~S.", 1, command);
  }
  return Values(pid, clasp_make_fixnum(parent_write), clasp_make_fixnum(parent_read), clasp_make_fixnum(parent_error));
}

} // namespace core

/*
    File: unixsys.cc
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
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    unixsys.c  -- Unix file system interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister.  - converted to C++
    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.
    See file '../Copyright' for full details.
*/
