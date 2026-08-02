#include <clasp/core/foundation.h>

#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/array.h>
#include <clasp/core/numbers.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/lispList.h>
#include <clasp/core/ql.h>
#include <clasp/core/wrappers.h>

#ifndef MAP_ANONYMOUS
#ifdef MAP_ANON
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

namespace core {

CL_DOCSTRING(R"dx(Raw POSIX shm_open. Returns (values fd errno); errno is 0 on success.)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_shm_open(const string& name, int oflag, int mode) {
  int fd = ::shm_open(name.c_str(), oflag, (mode_t)mode);
  return Values(make_fixnum(fd), make_fixnum(fd < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_shm_unlink(const string& name) {
  int r = ::shm_unlink(name.c_str());
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_ftruncate(int fd, Integer_sp length) {
  int r = ::ftruncate(fd, (off_t)clasp_to_size_t(length));
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

CL_DOCSTRING(R"dx(Raw POSIX mmap. addr/length/offset are integers. Returns (values address errno).)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_mmap(Integer_sp addr, Integer_sp length, int prot, int flags, int fd, Integer_sp offset) {
  void* p = ::mmap((void*)clasp_to_uintptr_t(addr), clasp_to_size_t(length), prot, flags, fd,
                   (off_t)clasp_to_uint64_t(offset));
  if (p == MAP_FAILED)
    return Values(make_fixnum(0), make_fixnum(errno));
  return Values(Integer_O::create((uint64_t)(uintptr_t)p), make_fixnum(0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_munmap(Integer_sp addr, Integer_sp length) {
  int r = ::munmap((void*)clasp_to_uintptr_t(addr), clasp_to_size_t(length));
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_mprotect(Integer_sp addr, Integer_sp length, int prot) {
  int r = ::mprotect((void*)clasp_to_uintptr_t(addr), clasp_to_size_t(length), prot);
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_msync(Integer_sp addr, Integer_sp length, int flags) {
  int r = ::msync((void*)clasp_to_uintptr_t(addr), clasp_to_size_t(length), flags);
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_mlock(Integer_sp addr, Integer_sp length) {
  int r = ::mlock((void*)clasp_to_uintptr_t(addr), clasp_to_size_t(length));
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__sys_munlock(Integer_sp addr, Integer_sp length) {
  int r = ::munlock((void*)clasp_to_uintptr_t(addr), clasp_to_size_t(length));
  return Values(make_fixnum(r), make_fixnum(r < 0 ? errno : 0));
}

DOCGROUP(clasp);
CL_DEFUN int core__sys_getpagesize() { return (int)::getpagesize(); }

DOCGROUP(clasp);
CL_DEFUN String_sp core__sys_strerror(int errno_value) { return clasp_strerror(errno_value); }

CL_DOCSTRING(R"dx(Return an alist of (keyword . int) for all POSIX shm/mmap flag constants.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__sys_shm_constants() {
  ql::list l;
#define SHMC(k, v) l << Cons_O::create(_lisp->internKeyword(k), make_fixnum(v))
  SHMC("RDONLY", O_RDONLY);
  SHMC("WRONLY", O_WRONLY);
  SHMC("RDWR", O_RDWR);
  SHMC("CREATE", O_CREAT);
  SHMC("EXCLUSIVE", O_EXCL);
  SHMC("TRUNCATE", O_TRUNC);
  SHMC("NONE", PROT_NONE);
  SHMC("READ", PROT_READ);
  SHMC("WRITE", PROT_WRITE);
  SHMC("EXEC", PROT_EXEC);
  SHMC("SHARED", MAP_SHARED);
  SHMC("PRIVATE", MAP_PRIVATE);
  SHMC("FIXED", MAP_FIXED);
  SHMC("ANONYMOUS", MAP_ANONYMOUS);
  SHMC("SYNC", MS_SYNC);
  SHMC("ASYNC", MS_ASYNC);
  SHMC("INVALIDATE", MS_INVALIDATE);
  SHMC("EEXIST", EEXIST);
#undef SHMC
  return l.cons();
}

}; // namespace core
