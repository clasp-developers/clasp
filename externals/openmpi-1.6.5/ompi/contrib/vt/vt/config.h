/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */


#ifndef _CONFIG_H
#define _CONFIG_H


/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* pathname of shared CUDA runtime library */
/* #undef CUDARTSHLIB_PATHNAME */

/* Command to list symbols from object files. */
#define DEFAULT_NM "/usr/bin/nm"

/* Path for node-local temporary directory */
/* #undef DEFAULT_PFORM_LDIR */

/* Define to 1 if you have the ACML. */
/* #undef HAVE_ACML */

/* Define to 1 if you have the <asm/intrinsics.h> header file. */
/* #undef HAVE_ASM_INTRINSICS_H */

/* Define to 1 if you have the `asprintf' function. */
#define HAVE_ASPRINTF 1

/* Define to 1 if you have the <catamount/data.h> header file. */
/* #undef HAVE_CATAMOUNT_DATA_H */

/* Define to 1 if you have the <catamount/dclock.h> header file. */
/* #undef HAVE_CATAMOUNT_DCLOCK_H */

/* Define to 1 if you have the `creat64' function. */
/* #undef HAVE_CREAT64 */

/* Define to 1 if you have the declaration of `environ', and to 0 if you
   don't. */
/* #undef HAVE_DECL_ENVIRON */

/* Define to 1 if you have the declaration of `MPI_IN_PLACE', and to 0 if you
   don't. */
#define HAVE_DECL_MPI_IN_PLACE 1

/* Define to 1 if you have the declaration of `MPI_ROOT', and to 0 if you
   don't. */
#define HAVE_DECL_MPI_ROOT 1

/* Define to 1 if you have the declaration of `MPI_STATUS_SIZE', and to 0 if
   you don't. */
/* #undef HAVE_DECL_MPI_STATUS_SIZE */

/* Define to 1 if you have the declaration of `RTLD_DEFAULT', and to 0 if you
   don't. */
#define HAVE_DECL_RTLD_DEFAULT 1

/* Define to 1 if you have the declaration of `RTLD_NEXT', and to 0 if you
   don't. */
#define HAVE_DECL_RTLD_NEXT 1

/* Define to 1 if you have the declaration of `RUSAGE_THREAD', and to 0 if you
   don't. */
#define HAVE_DECL_RUSAGE_THREAD 0

/* Define to 1 if you have the declaration of `_Errno', and to 0 if you don't.
   */
/* #undef HAVE_DECL__ERRNO */

/* Define to 1 if you have the declaration of `__errno_location', and to 0 if
   you don't. */
/* #undef HAVE_DECL___ERRNO_LOCATION */

/* Define to 1 if you have the declaration of `__vfprintf_chk', and to 0 if
   you don't. */
/* #undef HAVE_DECL___VFPRINTF_CHK */

/* Define to 1 if you have the <demangle.h> header file. */
/* #undef HAVE_DEMANGLE_H */

/* Define to 1 if you have the DL. */
#define HAVE_DL 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the ESSL. */
/* #undef HAVE_ESSL */

/* Define to 1 if you have the `execvpe' function. */
/* #undef HAVE_EXECVPE */

/* Define to 1 if VT is configured with Fortran support. */
/* #undef HAVE_FC */

/* Define to 1 if you have the `fdatasync' function. */
/* #undef HAVE_FDATASYNC */

/* Define to 1 if you have the `fexecve' function. */
/* #undef HAVE_FEXECVE */

/* Define to 1 if you have the `fflush' function. */
/* #undef HAVE_FFLUSH */

/* Define to 1 if you have the `flockfile' function. */
/* #undef HAVE_FLOCKFILE */

/* Define to 1 if VT is configured with MPI Fortran support. */
/* #undef HAVE_FMPI */

/* Define to 1 if you have the <fnmatch.h> header file. */
#define HAVE_FNMATCH_H 1

/* Define to 1 if you have the `fopen64' function. */
/* #undef HAVE_FOPEN64 */

/* Define to 1 if you have the `fseeko' function. */
/* #undef HAVE_FSEEKO */

/* Define to 1 if you have the `fseeko64' function. */
/* #undef HAVE_FSEEKO64 */

/* Define to 1 if you have the `fsetpos64' function. */
/* #undef HAVE_FSETPOS64 */

/* Define to 1 if you have the `fsync' function. */
/* #undef HAVE_FSYNC */

/* Define to 1 if you have the `ftrylockfile' function. */
/* #undef HAVE_FTRYLOCKFILE */

/* Define to 1 if you have the `funlockfile' function. */
/* #undef HAVE_FUNLOCKFILE */

/* Define to 1 if VT is configured with Hybrid (MPI/Threads) support. */
#define HAVE_HYBRID 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if VT is configured with IOFSL support. */
/* #undef HAVE_IOFSL */

/* Define to 1 if you have the <linux/mmtimer.h> header file. */
/* #undef HAVE_LINUX_MMTIMER_H */

/* Define to 1 if you have the `lockf' function. */
/* #undef HAVE_LOCKF */

/* Define to 1 if the system has the type `long_long'. */
/* #undef HAVE_LONG_LONG */

/* Define to 1 if you have the `lseek64' function. */
/* #undef HAVE_LSEEK64 */

/* Define to 1 if you have the `memalign' function. */
/* #undef HAVE_MEMALIGN */

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the MKL. */
/* #undef HAVE_MKL */

/* Define to 1 if you have the <mmtimer.h> header file. */
/* #undef HAVE_MMTIMER_H */

/* Define to 1 if VT is configured with MPI support. */
#define HAVE_MPI 1

/* Define to 1 if you have functions for MPI-2 One-Sided Communications. */
#define HAVE_MPI2_1SIDED 1

/* Define to 1 if you have functions for MPI-2 Extended Collective Operations.
   */
#define HAVE_MPI2_EXTCOLL 1

/* Define to 1 if you have functions for MPI-2 I/O. */
#define HAVE_MPI2_IO 1

/* Define to 1 if you have functions for MPI-2 Thread support. */
#define HAVE_MPI2_THREAD 1

/* Define to 1 if you have the `MPI_Add_error_class' function. */
#define HAVE_MPI_ADD_ERROR_CLASS 1

/* Define to 1 if you have the `MPI_Add_error_code' function. */
#define HAVE_MPI_ADD_ERROR_CODE 1

/* Define to 1 if you have the `MPI_Add_error_string' function. */
#define HAVE_MPI_ADD_ERROR_STRING 1

/* Define to 1 if you have the `MPI_Finalized' function. */
#define HAVE_MPI_FINALIZED 1

/* Define to 1 if you have the `MPI_Get_address' function. */
#define HAVE_MPI_GET_ADDRESS 1

/* Define to 1 if you have the `MPI_Register_datarep' function. */
#define HAVE_MPI_REGISTER_DATAREP 1

/* Define to 1 if you have the `MPI_Type_create_f90_complex' function. */
#define HAVE_MPI_TYPE_CREATE_F90_COMPLEX 1

/* Define to 1 if you have the `MPI_Type_create_f90_integer' function. */
#define HAVE_MPI_TYPE_CREATE_F90_INTEGER 1

/* Define to 1 if you have the `MPI_Type_create_f90_real' function. */
#define HAVE_MPI_TYPE_CREATE_F90_REAL 1

/* Define to 1 if you have the `MPI_Type_create_struct' function. */
#define HAVE_MPI_TYPE_CREATE_STRUCT 1

/* Define to 1 if you have the `MPI_Type_dup' function. */
#define HAVE_MPI_TYPE_DUP 1

/* Define to 1 if you have the `MPI_Type_match_size' function. */
#define HAVE_MPI_TYPE_MATCH_SIZE 1

/* Define to 1 if VT is configured with OpenMP support. */
/* #undef HAVE_OMP */

/* Define to 1 if you have the `open64' function. */
/* #undef HAVE_OPEN64 */

/* Define to 1 if you have the `PMPI_File_read_ordered' function. */
#define HAVE_PMPI_FILE_READ_ORDERED 1

/* Define to 1 if you have the `PMPI_File_read_ordered_begin' function. */
#define HAVE_PMPI_FILE_READ_ORDERED_BEGIN 1

/* Define to 1 if you have the `PMPI_File_write_ordered' function. */
#define HAVE_PMPI_FILE_WRITE_ORDERED 1

/* Define to 1 if you have the `PMPI_File_write_ordered_begin' function. */
#define HAVE_PMPI_FILE_WRITE_ORDERED_BEGIN 1

/* Define to 1 if you have the `PMPI_Win_lock' function. */
#define HAVE_PMPI_WIN_LOCK 1

/* Define to 1 if you have the `PMPI_Win_test' function. */
#define HAVE_PMPI_WIN_TEST 1

/* Define to 1 if you have the `PMPI_Win_unlock' function. */
#define HAVE_PMPI_WIN_UNLOCK 1

/* Define to 1 if you have the `posix_memalign' function. */
/* #undef HAVE_POSIX_MEMALIGN */

/* Define to 1 if you have the `pread64' function. */
/* #undef HAVE_PREAD64 */

/* Define to 1 if VT is configured with Pthreads support. */
#define HAVE_PTHREAD 1

/* Define to 1 if you have the `pthread_condattr_getpshared' function. */
#define HAVE_PTHREAD_CONDATTR_GETPSHARED 1

/* Define to 1 if you have the `pthread_condattr_setpshared' function. */
#define HAVE_PTHREAD_CONDATTR_SETPSHARED 1

/* Define to 1 if you have the `pthread_mutexattr_getpshared' function. */
#define HAVE_PTHREAD_MUTEXATTR_GETPSHARED 1

/* Define to 1 if you have the `pthread_mutexattr_setpshared' function. */
#define HAVE_PTHREAD_MUTEXATTR_SETPSHARED 1

/* Define to 1 if you have the `pwrite64' function. */
/* #undef HAVE_PWRITE64 */

/* Define to 1 if you have the `snprintf' function. */
#define HAVE_SNPRINTF 1

/* Define to 1 if you have the <sn/mmtimer.h> header file. */
/* #undef HAVE_SN_MMTIMER_H */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the SUN Performance Library. */
/* #undef HAVE_SUNPERF */

/* Define to 1 if you have the `sync' function. */
/* #undef HAVE_SYNC */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if VT is configured with Threads support. */
#define HAVE_THREADS 1

/* Define to 1 if you have the `__va_copy' function. */
#define HAVE_UNDERSCORE_VA_COPY 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `valloc' function. */
/* #undef HAVE_VALLOC */

/* Define to 1 if you have the `vasprintf' function. */
#define HAVE_VASPRINTF 1

/* Define to 1 if you have the `va_copy' function. */
#define HAVE_VA_COPY 1

/* Define to 1 if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Define to 1 if you have the `wait3' function. */
/* #undef HAVE_WAIT3 */

/* Define to 1 if you have the `wait4' function. */
/* #undef HAVE_WAIT4 */

/* Define to 1 if you have the `waitid' function. */
/* #undef HAVE_WAITID */

/* Define to 1 if you have the ZLIB. */
#define HAVE_ZLIB 1

/* Define to 1 if you have the `__fprintf_chk' function. */
/* #undef HAVE___FPRINTF_CHK */

/* Define to 1 if the system has the type `__WAIT_STATUS'. */
/* #undef HAVE___WAIT_STATUS */

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Define to 1 to record MPI functions called within MPI functions. */
#define MPI_TRACE_INSIDE 0

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* Name of package */
#define PACKAGE "VampirTrace"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "vampirsupport@zih.tu-dresden.de"

/* Define to the full name of this package. */
#define PACKAGE_NAME "VampirTrace"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "VampirTrace 5.14.4openmpi"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "VampirTrace"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "5.14.4openmpi"

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* pathname of shared LIBC */
/* #undef SHLIBC_PATHNAME */

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `mode_t', as computed by sizeof. */
#define SIZEOF_MODE_T 2

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T 8

/* The size of `void*', as computed by sizeof. */
#define SIZEOF_VOIDP 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Use timer (see below) */
#define TIMER TIMER_CYCLE_COUNTER

/* Use `clock_gettime' function */
/* #undef TIMER_CLOCK_GETTIME */

/* CRAY Real-Time-Clock */
/* #undef TIMER_CRAY_RTCLOCK */

/* Cycle counter (e.g. TSC) */
#define TIMER_CYCLE_COUNTER 1

/* Use `dclock' function */
/* #undef TIMER_DCLOCK */

/* gethrtime */
/* #undef TIMER_GETHRTIME */

/* Use `gettimeofday' function */
#define TIMER_GETTIMEOFDAY 2

/* Use `GetTimeBase' function */
/* #undef TIMER_GET_TIMEBASE */

/* Define to 1 if the selected timer is global (doesn't need synchronization)
   */
#define TIMER_IS_GLOBAL 0

/* Intel Multimedia Timer */
/* #undef TIMER_MMTIMER */

/* PAPI_get_real_cyc */
/* #undef TIMER_PAPI_REAL_CYC */

/* PAPI_get_real_usec */
/* #undef TIMER_PAPI_REAL_USEC */

/* IBM Power family Real-Time-Clock */
/* #undef TIMER_POWER_REALTIME */

/* RTC (DOES NOT WORK YET WITH FORTRAN CODES) */
/* #undef TIMER_RTC */

/* Use `rts_get_timebase' function */
/* #undef TIMER_RTS_GET_TIMEBASE */

/* Hardware Switch-Clock (it's necessary to link your application with
   '-lswclock') */
/* #undef TIMER_SWITCH_CLOCK */

/* NEC SX HGTIME */
/* #undef TIMER_SYSSX_HGTIME */

/* UniMCI's checker name */
/* #undef UNIMCI_CHECKER_NAME */

/* UniMCI's checker version */
/* #undef UNIMCI_CHECKER_VERSION */

/* Version number of package */
#define VERSION "5.14.4openmpi"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */


#include "config_bottom.h"
#endif /* _CONFIG_H */

