/* opal/include/opal_config.h.  Generated from opal_config.h.in by configure.  */
/* opal/include/opal_config.h.in.  Generated from configure.ac by autoheader.  */

/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef OPAL_CONFIG_H
#define OPAL_CONFIG_H


/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* enable openib BTL failover */
#define BTL_OPENIB_FAILOVER_ENABLED 0

/* Whether the openib BTL malloc hooks are enabled */
#define BTL_OPENIB_MALLOC_HOOKS_ENABLED 1

/* BLCR cr_request_file check */
/* #undef CRS_BLCR_HAVE_CR_REQUEST */

/* BLCR cr_request_checkpoint check */
/* #undef CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT */

/* BLCRs cr_checkpoint_info.requester member availability */
/* #undef CRS_BLCR_HAVE_INFO_REQUESTER */

/* Define to 1 if you have the <aio.h> header file. */
#define HAVE_AIO_H 1

/* Define to 1 if you have the <alloca.h> header file. */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define HAVE_ARPA_INET_H 1

/* Define to 1 if you have the `asprintf' function. */
#define HAVE_ASPRINTF 1

/* Define to 1 if you have the `backtrace' function. */
#define HAVE_BACKTRACE 1

/* Define to 1 if the system has the type `CACHE_DESCRIPTOR'. */
/* #undef HAVE_CACHE_DESCRIPTOR */

/* Define to 1 if the system has the type `CACHE_RELATIONSHIP'. */
/* #undef HAVE_CACHE_RELATIONSHIP */

/* Define to 1 if you have the <catamount/cnos_mpi_os.h> header file. */
/* #undef HAVE_CATAMOUNT_CNOS_MPI_OS_H */

/* Define to 1 if you have the <catamount/dclock.h> header file. */
/* #undef HAVE_CATAMOUNT_DCLOCK_H */

/* Define to 1 if you have the `ceil' function. */
#define HAVE_CEIL 1

/* Define to 1 if you have the `clz' function. */
/* #undef HAVE_CLZ */

/* Define to 1 if you have the `clzl' function. */
/* #undef HAVE_CLZL */

/* Define to 1 if you have the <cnos_mpi_os.h> header file. */
/* #undef HAVE_CNOS_MPI_OS_H */

/* Define to 1 if you have the <crt_externs.h> header file. */
/* #undef HAVE_CRT_EXTERNS_H */

/* Define to 1 if you have the `cr_request_checkpoint' function. */
/* #undef HAVE_CR_REQUEST_CHECKPOINT */

/* Define to 1 if you have the `cr_request_file' function. */
/* #undef HAVE_CR_REQUEST_FILE */

/* uDAPL DAT_MEM_TYPE_SO_VIRTUAL check */
/* #undef HAVE_DAT_MEM_TYPE_SO_VIRTUAL */

/* Define to 1 if you have the declaration of `AF_INET6', and to 0 if you
   don't. */
#define HAVE_DECL_AF_INET6 1

/* Define to 1 if you have the declaration of `AF_UNSPEC', and to 0 if you
   don't. */
#define HAVE_DECL_AF_UNSPEC 1

/* Define to 1 if you have the declaration of `CTL_HW', and to 0 if you don't.
   */
#define HAVE_DECL_CTL_HW 0

/* Define to 1 if you have the declaration of `HW_NCPU', and to 0 if you
   don't. */
#define HAVE_DECL_HW_NCPU 0

/* Define to 1 if you have the declaration of `HZ', and to 0 if you don't. */
#define HAVE_DECL_HZ 1

/* Define to 1 if you have the declaration of `IBV_ACCESS_SO', and to 0 if you
   don't. */
#define HAVE_DECL_IBV_ACCESS_SO 0

/* Define to 1 if you have the declaration of `IBV_EVENT_CLIENT_REREGISTER',
   and to 0 if you don't. */
#define HAVE_DECL_IBV_EVENT_CLIENT_REREGISTER 1

/* Define to 1 if you have the declaration of `IBV_LINK_LAYER_ETHERNET', and
   to 0 if you don't. */
#define HAVE_DECL_IBV_LINK_LAYER_ETHERNET 1

/* Define to 1 if you have the declaration of `MPOL_MF_MOVE', and to 0 if you
   don't. */
/* #undef HAVE_DECL_MPOL_MF_MOVE */

/* Define to 1 if you have the declaration of `PCI_LOOKUP_NO_NUMBERS', and to
   0 if you don't. */
/* #undef HAVE_DECL_PCI_LOOKUP_NO_NUMBERS */

/* Define to 1 if you have the declaration of `PF_INET6', and to 0 if you
   don't. */
#define HAVE_DECL_PF_INET6 1

/* Define to 1 if you have the declaration of `PF_UNSPEC', and to 0 if you
   don't. */
#define HAVE_DECL_PF_UNSPEC 1

/* Define to 1 if you have the declaration of `pthread_getaffinity_np', and to
   0 if you don't. */
#define HAVE_DECL_PTHREAD_GETAFFINITY_NP 1

/* Define to 1 if you have the declaration of `pthread_setaffinity_np', and to
   0 if you don't. */
#define HAVE_DECL_PTHREAD_SETAFFINITY_NP 1

/* Define to 1 if you have the declaration of `RLIMIT_MEMLOCK', and to 0 if
   you don't. */
#define HAVE_DECL_RLIMIT_MEMLOCK 1

/* Define to 1 if you have the declaration of `RLIMIT_NPROC', and to 0 if you
   don't. */
#define HAVE_DECL_RLIMIT_NPROC 1

/* Define to 1 if you have the declaration of `sbrk', and to 0 if you don't.
   */
#define HAVE_DECL_SBRK 1

/* Define to 1 if you have the declaration of `_SC_LARGE_PAGESIZE', and to 0
   if you don't. */
#define HAVE_DECL__SC_LARGE_PAGESIZE 0

/* Define to 1 if you have the declaration of `_SC_NPROCESSORS_CONF', and to 0
   if you don't. */
#define HAVE_DECL__SC_NPROCESSORS_CONF 1

/* Define to 1 if you have the declaration of `_SC_NPROCESSORS_ONLN', and to 0
   if you don't. */
#define HAVE_DECL__SC_NPROCESSORS_ONLN 1

/* Define to 1 if you have the declaration of `_SC_NPROC_CONF', and to 0 if
   you don't. */
#define HAVE_DECL__SC_NPROC_CONF 0

/* Define to 1 if you have the declaration of `_SC_NPROC_ONLN', and to 0 if
   you don't. */
#define HAVE_DECL__SC_NPROC_ONLN 0

/* Define to 1 if you have the declaration of `__func__', and to 0 if you
   don't. */
#define HAVE_DECL___FUNC__ 1

/* Define if /dev/poll is available */
/* #undef HAVE_DEVPOLL */

/* Define to 1 if you have the <dirent.h> header file. */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the `dirname' function. */
#define HAVE_DIRNAME 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the `dlsym' function. */
#define HAVE_DLSYM 1

/* Define to 1 if the system has the type `double _Complex'. */
#define HAVE_DOUBLE__COMPLEX 1

/* Define if your system supports the epoll interface */
#define HAVE_EPOLL 1

/* Define to 1 if you have the `epoll_ctl' function. */
#define HAVE_EPOLL_CTL 1

/* Define to 1 if you have the <err.h> header file. */
#define HAVE_ERR_H 1

/* Define if your system supports event ports */
/* #undef HAVE_EVENT_PORTS */

/* Define to 1 if you have the <execinfo.h> header file. */
#define HAVE_EXECINFO_H 1

/* Define to 1 if you have the `execve' function. */
#define HAVE_EXECVE 1

/* Define to 1 if you have the `fcntl' function. */
#define HAVE_FCNTL 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `ffs' function. */
#define HAVE_FFS 1

/* Define to 1 if you have the `ffsl' function. */
#define HAVE_FFSL 1

/* Define to 1 if the system has the type `float _Complex'. */
#define HAVE_FLOAT__COMPLEX 1

/* Define to 1 if you have the `fls' function. */
/* #undef HAVE_FLS */

/* Define to 1 if you have the `flsl' function. */
/* #undef HAVE_FLSL */

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the `getpwuid' function. */
#define HAVE_GETPWUID 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if the system has the type `GROUP_AFFINITY'. */
/* #undef HAVE_GROUP_AFFINITY */

/* Define to 1 if the system has the type `GROUP_RELATIONSHIP'. */
/* #undef HAVE_GROUP_RELATIONSHIP */

/* Define to 1 if you have the <grp.h> header file. */
#define HAVE_GRP_H 1

/* Define to 1 if you have the <hostLib.h> header file. */
/* #undef HAVE_HOSTLIB_H */

/* Define to 1 if you have the `host_info' function. */
/* #undef HAVE_HOST_INFO */

/* Define to 1 if you have the `ibv_create_xrc_rcv_qp' function. */
/* #undef HAVE_IBV_CREATE_XRC_RCV_QP */

/* Define to 1 if you have the `ibv_fork_init' function. */
#define HAVE_IBV_FORK_INIT 1

/* Define to 1 if you have the `ibv_get_device_list' function. */
#define HAVE_IBV_GET_DEVICE_LIST 1

/* Define to 1 if you have the `ibv_resize_cq' function. */
#define HAVE_IBV_RESIZE_CQ 1

/* Define to 1 if you have the <ifaddrs.h> header file. */
#define HAVE_IFADDRS_H 1

/* Define to 1 if you have the <infiniband/driver.h> header file. */
#define HAVE_INFINIBAND_DRIVER_H 1

/* Define to 1 if the system has the type `int128_t'. */
/* #undef HAVE_INT128_T */

/* Define to 1 if the system has the type `int16_t'. */
#define HAVE_INT16_T 1

/* Define to 1 if the system has the type `int32_t'. */
#define HAVE_INT32_T 1

/* Define to 1 if the system has the type `int64_t'. */
#define HAVE_INT64_T 1

/* Define to 1 if the system has the type `int8_t'. */
#define HAVE_INT8_T 1

/* Define to 1 if the system has the type `intptr_t'. */
#define HAVE_INTPTR_T 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <ioLib.h> header file. */
/* #undef HAVE_IOLIB_H */

/* Define to 1 if you have the `isatty' function. */
#define HAVE_ISATTY 1

/* Define to 1 if the system has the type `KAFFINITY'. */
/* #undef HAVE_KAFFINITY */

/* Define to 1 if you have the `kqueue' function. */
/* #undef HAVE_KQUEUE */

/* Define to 1 if you have the <kstat.h> header file. */
/* #undef HAVE_KSTAT_H */

/* Define to 1 if we have -lgdi32 */
/* #undef HAVE_LIBGDI32 */

/* Define to 1 if you have the <libgen.h> header file. */
#define HAVE_LIBGEN_H 1

/* Define to 1 if we have -lkstat */
/* #undef HAVE_LIBKSTAT */

/* Define to 1 if we have -llgrp */
/* #undef HAVE_LIBLGRP */

/* Define to 1 if you have the `nsl' library (-lnsl). */
#define HAVE_LIBNSL 1

/* Define to 1 if you have the `pci' library (-lpci). */
/* #undef HAVE_LIBPCI */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef HAVE_LIBSOCKET */

/* Define to 1 if you have the <libutil.h> header file. */
/* #undef HAVE_LIBUTIL_H */

/* Define to 1 if the system has the type `LOGICAL_PROCESSOR_RELATIONSHIP'. */
/* #undef HAVE_LOGICAL_PROCESSOR_RELATIONSHIP */

/* Define to 1 if the system has the type `long double'. */
#define HAVE_LONG_DOUBLE 1

/* Define to 1 if the system has the type `long double _Complex'. */
#define HAVE_LONG_DOUBLE__COMPLEX 1

/* Define to 1 if the system has the type `long long'. */
#define HAVE_LONG_LONG 1

/* Define to 1 if you have the <mach/mach_host.h> header file. */
/* #undef HAVE_MACH_MACH_HOST_H */

/* Define to 1 if you have the <mach/mach_init.h> header file. */
/* #undef HAVE_MACH_MACH_INIT_H */

/* Define to 1 if you have the <mach/mach_time.h> header file. */
/* #undef HAVE_MACH_MACH_TIME_H */

/* Define to 1 if you have the <mach/mach_vm.h> header file. */
/* #undef HAVE_MACH_MACH_VM_H */

/* Define to 1 if you have the `mach_vm_read' function. */
/* #undef HAVE_MACH_VM_READ */

/* Define to 1 if you have the `mach_vm_region' function. */
/* #undef HAVE_MACH_VM_REGION */

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `memalign' function. */
#define HAVE_MEMALIGN 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkfifo' function. */
#define HAVE_MKFIFO 1

/* Define to 1 if you have the `mmap' function. */
#define HAVE_MMAP 1

/* Define to 1 if the system has the type `mode_t'. */
#define HAVE_MODE_T 1

/* Define to 1 if you have the <mx_extensions.h> header file. */
/* #undef HAVE_MX_EXTENSIONS_H */

/* Define to 1 if you have the `mx_forget' function. */
/* #undef HAVE_MX_FORGET */

/* Define to 1 if you have the `mx_register_unexp_handler' function. */
/* #undef HAVE_MX_REGISTER_UNEXP_HANDLER */

/* Define to 1 if you have the <netdb.h> header file. */
#define HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#define HAVE_NETINET_IN_H 1

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#define HAVE_NETINET_TCP_H 1

/* Define to 1 if you have the <net/if.h> header file. */
#define HAVE_NET_IF_H 1

/* Define to 1 if you have the <net/uio.h> header file. */
/* #undef HAVE_NET_UIO_H */

/* Define to 1 if you have the <numaif.h> header file. */
/* #undef HAVE_NUMAIF_H */

/* Define to 1 if the system has the type `NUMA_NODE_RELATIONSHIP'. */
/* #undef HAVE_NUMA_NODE_RELATIONSHIP */

/* Define to 1 if you have the `openat' function. */
#define HAVE_OPENAT 1

/* Define to 1 if you have the `openpty' function. */
#define HAVE_OPENPTY 1

/* Define to 1 if you have the <pci/pci.h> header file. */
/* #undef HAVE_PCI_PCI_H */

/* Define to 1 if you have the `pipe' function. */
#define HAVE_PIPE 1

/* Define to 1 if you have the <pmapi.h> header file. */
/* #undef HAVE_PMAPI_H */

/* Define to 1 if you have the <pmi.h> header file. */
/* #undef HAVE_PMI_H */

/* Define to 1 if you have the `pm_cycles' function. */
/* #undef HAVE_PM_CYCLES */

/* Define to 1 if you have the `poll' function. */
#define HAVE_POLL 1

/* Define to 1 if you have the <poll.h> header file. */
#define HAVE_POLL_H 1

/* Define to 1 if you have the `port_create' function. */
/* #undef HAVE_PORT_CREATE */

/* Define to 1 if you have the `posix_memalign' function. */
#define HAVE_POSIX_MEMALIGN 1

/* Define to 1 if `srr0' is a member of `ppc_thread_state_t'. */
/* #undef HAVE_PPC_THREAD_STATE_T_SRR0 */

/* Define to 1 if you have the `printstack' function. */
/* #undef HAVE_PRINTSTACK */

/* Define to 1 if the system has the type `PROCESSOR_CACHE_TYPE'. */
/* #undef HAVE_PROCESSOR_CACHE_TYPE */

/* Define to 1 if the system has the type `PROCESSOR_GROUP_INFO'. */
/* #undef HAVE_PROCESSOR_GROUP_INFO */

/* Define to 1 if the system has the type `PROCESSOR_RELATIONSHIP'. */
/* #undef HAVE_PROCESSOR_RELATIONSHIP */

/* Define to 1 if the system has the type `PSAPI_WORKING_SET_EX_BLOCK'. */
/* #undef HAVE_PSAPI_WORKING_SET_EX_BLOCK */

/* Define to 1 if the system has the type `PSAPI_WORKING_SET_EX_INFORMATION'.
   */
/* #undef HAVE_PSAPI_WORKING_SET_EX_INFORMATION */

/* Define to 1 if you have the <pthread.h> header file. */
#define HAVE_PTHREAD_H 1

/* Define to 1 if you have the <pthread_np.h> header file. */
/* #undef HAVE_PTHREAD_NP_H */

/* Define to 1 if the system has the type `pthread_t'. */
#define HAVE_PTHREAD_T 1

/* Define to 1 if the system has the type `ptrdiff_t'. */
#define HAVE_PTRDIFF_T 1

/* Define to 1 if you have the `ptsname' function. */
#define HAVE_PTSNAME 1

/* Define to 1 if you have the <pty.h> header file. */
#define HAVE_PTY_H 1

/* Define to 1 if you have the <pwd.h> header file. */
#define HAVE_PWD_H 1

/* Define to 1 if you have the <rdma/rdma_cma.h> header file. */
/* #undef HAVE_RDMA_RDMA_CMA_H */

/* Define to 1 if you have the `regcmp' function. */
/* #undef HAVE_REGCMP */

/* Define to 1 if you have the `regexec' function. */
#define HAVE_REGEXEC 1

/* Define to 1 if you have the <regex.h> header file. */
#define HAVE_REGEX_H 1

/* Define to 1 if you have the `regfree' function. */
#define HAVE_REGFREE 1

/* Define to 1 if the system has the type `RelationProcessorPackage'. */
/* #undef HAVE_RELATIONPROCESSORPACKAGE */

/* Define if your system supports POSIX realtime signals */
/* #undef HAVE_RTSIG */

/* Define to 1 if you have the <sched.h> header file. */
#define HAVE_SCHED_H 1

/* Define to 1 if you have the `sched_yield' function. */
#define HAVE_SCHED_YIELD 1

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define if F_SETFD is defined in <fcntl.h> */
#define HAVE_SETFD 1

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the `setsid' function. */
#define HAVE_SETSID 1

/* Define to 1 if you have the <shlwapi.h> header file. */
/* #undef HAVE_SHLWAPI_H */

/* Define to 1 if `si_band' is a member of `siginfo_t'. */
#define HAVE_SIGINFO_T_SI_BAND 1

/* Define to 1 if `si_fd' is a member of `siginfo_t'. */
#define HAVE_SIGINFO_T_SI_FD 1

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if you have the `sigtimedwait' function. */
/* #undef HAVE_SIGTIMEDWAIT */

/* Define to 1 if you have the `snprintf' function. */
#define HAVE_SNPRINTF 1

/* Define to 1 if you have the `socketpair' function. */
#define HAVE_SOCKETPAIR 1

/* Define to 1 if the system has the type `socklen_t'. */
#define HAVE_SOCKLEN_T 1

/* Define to 1 if you have the <sockLib.h> header file. */
/* #undef HAVE_SOCKLIB_H */

/* Define to 1 if the system has the type `ssize_t'. */
#define HAVE_SSIZE_T 1

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if you have the <stdbool.h> header file. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strftime' function. */
#define HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define to 1 if you have the `strncpy_s' function. */
/* #undef HAVE_STRNCPY_S */

/* Define to 1 if you have the <stropts.h> header file. */
/* #undef HAVE_STROPTS_H */

/* Define to 1 if you have the `strsignal' function. */
#define HAVE_STRSIGNAL 1

/* Define to 1 if you have the `strtoll' function. */
#define HAVE_STRTOLL 1

/* Define to 1 if `d_type' is a member of `struct dirent'. */
#define HAVE_STRUCT_DIRENT_D_TYPE 1

/* Define to 1 if `transport_type' is a member of `struct ibv_device'. */
#define HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE 1

/* Define to 1 if the system has the type `struct sockaddr_in'. */
#define HAVE_STRUCT_SOCKADDR_IN 1

/* Define to 1 if the system has the type `struct sockaddr_in6'. */
#define HAVE_STRUCT_SOCKADDR_IN6 1

/* Define to 1 if `sa_len' is a member of `struct sockaddr'. */
/* #undef HAVE_STRUCT_SOCKADDR_SA_LEN */

/* Define to 1 if the system has the type `struct sockaddr_storage'. */
#define HAVE_STRUCT_SOCKADDR_STORAGE 1

/* Define to 1 if you have the `syscall' function. */
#define HAVE_SYSCALL 1

/* Define to 1 if you have the `sysconf' function. */
#define HAVE_SYSCONF 1

/* Define to 1 if you have the `sysctl' function. */
#define HAVE_SYSCTL 1

/* Define to 1 if you have the `sysctlbyname' function. */
/* #undef HAVE_SYSCTLBYNAME */

/* Define to 1 if you have the `syslog' function. */
#define HAVE_SYSLOG 1

/* Define to 1 if you have the <syslog.h> header file. */
#define HAVE_SYSLOG_H 1

/* Define to 1 if the system has the type
   `SYSTEM_LOGICAL_PROCESSOR_INFORMATION'. */
/* #undef HAVE_SYSTEM_LOGICAL_PROCESSOR_INFORMATION */

/* Define to 1 if the system has the type
   `SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX'. */
/* #undef HAVE_SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX */

/* Define to 1 if you have the <sys/cpuset.h> header file. */
/* #undef HAVE_SYS_CPUSET_H */

/* Define to 1 if you have the <sys/devpoll.h> header file. */
/* #undef HAVE_SYS_DEVPOLL_H */

/* Define to 1 if you have the <sys/epoll.h> header file. */
#define HAVE_SYS_EPOLL_H 1

/* Define to 1 if you have the <sys/event.h> header file. */
/* #undef HAVE_SYS_EVENT_H */

/* Define to 1 if you have the <sys/fcntl.h> header file. */
#define HAVE_SYS_FCNTL_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ipc.h> header file. */
#define HAVE_SYS_IPC_H 1

/* Define to 1 if you have the <sys/lgrp_user.h> header file. */
/* #undef HAVE_SYS_LGRP_USER_H */

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/mount.h> header file. */
#define HAVE_SYS_MOUNT_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/poll.h> header file. */
#define HAVE_SYS_POLL_H 1

/* Define to 1 if you have the <sys/queue.h> header file. */
#define HAVE_SYS_QUEUE_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/shm.h> header file. */
#define HAVE_SYS_SHM_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/sockio.h> header file. */
/* #undef HAVE_SYS_SOCKIO_H */

/* Define to 1 if you have the <sys/statfs.h> header file. */
#define HAVE_SYS_STATFS_H 1

/* Define to 1 if you have the <sys/statvfs.h> header file. */
#define HAVE_SYS_STATVFS_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/synch.h> header file. */
/* #undef HAVE_SYS_SYNCH_H */

/* Define to 1 if you have the <sys/sysctl.h> header file. */
#define HAVE_SYS_SYSCTL_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/tree.h> header file. */
/* #undef HAVE_SYS_TREE_H */

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/uio.h> header file. */
#define HAVE_SYS_UIO_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#define HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have the <sys/vfs.h> header file. */
#define HAVE_SYS_VFS_H 1

/* Define to 1 if you have the <sys/wait.h> header file. */
#define HAVE_SYS_WAIT_H 1

/* Define if TAILQ_FOREACH is defined in <sys/queue.h> */
#define HAVE_TAILQFOREACH 1

/* Define to 1 if you have the `tcgetpgrp' function. */
#define HAVE_TCGETPGRP 1

/* Define to 1 if you have the <termios.h> header file. */
#define HAVE_TERMIOS_H 1

/* Define if timeradd is defined in <sys/time.h> */
#define HAVE_TIMERADD 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the <ucontext.h> header file. */
#define HAVE_UCONTEXT_H 1

/* Define to 1 if the system has the type `uint128_t'. */
/* #undef HAVE_UINT128_T */

/* Define to 1 if the system has the type `uint16_t'. */
#define HAVE_UINT16_T 1

/* Define to 1 if the system has the type `uint32_t'. */
#define HAVE_UINT32_T 1

/* Define to 1 if the system has the type `uint64_t'. */
#define HAVE_UINT64_T 1

/* Define to 1 if the system has the type `uint8_t'. */
#define HAVE_UINT8_T 1

/* Define to 1 if the system has the type `uintptr_t'. */
#define HAVE_UINTPTR_T 1

/* Define to 1 if you have the <ulimit.h> header file. */
#define HAVE_ULIMIT_H 1

/* Define to 1 if you have the `uname' function. */
#define HAVE_UNAME 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* whether unix byteswap routines -- htonl, htons, nothl, ntohs -- are
   available */
#define HAVE_UNIX_BYTESWAP 1

/* Define to 1 if you have the `usleep' function. */
#define HAVE_USLEEP 1

/* Define to 1 if you have the <util.h> header file. */
/* #undef HAVE_UTIL_H */

/* Define to 1 if you have the <utmp.h> header file. */
#define HAVE_UTMP_H 1

/* Define to 1 if you have the <valgrind/valgrind.h> header file. */
/* #undef HAVE_VALGRIND_VALGRIND_H */

/* Define to 1 if you have the `vasprintf' function. */
#define HAVE_VASPRINTF 1

/* Define to 1 if you have the `vm_read_overwrite' function. */
/* #undef HAVE_VM_READ_OVERWRITE */

/* Define to 1 if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Define to 1 if you have the `waitpid' function. */
#define HAVE_WAITPID 1

/* Define if kqueue works correctly with pipes */
/* #undef HAVE_WORKING_KQUEUE */

/* Whether poll works for file descriptors and devices */
#define HAVE_WORKING_POLL 1

/* Define if realtime signals work on pipes */
/* #undef HAVE_WORKING_RTSIG */

/* Define to 1 if you have the `_NSGetEnviron' function. */
/* #undef HAVE__NSGETENVIRON */

/* Define to 1 if you have the `_strdup' function. */
/* #undef HAVE__STRDUP */

/* Define to 1 if you have the `__mmap' function. */
/* #undef HAVE___MMAP */

/* Define to 1 if you have the `__munmap' function. */
/* #undef HAVE___MUNMAP */

/* Define to 1 on AIX */
/* #undef HWLOC_AIX_SYS */

/* Whether C compiler supports symbol visibility or not */
#define HWLOC_C_HAVE_VISIBILITY 1

/* Define to 1 on Darwin */
/* #undef HWLOC_DARWIN_SYS */

/* Whether we are in debugging mode or not */
/* #undef HWLOC_DEBUG */

/* Version of hwloc */
/* #undef HWLOC_EXTERNAL_HWLOC_VERSION */

/* Define to 1 on *FREEBSD */
/* #undef HWLOC_FREEBSD_SYS */

/* Whether your compiler has __attribute__ or not */
#define HWLOC_HAVE_ATTRIBUTE 1

/* Whether your compiler has __attribute__ aligned or not */
#define HWLOC_HAVE_ATTRIBUTE_ALIGNED 1

/* Whether your compiler has __attribute__ always_inline or not */
#define HWLOC_HAVE_ATTRIBUTE_ALWAYS_INLINE 1

/* Whether your compiler has __attribute__ cold or not */
#define HWLOC_HAVE_ATTRIBUTE_COLD 1

/* Whether your compiler has __attribute__ const or not */
#define HWLOC_HAVE_ATTRIBUTE_CONST 1

/* Whether your compiler has __attribute__ deprecated or not */
#define HWLOC_HAVE_ATTRIBUTE_DEPRECATED 1

/* Whether your compiler has __attribute__ format or not */
#define HWLOC_HAVE_ATTRIBUTE_FORMAT 1

/* Whether your compiler has __attribute__ hot or not */
#define HWLOC_HAVE_ATTRIBUTE_HOT 1

/* Whether your compiler has __attribute__ malloc or not */
#define HWLOC_HAVE_ATTRIBUTE_MALLOC 1

/* Whether your compiler has __attribute__ may_alias or not */
#define HWLOC_HAVE_ATTRIBUTE_MAY_ALIAS 1

/* Whether your compiler has __attribute__ nonnull or not */
#define HWLOC_HAVE_ATTRIBUTE_NONNULL 1

/* Whether your compiler has __attribute__ noreturn or not */
#define HWLOC_HAVE_ATTRIBUTE_NORETURN 1

/* Whether your compiler has __attribute__ no_instrument_function or not */
#define HWLOC_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION 1

/* Whether your compiler has __attribute__ packed or not */
#define HWLOC_HAVE_ATTRIBUTE_PACKED 1

/* Whether your compiler has __attribute__ pure or not */
#define HWLOC_HAVE_ATTRIBUTE_PURE 1

/* Whether your compiler has __attribute__ sentinel or not */
#define HWLOC_HAVE_ATTRIBUTE_SENTINEL 1

/* Whether your compiler has __attribute__ unused or not */
#define HWLOC_HAVE_ATTRIBUTE_UNUSED 1

/* Whether your compiler has __attribute__ warn unused result or not */
#define HWLOC_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT 1

/* Whether your compiler has __attribute__ weak alias or not */
#define HWLOC_HAVE_ATTRIBUTE_WEAK_ALIAS 1

/* Define to 1 if your `ffs' function is known to be broken. */
/* #undef HWLOC_HAVE_BROKEN_FFS */

/* Define to 1 if you have the `clz' function. */
/* #undef HWLOC_HAVE_CLZ */

/* Define to 1 if you have the `clzl' function. */
/* #undef HWLOC_HAVE_CLZL */

/* Define to 1 if you have cpuid */
#define HWLOC_HAVE_CPUID 1

/* Define to 1 if the CPU_SET macro works */
#define HWLOC_HAVE_CPU_SET 1

/* Define to 1 if the CPU_SET_S macro works */
#define HWLOC_HAVE_CPU_SET_S 1

/* Define to 1 if function `clz' is declared by system headers */
/* #undef HWLOC_HAVE_DECL_CLZ */

/* Define to 1 if function `clzl' is declared by system headers */
/* #undef HWLOC_HAVE_DECL_CLZL */

/* Define to 1 if function `ffs' is declared by system headers */
#define HWLOC_HAVE_DECL_FFS 1

/* Define to 1 if function `ffsl' is declared by system headers */
#define HWLOC_HAVE_DECL_FFSL 1

/* Define to 1 if function `fls' is declared by system headers */
/* #undef HWLOC_HAVE_DECL_FLS */

/* Define to 1 if function `flsl' is declared by system headers */
/* #undef HWLOC_HAVE_DECL_FLSL */

/* Define to 1 if you have the `ffs' function. */
#define HWLOC_HAVE_FFS 1

/* Define to 1 if you have the `ffsl' function. */
#define HWLOC_HAVE_FFSL 1

/* Define to 1 if you have the `fls' function. */
/* #undef HWLOC_HAVE_FLS */

/* Define to 1 if you have the `flsl' function. */
/* #undef HWLOC_HAVE_FLSL */

/* Define to 1 if you have the `libpci' library. */
/* #undef HWLOC_HAVE_LIBPCI */

/* Define to 1 if you have the `libxml2' library. */
/* #undef HWLOC_HAVE_LIBXML2 */

/* Define to 1 if mbind is available. */
/* #undef HWLOC_HAVE_MBIND */

/* Define to 1 if migrate_pages is available. */
/* #undef HWLOC_HAVE_MIGRATE_PAGES */

/* Define to 1 if glibc provides the old prototype (without length) of
   sched_setaffinity() */
/* #undef HWLOC_HAVE_OLD_SCHED_SETAFFINITY */

/* Define to 1 if struct pci_dev has a `device_class' field. */
/* #undef HWLOC_HAVE_PCIDEV_DEVICE_CLASS */

/* Define to 1 if struct pci_dev has a `domain' field. */
/* #undef HWLOC_HAVE_PCIDEV_DOMAIN */

/* Define to 1 if `libpci' has the `pci_find_cap' function. */
/* #undef HWLOC_HAVE_PCI_FIND_CAP */

/* `Define to 1 if you have pthread_getthrds_np' */
/* #undef HWLOC_HAVE_PTHREAD_GETTHRDS_NP */

/* Define to 1 if glibc provides a prototype of sched_setaffinity() */
#define HWLOC_HAVE_SCHED_SETAFFINITY 1

/* Define to 1 if set_mempolicy is available. */
/* #undef HWLOC_HAVE_SET_MEMPOLICY */

/* Define to 1 if you have the <stdint.h> header file. */
#define HWLOC_HAVE_STDINT_H 1

/* Define to 1 if you have the `windows.h' header. */
/* #undef HWLOC_HAVE_WINDOWS_H */

/* Define to 1 if the _syscall3 macro works */
/* #undef HWLOC_HAVE__SYSCALL3 */

/* Define to 1 on HP-UX */
/* #undef HWLOC_HPUX_SYS */

/* Version of hwloc */
#define HWLOC_HWLOC132_HWLOC_VERSION "internal v1.3.2"

/* Define to 1 on Irix */
/* #undef HWLOC_IRIX_SYS */

/* Define to 1 on Linux */
#define HWLOC_LINUX_SYS 1

/* Define to 1 on OSF */
/* #undef HWLOC_OSF_SYS */

/* The size of `unsigned int', as computed by sizeof */
#define HWLOC_SIZEOF_UNSIGNED_INT 4

/* The size of `unsigned long', as computed by sizeof */
#define HWLOC_SIZEOF_UNSIGNED_LONG 8

/* Define to 1 on Solaris */
/* #undef HWLOC_SOLARIS_SYS */

/* The hwloc symbol prefix */
#define HWLOC_SYM_PREFIX opal_hwloc132_

/* The hwloc symbol prefix in all caps */
#define HWLOC_SYM_PREFIX_CAPS OPAL_HWLOC132_

/* Whether we need to re-define all the hwloc public symbols or not */
#define HWLOC_SYM_TRANSFORM 1

/* Define to 1 on unsupported systems */
/* #undef HWLOC_UNSUPPORTED_SYS */

/* Define to 1 on WINDOWS */
/* #undef HWLOC_WIN_SYS */

/* Define to 1 on x86_32 */
/* #undef HWLOC_X86_32_ARCH */

/* Define to 1 on x86_64 */
#define HWLOC_X86_64_ARCH 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Header to include for hwloc implementation */
#define MCA_hwloc_IMPLEMENTATION_HEADER "opal/mca/hwloc/hwloc132/hwloc132.h"

/* Complete set of command line arguments given to ROMIOs configure script */
#define MCA_io_romio_COMPLETE_CONFIGURE_FLAGS " CFLAGS='-DNDEBUG -g -O2 -finline-functions -fno-strict-aliasing -pthread' CPPFLAGS='  -I/home/meister/Development/clasp/externals/openmpi-1.6.5/opal/mca/hwloc/hwloc132/hwloc/include   -I/usr/include/infiniband -I/usr/include/infiniband' FFLAGS='' LDFLAGS=' ' --enable-shared --disable-static  --prefix=/home/meister/Development/clasp/build/cando.app/Contents/Resources/externals/common --with-mpi=open_mpi --disable-aio"

/* Set of user-defined configure flags given to ROMIOs configure script via
   --with-io-romio-flags */
#define MCA_io_romio_USER_CONFIGURE_FLAGS ""

/* Header to include for memcpy implementation */
#define MCA_memcpy_IMPLEMENTATION_HEADER "opal/mca/memcpy/base/memcpy_base_default.h"

/* Header to include for parts of the memory implementation */
#define MCA_memory_IMPLEMENTATION_HEADER "opal/mca/memory/base/empty.h"

/* Defined to 1 if mtl should use direct calls instead of components */
#define MCA_mtl_DIRECT_CALL 0

/* name of component to use for direct calls, if MCA_mtl_DIRECT_CALL is 1 */
#define MCA_mtl_DIRECT_CALL_COMPONENT 

/* Header mtl includes to be direct called */
#define MCA_mtl_DIRECT_CALL_HEADER ""

/* Defined to 1 if pml should use direct calls instead of components */
#define MCA_pml_DIRECT_CALL 0

/* name of component to use for direct calls, if MCA_pml_DIRECT_CALL is 1 */
#define MCA_pml_DIRECT_CALL_COMPONENT 

/* Header pml includes to be direct called */
#define MCA_pml_DIRECT_CALL_HEADER ""

/* Header to include for timer implementation */
#define MCA_timer_IMPLEMENTATION_HEADER "opal/mca/timer/linux/timer_linux.h"

/* Whether ptmalloc2 is supported on this system or not */
#define MEMORY_LINUX_PTMALLOC2 1

/* Whether ummunotify is supported on this system or not */
#define MEMORY_LINUX_UMMUNOTIFY 0

/* Whether we want to check MPI parameters always, never, or decide at
   run-time */
#define MPI_PARAM_CHECK ompi_mpi_param_check

/* MX installation provide access to the mx_open_board and
   mx__get_mapper_state functions */
/* #undef MX_HAVE_MAPPER_STATE */

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* Alignment of Fortran 77 CHARACTER */
#define OMPI_ALIGNMENT_FORTRAN_CHARACTER 1

/* Alignment of Fortran 77 COMPLEX */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX 4

/* Alignment of Fortran 77 COMPLEX*16 */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX16 8

/* Alignment of Fortran 77 COMPLEX*32 */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX32 4

/* Alignment of Fortran 77 COMPLEX*8 */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX8 4

/* Alignment of Fortran 77 DOUBLE COMPLEX */
#define OMPI_ALIGNMENT_FORTRAN_DOUBLE_COMPLEX 8

/* Alignment of Fortran 77 DOUBLE PRECISION */
#define OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION 8

/* Alignment of Fortran 77 INTEGER */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER 4

/* Alignment of Fortran 77 INTEGER*1 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER1 1

/* Alignment of Fortran 77 INTEGER*16 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER16 4

/* Alignment of Fortran 77 INTEGER*2 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER2 2

/* Alignment of Fortran 77 INTEGER*4 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER4 4

/* Alignment of Fortran 77 INTEGER*8 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER8 8

/* Alignment of Fortran 77 LOGICAL */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL 4

/* Alignment of Fortran 77 LOGICAL*1 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL1 1

/* Alignment of Fortran 77 LOGICAL*2 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL2 2

/* Alignment of Fortran 77 LOGICAL*4 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL4 4

/* Alignment of Fortran 77 LOGICAL*8 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL8 8

/* Alignment of Fortran 77 REAL */
#define OMPI_ALIGNMENT_FORTRAN_REAL 4

/* Alignment of Fortran 77 REAL*16 */
#define OMPI_ALIGNMENT_FORTRAN_REAL16 4

/* Alignment of Fortran 77 REAL*2 */
#define OMPI_ALIGNMENT_FORTRAN_REAL2 4

/* Alignment of Fortran 77 REAL*4 */
#define OMPI_ALIGNMENT_FORTRAN_REAL4 4

/* Alignment of Fortran 77 REAL*8 */
#define OMPI_ALIGNMENT_FORTRAN_REAL8 8

/* If knem support can be enabled */
#define OMPI_BTL_SM_HAVE_KNEM 0

/* OMPI underlying C++ compiler */
#define OMPI_CXX "g++"

/* Whether C++ compiler supports DEC style inline assembly */
#define OMPI_CXX_DEC_INLINE_ASSEMBLY 0

/* Whether C++ compiler supports GCC style inline assembly */
#define OMPI_CXX_GCC_INLINE_ASSEMBLY 1

/* Whether C++ compiler supports __builtin_expect */
#define OMPI_CXX_HAVE_BUILTIN_EXPECT 0

/* Whether C++ compiler supports __builtin_prefetch */
#define OMPI_CXX_HAVE_BUILTIN_PREFETCH 0

/* Whether a const_cast on a 2-d array will work with the C++ compiler */
#define OMPI_CXX_SUPPORTS_2D_CONST_CAST 1

/* Whether C++ compiler supports XLC style inline assembly */
#define OMPI_CXX_XLC_INLINE_ASSEMBLY 0

/* Enable contributed software package libompitrace */
#define OMPI_ENABLE_CONTRIB_libompitrace 1

/* Enable contributed software package vt */
#define OMPI_ENABLE_CONTRIB_vt 1

/* Enable features required for dynamic SL support */
#define OMPI_ENABLE_DYNAMIC_SL 0

/* Whether we want MPI profiling or not */
#define OMPI_ENABLE_MPI_PROFILING 1

/* Enable MPI_THREAD_MULTIPLE */
#define OMPI_ENABLE_THREAD_MULTIPLE 0

/* MPI Extended Interface Components */
#define OMPI_EXT_COMPONENTS " affinity example"

/* OMPI underlying F77 compiler */
#define OMPI_F77 "gfortran"

/* Whether fortran symbols are all caps or not */
#define OMPI_F77_CAPS 0

/* Whether fortran symbols have a trailing double underscore or not */
#define OMPI_F77_DOUBLE_UNDERSCORE 0

/* Whether fortran symbols have no trailing underscore or not */
#define OMPI_F77_PLAIN 0

/* Whether fortran symbols have a trailing underscore or not */
#define OMPI_F77_SINGLE_UNDERSCORE 1

/* OMPI underlying F90 compiler */
#define OMPI_F90 "gfortran"

/* Max handle value for fortran MPI handles, effectively min(INT_MAX, max
   fortran INTEGER value) */
#define OMPI_FORTRAN_HANDLE_MAX ( 2147483647 < 2147483647 ? 2147483647 : 2147483647 )

/* Fortran value for LOGICAL .TRUE. value */
#define OMPI_FORTRAN_VALUE_TRUE 1

/* Greek - alpha, beta, etc - release number of Open MPI */
#define OMPI_GREEK_VERSION ""

/* Wether we want sparse process groups */
#define OMPI_GROUP_SPARSE 0

/* whether to use cnos_barrier or not */
#define OMPI_GRPCOMM_CNOS_HAVE_BARRIER 0

/* Enable features required for ConnectX XRC support */
#define OMPI_HAVE_CONNECTX_XRC 0

/* Whether or not we have compiled with C++ exceptions support */
#define OMPI_HAVE_CXX_EXCEPTION_SUPPORT 0

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_CHARACTER 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_COMPLEX 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_COMPLEX16 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_COMPLEX32 0

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_COMPLEX8 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_DOUBLE_COMPLEX 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_DOUBLE_PRECISION 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_INTEGER 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_INTEGER1 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_INTEGER16 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_INTEGER2 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_INTEGER4 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_INTEGER8 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_LOGICAL 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_LOGICAL1 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_LOGICAL2 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_LOGICAL4 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_LOGICAL8 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_REAL 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_REAL16 0

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_REAL2 0

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_REAL4 1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#define OMPI_HAVE_F90_REAL8 1

/* Whether we have Fortran 77 CHARACTER or not */
#define OMPI_HAVE_FORTRAN_CHARACTER 1

/* Whether we have Fortran 77 COMPLEX or not */
#define OMPI_HAVE_FORTRAN_COMPLEX 1

/* Whether we have Fortran 77 COMPLEX*16 or not */
#define OMPI_HAVE_FORTRAN_COMPLEX16 1

/* Whether we have Fortran 77 COMPLEX*32 or not */
#define OMPI_HAVE_FORTRAN_COMPLEX32 0

/* Whether we have Fortran 77 COMPLEX*8 or not */
#define OMPI_HAVE_FORTRAN_COMPLEX8 1

/* Whether we have Fortran 77 DOUBLE COMPLEX or not */
#define OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX 1

/* Whether we have Fortran 77 DOUBLE PRECISION or not */
#define OMPI_HAVE_FORTRAN_DOUBLE_PRECISION 1

/* Whether we have Fortran 77 INTEGER or not */
#define OMPI_HAVE_FORTRAN_INTEGER 1

/* Whether we have Fortran 77 INTEGER*1 or not */
#define OMPI_HAVE_FORTRAN_INTEGER1 1

/* Whether we have Fortran 77 INTEGER*16 or not */
#define OMPI_HAVE_FORTRAN_INTEGER16 0

/* Whether we have Fortran 77 INTEGER*2 or not */
#define OMPI_HAVE_FORTRAN_INTEGER2 1

/* Whether we have Fortran 77 INTEGER*4 or not */
#define OMPI_HAVE_FORTRAN_INTEGER4 1

/* Whether we have Fortran 77 INTEGER*8 or not */
#define OMPI_HAVE_FORTRAN_INTEGER8 1

/* Whether we have Fortran 77 LOGICAL or not */
#define OMPI_HAVE_FORTRAN_LOGICAL 1

/* Whether we have Fortran 77 LOGICAL*1 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL1 1

/* Whether we have Fortran 77 LOGICAL*2 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL2 1

/* Whether we have Fortran 77 LOGICAL*4 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL4 1

/* Whether we have Fortran 77 LOGICAL*8 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL8 1

/* Whether we have Fortran 77 REAL or not */
#define OMPI_HAVE_FORTRAN_REAL 1

/* Whether we have Fortran 77 REAL*16 or not */
#define OMPI_HAVE_FORTRAN_REAL16 0

/* Whether we have Fortran 77 REAL*2 or not */
#define OMPI_HAVE_FORTRAN_REAL2 0

/* Whether we have Fortran 77 REAL*4 or not */
#define OMPI_HAVE_FORTRAN_REAL4 1

/* Whether we have Fortran 77 REAL*8 or not */
#define OMPI_HAVE_FORTRAN_REAL8 1

/* If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK */
#define OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK 1

/* If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK_NP */
#define OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP 1

/* Whether RDMA CM is available or not */
#define OMPI_HAVE_RDMACM 0

/* Enable RDMAoE support */
#define OMPI_HAVE_RDMAOE 1

/* Number of arguments to ibv_create_cq */
#define OMPI_IBV_CREATE_CQ_ARGS 5

/* Major release number of Open MPI */
#define OMPI_MAJOR_VERSION 1

/* False if you can use iovec's directly with SCTP BTL */
#define OMPI_MCA_BTL_SCTP_CONCATENATES_IOVS 0

/* Default value for socket style to use with SCTP BTL */
#define OMPI_MCA_BTL_SCTP_USE_ONE_TO_ONE_SOCKET 0

/* Whether any opal memory mca components were found */
#define OMPI_MEMORY_HAVE_COMPONENT 1

/* Minor release number of Open MPI */
#define OMPI_MINOR_VERSION 6

/* Size of the MPI_Offset */
#define OMPI_MPI_OFFSET_SIZE 8

/* Type of MPI_Offset -- has to be defined here and typedef'ed later because
   mpi.h does not get AC SUBST's */
#define OMPI_MPI_OFFSET_TYPE long long

/* MPI datatype corresponding to MPI_Offset */
#define OMPI_OFFSET_DATATYPE MPI_LONG_LONG

/* Add padding bytes to the openib control header */
#define OMPI_OPENIB_PAD_HDR 0

/* Use the Cray XT-3 implementation of Portals */
#define OMPI_PORTALS_CRAYXT3 0

/* Use the Cray XT-3 implementation of Portals using Modex */
#define OMPI_PORTALS_CRAYXT3_MODEX 0

/* Does Portals send a PTL_EVENT_UNLINK event */
#define OMPI_PORTALS_HAVE_EVENT_UNLINK 1

/* Use the UTCP reference implementation of Portals */
#define OMPI_PORTALS_UTCP 1

/* Whether OMPI should provide MPI File interface */
#define OMPI_PROVIDE_MPI_FILE_INTERFACE 1

/* Whether Fortran REAL*16 matches the bit format of the equivalent C type */
#define OMPI_REAL16_MATCHES_C 0

/* Release date of Open MPI */
#define OMPI_RELEASE_DATE "Jun 26, 2013"

/* Release release number of Open MPI */
#define OMPI_RELEASE_VERSION 5

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_CHARACTER 1

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_COMPLEX 8

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_COMPLEX16 16

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_COMPLEX32 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_COMPLEX8 8

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_DOUBLE_COMPLEX 16

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_DOUBLE_PRECISION 8

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_INTEGER 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_INTEGER1 1

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_INTEGER16 16

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_INTEGER2 2

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_INTEGER4 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_INTEGER8 8

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_LOGICAL 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_LOGICAL1 1

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_LOGICAL2 2

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_LOGICAL4 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_LOGICAL8 8

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_REAL 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_REAL16 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_REAL2 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_REAL4 4

/* Size of Fortran 77 $ofc_fortran_type */
#define OMPI_SIZEOF_F90_REAL8 8

/* Size of Fortran 77 CHARACTER */
#define OMPI_SIZEOF_FORTRAN_CHARACTER 1

/* Size of Fortran 77 COMPLEX */
#define OMPI_SIZEOF_FORTRAN_COMPLEX 8

/* Size of Fortran 77 COMPLEX*16 */
#define OMPI_SIZEOF_FORTRAN_COMPLEX16 16

/* Size of Fortran 77 COMPLEX*32 */
#define OMPI_SIZEOF_FORTRAN_COMPLEX32 4

/* Size of Fortran 77 COMPLEX*8 */
#define OMPI_SIZEOF_FORTRAN_COMPLEX8 8

/* Size of Fortran 77 DOUBLE COMPLEX */
#define OMPI_SIZEOF_FORTRAN_DOUBLE_COMPLEX 16

/* Size of Fortran 77 DOUBLE PRECISION */
#define OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION 8

/* Size of Fortran 77 INTEGER */
#define OMPI_SIZEOF_FORTRAN_INTEGER 4

/* Size of Fortran 77 INTEGER*1 */
#define OMPI_SIZEOF_FORTRAN_INTEGER1 1

/* Size of Fortran 77 INTEGER*16 */
#define OMPI_SIZEOF_FORTRAN_INTEGER16 16

/* Size of Fortran 77 INTEGER*2 */
#define OMPI_SIZEOF_FORTRAN_INTEGER2 2

/* Size of Fortran 77 INTEGER*4 */
#define OMPI_SIZEOF_FORTRAN_INTEGER4 4

/* Size of Fortran 77 INTEGER*8 */
#define OMPI_SIZEOF_FORTRAN_INTEGER8 8

/* Size of Fortran 77 LOGICAL */
#define OMPI_SIZEOF_FORTRAN_LOGICAL 4

/* Size of Fortran 77 LOGICAL*1 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL1 1

/* Size of Fortran 77 LOGICAL*2 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL2 2

/* Size of Fortran 77 LOGICAL*4 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL4 4

/* Size of Fortran 77 LOGICAL*8 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL8 8

/* Size of Fortran 77 REAL */
#define OMPI_SIZEOF_FORTRAN_REAL 4

/* Size of Fortran 77 REAL*16 */
#define OMPI_SIZEOF_FORTRAN_REAL16 4

/* Size of Fortran 77 REAL*2 */
#define OMPI_SIZEOF_FORTRAN_REAL2 4

/* Size of Fortran 77 REAL*4 */
#define OMPI_SIZEOF_FORTRAN_REAL4 4

/* Size of Fortran 77 REAL*8 */
#define OMPI_SIZEOF_FORTRAN_REAL8 8

/* Complete release number of Open MPI */
#define OMPI_VERSION "5"

/* Whether we want MPI C++ support or not */
#define OMPI_WANT_CXX_BINDINGS 1

/* Whether we want the MPI f77 bindings or not */
#define OMPI_WANT_F77_BINDINGS 1

/* Whether we want the MPI f90 bindings or not */
#define OMPI_WANT_F90_BINDINGS 1

/* if the memory and buffer checking should be enabled */
#define OMPI_WANT_MEMCHECKER 0

/* do we want to try to work around C++ bindings SEEK_* issue? */
#define OMPI_WANT_MPI_CXX_SEEK 1

/* Enable warnings in wrong usage (e.g. deprecated) in user-level code */
#define OMPI_WANT_MPI_INTERFACE_WARNING 0

/* if the peruse interface should be enabled */
#define OMPI_WANT_PERUSE 0

/* Alignment of type _Bool */
#define OPAL_ALIGNMENT_BOOL 1

/* Alignment of type char */
#define OPAL_ALIGNMENT_CHAR 1

/* Alignment of type bool */
#define OPAL_ALIGNMENT_CXX_BOOL 1

/* Alignment of type double */
#define OPAL_ALIGNMENT_DOUBLE 8

/* Alignment of type double _Complex */
#define OPAL_ALIGNMENT_DOUBLE_COMPLEX 8

/* Alignment of type float */
#define OPAL_ALIGNMENT_FLOAT 4

/* Alignment of type float _Complex */
#define OPAL_ALIGNMENT_FLOAT_COMPLEX 4

/* Alignment of type int */
#define OPAL_ALIGNMENT_INT 4

/* Alignment of type int128_t */
/* #undef OPAL_ALIGNMENT_INT128 */

/* Alignment of type int16_t */
#define OPAL_ALIGNMENT_INT16 2

/* Alignment of type int32_t */
#define OPAL_ALIGNMENT_INT32 4

/* Alignment of type int64_t */
#define OPAL_ALIGNMENT_INT64 8

/* Alignment of type int8_t */
#define OPAL_ALIGNMENT_INT8 1

/* Alignment of type long */
#define OPAL_ALIGNMENT_LONG 8

/* Alignment of type long double */
#define OPAL_ALIGNMENT_LONG_DOUBLE 16

/* Alignment of type long double _Complex */
#define OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX 16

/* Alignment of type long long */
#define OPAL_ALIGNMENT_LONG_LONG 8

/* Alignment of type short */
#define OPAL_ALIGNMENT_SHORT 2

/* Alignment of type void * */
#define OPAL_ALIGNMENT_VOID_P 8

/* Alignment of type wchar_t */
#define OPAL_ALIGNMENT_WCHAR 4

/* set to 1 if word-size integers must be aligned to word-size padding to
   prevent bus errors */
#define OPAL_ALIGN_WORD_SIZE_INTEGERS 0

/* OMPI architecture string */
#define OPAL_ARCH "x86_64-unknown-linux-gnu"

/* Assembly align directive expects logarithmic value */
#define OPAL_ASM_ALIGN_LOG 

/* What ARM assembly version to use */
/* #undef OPAL_ASM_ARM_VERSION */

/* Assembly directive for exporting symbols */
#define OPAL_ASM_GLOBAL ".globl"

/* Assembly prefix for gsym labels */
#define OPAL_ASM_GSYM ""

/* Assembly suffix for labels */
#define OPAL_ASM_LABEL_SUFFIX ":"

/* Assembly prefix for lsym labels */
#define OPAL_ASM_LSYM ".L"

/* Do we need to give a .size directive */
#define OPAL_ASM_SIZE "1"

/* Whether we can do 64bit assembly operations or not. Should not be used
   outside of the assembly header files */
#define OPAL_ASM_SUPPORT_64BIT 1

/* Assembly directive for setting text section */
#define OPAL_ASM_TEXT ".text"

/* How to set function type in .type directive */
#define OPAL_ASM_TYPE "@"

/* Architecture type of assembly to use for atomic operations */
#define OPAL_ASSEMBLY_ARCH OMPI_AMD64

/* Format of assembly file */
#define OPAL_ASSEMBLY_FORMAT "default-.text-.globl-:--.L-@-1-0-1-1-1"

/* The compiler $lower which OMPI was built with */
#define OPAL_BUILD_PLATFORM_COMPILER_FAMILYID 1

/* The compiler $lower which OMPI was built with */
#define OPAL_BUILD_PLATFORM_COMPILER_FAMILYNAME GNU

/* The compiler $lower which OMPI was built with */
#define OPAL_BUILD_PLATFORM_COMPILER_VERSION 264195

/* The compiler $lower which OMPI was built with */
#define OPAL_BUILD_PLATFORM_COMPILER_VERSION_STR 4.8.3

/* OMPI underlying C compiler */
#define OPAL_CC "gcc"

/* Use static const char[] strings for C files */
#define OPAL_CC_USE_CONST_CHAR_IDENT 0

/* Use #ident strings for C files */
#define OPAL_CC_USE_IDENT 1

/* Use #pragma comment for C files */
#define OPAL_CC_USE_PRAGMA_COMMENT 

/* Use #pragma ident strings for C files */
#define OPAL_CC_USE_PRAGMA_IDENT 0

/* OPAL underlying C++ compiler */
#define OPAL_CXX "g++"

/* Use static const char[] strings for C++ files */
#define OPAL_CXX_USE_CONST_CHAR_IDENT 0

/* Use #ident strings for C++ files */
#define OPAL_CXX_USE_IDENT 1

/* Use #pragma comment for C++ files */
#define OPAL_CXX_USE_PRAGMA_COMMENT 

/* Use #pragma ident strings for C++ files */
#define OPAL_CXX_USE_PRAGMA_IDENT 0

/* Whether C compiler supports DEC style inline assembly */
#define OPAL_C_DEC_INLINE_ASSEMBLY 0

/* Whether C compiler supports GCC style inline assembly */
#define OPAL_C_GCC_INLINE_ASSEMBLY 1

/* Whether C compiler supports __builtin_expect */
#define OPAL_C_HAVE_BUILTIN_EXPECT 1

/* Whether C compiler supports __builtin_prefetch */
#define OPAL_C_HAVE_BUILTIN_PREFETCH 1

/* Whether C compiler supports symbol visibility or not */
#define OPAL_C_HAVE_VISIBILITY 1

/* Whether C compiler supports XLC style inline assembly */
#define OPAL_C_XLC_INLINE_ASSEMBLY 0

/* Whether we want developer-level debugging code or not */
#define OPAL_ENABLE_DEBUG 0

/* Enable fault tolerance general components and logic */
#define OPAL_ENABLE_FT 0

/* Enable fault tolerance checkpoint/restart components and logic */
#define OPAL_ENABLE_FT_CR 0

/* Enable fault tolerance thread in Open PAL */
#define OPAL_ENABLE_FT_THREAD 0

/* Enable features required for heterogeneous support */
#define OPAL_ENABLE_HETEROGENEOUS_SUPPORT 0

/* Enable IPv6 support, but only if the underlying system supports it */
#define OPAL_ENABLE_IPV6 0

/* Whether we want the memory profiling or not */
#define OPAL_ENABLE_MEM_DEBUG 0

/* Whether we want the memory profiling or not */
#define OPAL_ENABLE_MEM_PROFILE 0

/* Whether we should enable OPAL support for threads */
#define OPAL_ENABLE_MULTI_THREADS 0

/* Hardcode the OPAL progress thread to be off */
#define OPAL_ENABLE_PROGRESS_THREADS 0

/* Whether user wants PTY support or not */
#define OPAL_ENABLE_PTY_SUPPORT 1

/* Enable run-time tracing of internal functions */
#define OPAL_ENABLE_TRACE 0

/* Greek - alpha, beta, etc - release number of Open Portable Access Layer */
#define OPAL_GREEK_VERSION ""

/* Whether there is an atomic assembly file available */
#define OPAL_HAVE_ASM_FILE 1

/* Whether your compiler has __attribute__ or not */
#define OPAL_HAVE_ATTRIBUTE 1

/* Whether your compiler has __attribute__ aligned or not */
#define OPAL_HAVE_ATTRIBUTE_ALIGNED 1

/* Whether your compiler has __attribute__ always_inline or not */
#define OPAL_HAVE_ATTRIBUTE_ALWAYS_INLINE 1

/* Whether your compiler has __attribute__ cold or not */
#define OPAL_HAVE_ATTRIBUTE_COLD 1

/* Whether your compiler has __attribute__ const or not */
#define OPAL_HAVE_ATTRIBUTE_CONST 1

/* Whether your compiler has __attribute__ deprecated or not */
#define OPAL_HAVE_ATTRIBUTE_DEPRECATED 1

/* Whether your compiler has __attribute__ deprecated with optional argument
   */
#define OPAL_HAVE_ATTRIBUTE_DEPRECATED_ARGUMENT 1

/* Whether your compiler has __attribute__ format or not */
#define OPAL_HAVE_ATTRIBUTE_FORMAT 1

/* Whether your compiler has __attribute__ format and it works on function
   pointers */
#define OPAL_HAVE_ATTRIBUTE_FORMAT_FUNCPTR 1

/* Whether your compiler has __attribute__ hot or not */
#define OPAL_HAVE_ATTRIBUTE_HOT 1

/* Whether your compiler has __attribute__ malloc or not */
#define OPAL_HAVE_ATTRIBUTE_MALLOC 1

/* Whether your compiler has __attribute__ may_alias or not */
#define OPAL_HAVE_ATTRIBUTE_MAY_ALIAS 1

/* Whether your compiler has __attribute__ nonnull or not */
#define OPAL_HAVE_ATTRIBUTE_NONNULL 1

/* Whether your compiler has __attribute__ noreturn or not */
#define OPAL_HAVE_ATTRIBUTE_NORETURN 1

/* Whether your compiler has __attribute__ noreturn and it works on function
   pointers */
#define OPAL_HAVE_ATTRIBUTE_NORETURN_FUNCPTR 1

/* Whether your compiler has __attribute__ no_instrument_function or not */
#define OPAL_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION 1

/* Whether your compiler has __attribute__ packed or not */
#define OPAL_HAVE_ATTRIBUTE_PACKED 1

/* Whether your compiler has __attribute__ pure or not */
#define OPAL_HAVE_ATTRIBUTE_PURE 1

/* Whether your compiler has __attribute__ sentinel or not */
#define OPAL_HAVE_ATTRIBUTE_SENTINEL 1

/* Whether your compiler has __attribute__ unused or not */
#define OPAL_HAVE_ATTRIBUTE_UNUSED 1

/* Whether your compiler has __attribute__ visibility or not */
#define OPAL_HAVE_ATTRIBUTE_VISIBILITY 1

/* Whether your compiler has __attribute__ warn unused result or not */
#define OPAL_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT 1

/* Whether your compiler has __attribute__ weak alias or not */
#define OPAL_HAVE_ATTRIBUTE_WEAK_ALIAS 1

/* whether qsort is broken or not */
#define OPAL_HAVE_BROKEN_QSORT 0

/* Whether we have hwloc support or not */
#define OPAL_HAVE_HWLOC 1

/* Do not use outside of mpi.h. Define to 1 if the system has the type `long
   long'. */
#define OPAL_HAVE_LONG_LONG 1

/* Whether libltdl appears to have the lt_dladvise interface */
#define OPAL_HAVE_LTDL_ADVISE 0

/* Do we have POSIX threads */
#define OPAL_HAVE_POSIX_THREADS 1

/* Whether we have SA_RESTART in <signal.h> or not */
#define OPAL_HAVE_SA_RESTART 1

/* Do we have native Solaris threads */
#define OPAL_HAVE_SOLARIS_THREADS 0

/* Do not use outside of mpi.h. Define to 1 if you have the <sys/synch.h>
   header file. */
/* #undef OPAL_HAVE_SYS_SYNCH_H */

/* Do not use outside of mpi.h. Define to 1 if you have the <sys/time.h>
   header file. */
#define OPAL_HAVE_SYS_TIME_H 1

/* Whether we have __va_copy or not */
#define OPAL_HAVE_UNDERSCORE_VA_COPY 1

/* Whether we have va_copy or not */
#define OPAL_HAVE_VA_COPY 1

/* Whether we have weak symbols or not */
#define OPAL_HAVE_WEAK_SYMBOLS 1

/* Define to 1 ifyou have the declaration of _SC_NPROCESSORS_ONLN, and to 0
   otherwise */
#define OPAL_HAVE__SC_NPROCESSORS_ONLN 1

/* ident string for Open MPI */
#define OPAL_IDENT_STRING "1.6.5"

/* Whether we are using the internal libltdl or not */
#define OPAL_LIBLTDL_INTERNAL 1

/* Major release number of Open Portable Access Layer */
#define OPAL_MAJOR_VERSION 1

/* Maximum length of datarep strings (default is 128) */
#define OPAL_MAX_DATAREP_STRING 128

/* Maximum length of error strings (default is 256) */
#define OPAL_MAX_ERROR_STRING 256

/* Maximum length of info keys (default is 36) */
#define OPAL_MAX_INFO_KEY 36

/* Maximum length of info vals (default is 256) */
#define OPAL_MAX_INFO_VAL 256

/* Maximum length of object names (default is 64) */
#define OPAL_MAX_OBJECT_NAME 64

/* Maximum length of port names (default is 1024) */
#define OPAL_MAX_PORT_NAME 1024

/* Maximum length of processor names (default is 256) */
#define OPAL_MAX_PROCESSOR_NAME 256

/* Minor release number of Open Portable Access Layer */
#define OPAL_MINOR_VERSION 6

/* Whether the C compiler supports "bool" without any other help (such as
   <stdbool.h>) */
#define OPAL_NEED_C_BOOL 1

/* package/branding string for Open MPI */
#define OPAL_PACKAGE_STRING "Open MPI meister@compute.temple.edu Distribution"

/* Whether r notation is used for ppc registers */
/* #undef OPAL_POWERPC_R_REGISTERS */

/* type to use for ptrdiff_t */
#define OPAL_PTRDIFF_TYPE ptrdiff_t

/* Release date of Open Portable Access Layer */
#define OPAL_RELEASE_DATE "Jun 26, 2013"

/* Release release number of Open Portable Access Layer */
#define OPAL_RELEASE_VERSION 5

/* Whether we have shared memory support for mmap or not */
#define OPAL_SHMEM_MMAP 1

/* Whether we have shared memory support for POSIX or not */
#define OPAL_SHMEM_POSIX 1

/* Whether we have shared memory support for SYSV or not */
#define OPAL_SHMEM_SYSV 1

/* Whether we have shared memory support for POSIX or not */
#define OPAL_SHMEM_WINDOWS 0

/* Do not use outside of mpi.h. Define to 1 if you have the ANSI C header
   files. */
#define OPAL_STDC_HEADERS 1

/* Do threads have different pids (pthreads on linux) */
#define OPAL_THREADS_HAVE_DIFFERENT_PIDS 0

/* Whether to use <stdbool.h> or not */
#define OPAL_USE_STDBOOL_H 1

/* Complete release number of Open Portable Access Layer */
#define OPAL_VERSION "5"

/* Enable per-user config files */
#define OPAL_WANT_HOME_CONFIG_FILES 1

/* Whether to include support for libltdl or not */
#define OPAL_WANT_LIBLTDL 1

/* if want pretty-print stack trace feature */
#define OPAL_WANT_PRETTY_PRINT_STACKTRACE 1

/* whether we want to have smp locks in atomic ops or not */
#define OPAL_WANT_SMP_LOCKS 1

/* Specific ps command to use in orte-clean */
#define ORTE_CLEAN_PS_CMD "ps -A -o fname,pid,user"

/* Build full RTE support */
#define ORTE_DISABLE_FULL_SUPPORT 0

/* Greek - alpha, beta, etc - release number of Open MPI Run-Time Environment
   */
#define ORTE_GREEK_VERSION ""

/* Major release number of Open MPI Run-Time Environment */
#define ORTE_MAJOR_VERSION 1

/* Whether we have CNOS support in alps ess or not */
#define ORTE_MCA_ESS_ALPS_HAVE_CNOS 0

/* Minor release number of Open MPI Run-Time Environment */
#define ORTE_MINOR_VERSION 6

/* Release date of Open MPI Run-Time Environment */
#define ORTE_RELEASE_DATE "Jun 26, 2013"

/* Release release number of Open MPI Run-Time Environment */
#define ORTE_RELEASE_VERSION 5

/* Complete release number of Open MPI Run-Time Environment */
#define ORTE_VERSION "5"

/* Whether we want orterun to effect "--prefix $prefix" by default */
#define ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT 0

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "http://www.open-mpi.org/community/help/"

/* Define to the full name of this package. */
#define PACKAGE_NAME "Open MPI"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "Open MPI 1.6.5"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "openmpi"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.6.5"

/* The size of `bool', as computed by sizeof. */
#define SIZEOF_BOOL 1

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of `double _Complex', as computed by sizeof. */
#define SIZEOF_DOUBLE__COMPLEX 16

/* The size of `float', as computed by sizeof. */
#define SIZEOF_FLOAT 4

/* The size of `float _Complex', as computed by sizeof. */
#define SIZEOF_FLOAT__COMPLEX 8

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `long double', as computed by sizeof. */
#define SIZEOF_LONG_DOUBLE 16

/* The size of `long double _Complex', as computed by sizeof. */
#define SIZEOF_LONG_DOUBLE__COMPLEX 32

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `pid_t', as computed by sizeof. */
#define SIZEOF_PID_T 4

/* The size of `ptrdiff_t', as computed by sizeof. */
#define SIZEOF_PTRDIFF_T 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T 8

/* The size of `ssize_t', as computed by sizeof. */
#define SIZEOF_SSIZE_T 8

/* The size of `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* The size of `wchar_t', as computed by sizeof. */
#define SIZEOF_WCHAR_T 4

/* The size of `_Bool', as computed by sizeof. */
#define SIZEOF__BOOL 1

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Enable extensions on HP-UX. */
#ifndef _HPUX_SOURCE
# define _HPUX_SOURCE 1
#endif


/* Whether to use the legacy Solaris munmap prototype or not */
/* #undef USE_SOLARIS_LEGACY_MUNMAP_PROTOTYPE */

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Whether we want to use Cray PMI2 extensions */
#define WANT_CRAY_PMI2_EXT 

/* Whether we want PMI support */
#define WANT_PMI_SUPPORT 0

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

/* Additional CFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_CFLAGS "-pthread "

/* Additional CXXFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_CXXFLAGS "-pthread "

/* Additional FCFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_FCFLAGS "-pthread "

/* Additional FFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_FFLAGS "-pthread "

/* Additional LDFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_LDFLAGS "  "

/* Additional LIBS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_LIBS "-ldl  -lm  -Wl,--export-dynamic -lrt -lnsl -lutil -lm -ldl "

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#define YYTEXT_POINTER 1

/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif

/* Are we building for HP-UX? */
#define _HPUX_SOURCE 1

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define this to the process ID type */
#define hwloc_pid_t pid_t

/* Define this to either strncasecmp or strncmp */
#define hwloc_strncasecmp strncasecmp

/* Define this to the thread ID type */
#define hwloc_thread_t pthread_t

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
#define inline __inline__
#endif

/* A bogus type that allows us to have sentinel type values that are still
   valid */
#define ompi_fortran_bogus_type_t int

/* C type corresponding to Fortran 77 CHARACTER */
#define ompi_fortran_character_t char

/* C type corresponding to Fortran 77 COMPLEX*16 */
/* #undef ompi_fortran_complex16_t */

/* C type corresponding to Fortran 77 COMPLEX*32 */
/* #undef ompi_fortran_complex32_t */

/* C type corresponding to Fortran 77 COMPLEX*8 */
/* #undef ompi_fortran_complex8_t */

/* C type corresponding to Fortran 77 COMPLEX */
/* #undef ompi_fortran_complex_t */

/* C type corresponding to Fortran 77 DOUBLE COMPLEX */
/* #undef ompi_fortran_double_complex_t */

/* C type corresponding to Fortran 77 DOUBLE PRECISION */
#define ompi_fortran_double_precision_t double

/* C type corresponding to Fortran 77 INTEGER*16 */
#define ompi_fortran_integer16_t 

/* C type corresponding to Fortran 77 INTEGER*1 */
#define ompi_fortran_integer1_t char

/* C type corresponding to Fortran 77 INTEGER*2 */
#define ompi_fortran_integer2_t short

/* C type corresponding to Fortran 77 INTEGER*4 */
#define ompi_fortran_integer4_t int

/* C type corresponding to Fortran 77 INTEGER*8 */
#define ompi_fortran_integer8_t long long

/* C type corresponding to Fortran 77 INTEGER */
#define ompi_fortran_integer_t int

/* C type corresponding to Fortran 77 LOGICAL*1 */
#define ompi_fortran_logical1_t char

/* C type corresponding to Fortran 77 LOGICAL*2 */
#define ompi_fortran_logical2_t short

/* C type corresponding to Fortran 77 LOGICAL*4 */
#define ompi_fortran_logical4_t int

/* C type corresponding to Fortran 77 LOGICAL*8 */
#define ompi_fortran_logical8_t long long

/* C type corresponding to Fortran 77 LOGICAL */
#define ompi_fortran_logical_t int

/* C type corresponding to Fortran 77 REAL*16 */
#define ompi_fortran_real16_t ompi_fortran_bogus_type_t

/* C type corresponding to Fortran 77 REAL*2 */
#define ompi_fortran_real2_t ompi_fortran_bogus_type_t

/* C type corresponding to Fortran 77 REAL*4 */
#define ompi_fortran_real4_t float

/* C type corresponding to Fortran 77 REAL*8 */
#define ompi_fortran_real8_t double

/* C type corresponding to Fortran 77 REAL */
#define ompi_fortran_real_t float

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#define restrict __restrict
/* Work around a bug in Sun C++: it does not support _Restrict or
   __restrict__, even though the corresponding Sun C compiler ends up with
   "#define restrict _Restrict" or "#define restrict __restrict__" in the
   previous line.  Perhaps some future version of Sun C++ will work with
   restrict; if so, hopefully it defines __RESTRICT like Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
# define __restrict__
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */


#include "opal_config_bottom.h"
#endif /* OPAL_CONFIG_H */

