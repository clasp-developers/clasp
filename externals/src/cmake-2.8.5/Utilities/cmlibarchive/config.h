/* config.h.  Generated from config.h.cmake by cmake configure */
#if defined(__osf__)
# define _OSF_SOURCE
#endif

/* Version number of bsdcpio */
#define BSDCPIO_VERSION_STRING "2.7.900a"

/* Version number of bsdtar */
#define BSDTAR_VERSION_STRING "2.7.900a"

/* Define to 1 if you have the `acl_create_entry' function. */
/* #undef HAVE_ACL_CREATE_ENTRY */

/* Define to 1 if you have the `acl_get_link' function. */
/* #undef HAVE_ACL_GET_LINK */

/* Define to 1 if you have the `acl_get_link_np' function. */
/* #undef HAVE_ACL_GET_LINK_NP */

/* Define to 1 if you have the `acl_get_perm' function. */
/* #undef HAVE_ACL_GET_PERM */

/* Define to 1 if you have the `acl_get_perm_np' function. */
/* #undef HAVE_ACL_GET_PERM_NP */

/* Define to 1 if you have the `acl_init' function. */
/* #undef HAVE_ACL_INIT */

/* Define to 1 if you have the <acl/libacl.h> header file. */
/* #undef HAVE_ACL_LIBACL_H */

/* Define to 1 if the system has the type `acl_permset_t'. */
/* #undef HAVE_ACL_PERMSET_T */

/* Define to 1 if you have the `acl_set_fd' function. */
/* #undef HAVE_ACL_SET_FD */

/* Define to 1 if you have the `acl_set_fd_np' function. */
/* #undef HAVE_ACL_SET_FD_NP */

/* Define to 1 if you have the `acl_set_file' function. */
/* #undef HAVE_ACL_SET_FILE */

/* True for systems with POSIX ACL support */
/* #undef HAVE_ACL_USER */

/* Define to 1 if you have the <attr/xattr.h> header file. */
/* #undef HAVE_ATTR_XATTR_H */

/* Define to 1 if you have the <bzlib.h> header file. */
#define HAVE_BZLIB_H 1

/* Define to 1 if you have the `chflags' function. */
#define HAVE_CHFLAGS 1

/* Define to 1 if you have the `chown' function. */
#define HAVE_CHOWN 1

/* Define to 1 if you have the `chroot' function. */
#define HAVE_CHROOT 1

/* Define to 1 if you have the `CreateHardLinkA' function. */
/* #undef HAVE_CREATEHARDLINKA */

/* Define to 1 if you have the `CreateHardLinkW' function. */
/* #undef HAVE_CREATEHARDLINKW */

/* Define to 1 if you have the <ctype.h> header file. */
#define HAVE_CTYPE_H 1

/* Define to 1 if you have the declaration of `INT64_MAX', and to 0 if you
   don't. */
#define HAVE_DECL_INT64_MAX 1

/* Define to 1 if you have the declaration of `INT64_MIN', and to 0 if you
   don't. */
#define HAVE_DECL_INT64_MIN 1

/* Define to 1 if you have the declaration of `optarg', and to 0 if you don't.
   */
#define HAVE_DECL_OPTARG 1

/* Define to 1 if you have the declaration of `optind', and to 0 if you don't.
   */
#define HAVE_DECL_OPTIND 1

/* Define to 1 if you have the declaration of `SIZE_MAX', and to 0 if you
   don't. */
#define HAVE_DECL_SIZE_MAX 1

/* Define to 1 if you have the declaration of `SSIZE_MAX', and to 0 if you
   don't. */
#define HAVE_DECL_SSIZE_MAX 1

/* Define to 1 if you have the declaration of `strerror_r', and to 0 if you
   don't. */
#define HAVE_DECL_STRERROR_R 1

/* Define to 1 if you have the declaration of `UINT32_MAX', and to 0 if you
   don't. */
#define HAVE_DECL_UINT32_MAX 1

/* Define to 1 if you have the declaration of `UINT64_MAX', and to 0 if you
   don't. */
#define HAVE_DECL_UINT64_MAX 1

/* Define to 1 if you have the <direct.h> header file. */
/* #undef HAVE_DIRECT_H */

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
/* #undef HAVE_DOPRNT */

/* Define to 1 if nl_langinfo supports D_MD_ORDER */
#define HAVE_D_MD_ORDER 1

/* A possible errno value for invalid file format errors */
#define HAVE_EFTYPE 1

/* A possible errno value for invalid file format errors */
#define HAVE_EILSEQ 1

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the <ext2fs/ext2_fs.h> header file. */
/* #undef HAVE_EXT2FS_EXT2_FS_H */

/* Define to 1 if you have the `extattr_get_file' function. */
/* #undef HAVE_EXTATTR_GET_FILE */

/* Define to 1 if you have the `extattr_list_file' function. */
/* #undef HAVE_EXTATTR_LIST_FILE */

/* Define to 1 if you have the `extattr_set_fd' function. */
/* #undef HAVE_EXTATTR_SET_FD */

/* Define to 1 if you have the `extattr_set_file' function. */
/* #undef HAVE_EXTATTR_SET_FILE */

/* Define to 1 if you have the `fchdir' function. */
#define HAVE_FCHDIR 1

/* Define to 1 if you have the `fchflags' function. */
#define HAVE_FCHFLAGS 1

/* Define to 1 if you have the `fchmod' function. */
#define HAVE_FCHMOD 1

/* Define to 1 if you have the `fchown' function. */
#define HAVE_FCHOWN 1

/* Define to 1 if you have the `fcntl' function. */
#define HAVE_FCNTL 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
#define HAVE_FSEEKO 1

/* Define to 1 if you have the `fsetxattr' function. */
#define HAVE_FSETXATTR 1

/* Define to 1 if you have the `fstat' function. */
#define HAVE_FSTAT 1

/* Define to 1 if you have the `ftruncate' function. */
#define HAVE_FTRUNCATE 1

/* Define to 1 if you have the `futimes' function. */
#define HAVE_FUTIMES 1

/* Define to 1 if you have the `geteuid' function. */
#define HAVE_GETEUID 1

/* Define to 1 if you have the `getpid' function. */
#define HAVE_GETPID 1

/* Define to 1 if you have the `getxattr' function. */
#define HAVE_GETXATTR 1

/* Define to 1 if you have the <grp.h> header file. */
#define HAVE_GRP_H 1

/* Define to 1 if the system has the type `intmax_t'. */
#define HAVE_INTMAX_T 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <io.h> header file. */
/* #undef HAVE_IO_H */

/* Define to 1 if you have the <langinfo.h> header file. */
#define HAVE_LANGINFO_H 1

/* Define to 1 if you have the `lchflags' function. */
#define HAVE_LCHFLAGS 1

/* Define to 1 if you have the `lchmod' function. */
#define HAVE_LCHMOD 1

/* Define to 1 if you have the `lchown' function. */
#define HAVE_LCHOWN 1

/* Define to 1 if you have the `lgetxattr' function. */
/* #undef HAVE_LGETXATTR */

/* Define to 1 if you have the `acl' library (-lacl). */
/* #undef HAVE_LIBACL */

/* Define to 1 if you have the `attr' library (-lattr). */
/* #undef HAVE_LIBATTR */

/* Define to 1 if you have the `bz2' library (-lbz2). */
#define HAVE_LIBBZ2 1

/* Define to 1 if you have the `lzma' library (-llzma). */
/* #undef HAVE_LIBLZMA */

/* Define to 1 if you have the `lzmadec' library (-llzmadec). */
/* #undef HAVE_LIBLZMADEC */

/* Define to 1 if you have the `z' library (-lz). */
#define HAVE_LIBZ 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the link() function. */
#define HAVE_LINK 1

/* Define to 1 if you have the <linux/fs.h> header file. */
/* #undef HAVE_LINUX_FS_H */

/* Define to 1 if you have the `listxattr' function. */
#define HAVE_LISTXATTR 1

/* Define to 1 if you have the `llistxattr' function. */
/* #undef HAVE_LLISTXATTR */

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if the system has the type `long long int'. */
#define HAVE_LONG_LONG_INT 1

/* Define to 1 if you have the `lsetxattr' function. */
/* #undef HAVE_LSETXATTR */

/* Define to 1 if you have the `lstat' function. */
#define HAVE_LSTAT 1

/* Define to 1 if `lstat' has the bug that it succeeds when given the
   zero-length file name argument. */
/* #undef HAVE_LSTAT_EMPTY_STRING_BUG */

/* Define to 1 if you have the `lutimes' function. */
#define HAVE_LUTIMES 1

/* Define to 1 if you have the <lzmadec.h> header file. */
/* #undef HAVE_LZMADEC_H */

/* Define to 1 if you have the <lzma.h> header file. */
/* #undef HAVE_LZMA_H */

/* Define to 1 if you have the `MD5' functions. */
/* #undef HAVE_MD5 */

/* Define to 1 if you have the <md5.h> header file. */
/* #undef HAVE_MD5_H */

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkdir' function. */
#define HAVE_MKDIR 1

/* Define to 1 if you have the `mkfifo' function. */
#define HAVE_MKFIFO 1

/* Define to 1 if you have the `mknod' function. */
#define HAVE_MKNOD 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the `nl_langinfo' function. */
#define HAVE_NL_LANGINFO 1

/* Define to 1 if you have the <openssl/md5.h> header file. */
#define HAVE_OPENSSL_MD5_H 1

/* Define to 1 if you have the <openssl/ripemd.h> header file. */
#define HAVE_OPENSSL_RIPEMD_H 1

/* Define to 1 if you have the <openssl/sha.h> header file. */
#define HAVE_OPENSSL_SHA_H 1

/* Define to 1 if you have the <paths.h> header file. */
#define HAVE_PATHS_H 1

/* Define to 1 if you have the `pipe' function. */
#define HAVE_PIPE 1

/* Define to 1 if you have the `poll' function. */
#define HAVE_POLL 1

/* Define to 1 if you have the <poll.h> header file. */
#define HAVE_POLL_H 1

/* Define to 1 if you have the <process.h> header file. */
/* #undef HAVE_PROCESS_H */

/* Define to 1 if you have the <pwd.h> header file. */
#define HAVE_PWD_H 1

/* Define to 1 if you have the `readlink' function. */
#define HAVE_READLINK 1

/* Define to 1 if you have the <regex.h> header file. */
#define HAVE_REGEX_H 1

/* Define to 1 if you have the <ripemd.h> header file. */
/* #undef HAVE_RIPEMD_H */

/* Define to 1 if you have the `RIPEMD160' functions. */
/* #undef HAVE_RMD160 */

/* Define to 1 if you have the <rmd160.h> header file. */
/* #undef HAVE_RMD160_H */

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the `SHA1' functions. */
/* #undef HAVE_SHA1 */

/* Define to 1 if you have the <sha1.h> header file. */
/* #undef HAVE_SHA1_H */

/* Define to 1 if you have the `SHA256' functions. */
/* #undef HAVE_SHA256 */

/* Define to 1 if you have the <sha256.h> header file. */
/* #undef HAVE_SHA256_H */

/* Define to 1 if you have the <sha2.h> header file. */
/* #undef HAVE_SHA2_H */

/* Define to 1 if you have the `SHA384' functions. */
/* #undef HAVE_SHA384 */

/* Define to 1 if you have the `SHA512' functions. */
/* #undef HAVE_SHA512 */

/* Define to 1 if you have the <sha.h> header file. */
/* #undef HAVE_SHA_H */

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if `stat' has the bug that it succeeds when given the
   zero-length file name argument. */
/* #undef HAVE_STAT_EMPTY_STRING_BUG */

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the `strerror_r' function. */
#define HAVE_STRERROR_R 1

/* Define to 1 if you have the `strftime' function. */
#define HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strrchr' function. */
#define HAVE_STRRCHR 1

/* Define to 1 if `st_birthtime' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BIRTHTIME 1

/* Define to 1 if `st_birthtimespec.tv_nsec' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BIRTHTIMESPEC_TV_NSEC 1

/* Define to 1 if `st_blksize' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BLKSIZE 1

/* Define to 1 if `st_flags' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_FLAGS 1

/* Define to 1 if `st_mtimespec.tv_nsec' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_MTIMESPEC_TV_NSEC 1

/* Define to 1 if `st_mtime_n' is member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_MTIME_N */

/* Define to 1 if `st_mtime_usec' is member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_MTIME_USEC */

/* Define to 1 if `st_mtim.tv_nsec' is member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_MTIM_TV_NSEC */

/* Define to 1 if `st_umtime' is member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_UMTIME */

/* Define to 1 if you have the symlink() function. */
#define HAVE_SYMLINK 1

/* Define to 1 if you have the <sys/acl.h> header file. */
#define HAVE_SYS_ACL_H 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/extattr.h> header file. */
/* #undef HAVE_SYS_EXTATTR_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/mkdev.h> header file. */
/* #undef HAVE_SYS_MKDEV_H */

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/poll.h> header file. */
#define HAVE_SYS_POLL_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/utime.h> header file. */
/* #undef HAVE_SYS_UTIME_H */

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the <sys/xattr.h> header file. */
#define HAVE_SYS_XATTR_H 1

/* Define to 1 if you have the `timegm' function. */
#define HAVE_TIMEGM 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the `tzset' function. */
#define HAVE_TZSET 1

/* Define to 1 if the system has the type `uintmax_t'. */
#define HAVE_UINTMAX_T 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `unsetenv' function. */
#define HAVE_UNSETENV 1

/* Define to 1 if the system has the type `unsigned long long'. */
#define HAVE_UNSIGNED_LONG_LONG 1

/* Define to 1 if the system has the type `unsigned long long int'. */
#define HAVE_UNSIGNED_LONG_LONG_INT 1

/* Define to 1 if you have the `utime' function. */
#define HAVE_UTIME 1

/* Define to 1 if you have the `utimes' function. */
#define HAVE_UTIMES 1

/* Define to 1 if you have the <utime.h> header file. */
#define HAVE_UTIME_H 1

/* Define to 1 if you have the `vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if the system has the type `wchar_t'. */
#define HAVE_WCHAR_T 1

/* Define to 1 if you have the `wcrtomb' function. */
#define HAVE_WCRTOMB 1

/* Define to 1 if you have the `wcscpy' function. */
#define HAVE_WCSCPY 1

/* Define to 1 if you have the `wcslen' function. */
#define HAVE_WCSLEN 1

/* Define to 1 if you have the `wctomb' function. */
#define HAVE_WCTOMB 1

/* Define to 1 if you have the <wctype.h> header file. */
#define HAVE_WCTYPE_H 1

/* Define to 1 if you have the <windows.h> header file. */
/* #undef HAVE_WINDOWS_H */

/* Define to 1 if you have the `wmemcmp' function. */
#define HAVE_WMEMCMP 1

/* Define to 1 if you have the `wmemcpy' function. */
#define HAVE_WMEMCPY 1

/* Define to 1 if you have the <zlib.h> header file. */
#define HAVE_ZLIB_H 1

/* Version number of libarchive as a single integer */
#define LIBARCHIVE_VERSION_NUMBER "2007900"

/* Version number of libarchive */
#define LIBARCHIVE_VERSION_STRING "2.7.900a"

/* Define to 1 if `lstat' dereferences a symlink specified with a trailing
   slash. */
/* #undef LSTAT_FOLLOWS_SLASHED_SYMLINK */

/* Define to 1 if `major', `minor', and `makedev' are declared in <mkdev.h>.
   */
/* #undef MAJOR_IN_MKDEV */

/* Define to 1 if `major', `minor', and `makedev' are declared in
   <sysmacros.h>. */
/* #undef MAJOR_IN_SYSMACROS */

/* Define to the generates final MD5 hash function. */
/* #undef MD5_Final */

/* Define to the initializes MD5 context function. */
/* #undef MD5_Init */

/* Define to the updates MD5 context function. */
/* #undef MD5_Update */

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* Define to the generates final RIPEMD160 hash function. */
/* #undef RIPEMD160_Final */

/* Define to the initializes RIPEMD160 context function. */
/* #undef RIPEMD160_Init */

/* Define to the updates RIPEMD160 context function. */
/* #undef RIPEMD160_Update */

/* Define to the generates final SHA1 hash function. */
/* #undef SHA1_Final */

/* Define to the initializes SHA1 context function. */
/* #undef SHA1_Init */

/* Define to the updates SHA1 context function. */
/* #undef SHA1_Update */

/* The size of `wchar_t', as computed by sizeof. */
#define SIZEOF_WCHAR_T 4

/* Define to 1 if strerror_r returns char *. */
/* #undef STRERROR_R_CHAR_P */

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Version number of package */
#define VERSION "2.7.900a"

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
/* #undef _LARGEFILE_SOURCE */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define for Solaris 2.5.1 so the uint64_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef were allowed, the
   #define below would cause a syntax error. */
/* #undef _UINT64_T */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef gid_t */

/* Define to `unsigned long' if <sys/types.h> does not define. */
/* #undef id_t */

/* Define to the type of a signed integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
/* #undef int64_t */

/* Define to the widest signed integer type if <stdint.h> and <inttypes.h> do
   not define. */
/* #undef intmax_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef dev_t */

/* Define to `long long' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef ssize_t */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef uid_t */

/* Define to `unsigned short' if <sys/types.h> doesn't define. */
/* #undef uint16_t */

/* Define to `unsigned int' if <sys/types.h> doesn't define. */
/* #undef uint32_t */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef int32_t */

/* Define to the type of an unsigned integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
/* #undef uint64_t */

/* Define to the widest unsigned integer type if <stdint.h> and <inttypes.h>
   do not define. */
/* #undef uintmax_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef intptr_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef uintptr_t */
